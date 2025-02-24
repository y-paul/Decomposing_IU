library(rstan)
library(loo)
library(bayesplot)
library(tidyverse)

rm(list=ls())


# helper functions --------------------------------------------------------

# convergence diagnostics
source('fit/stan_estimation/functions/00_stan_diags.R')

# loo-accuracy
source('fit/stan_estimation/functions/binary_accuracy_loo.R')


# data set with  ID, can be used to reconstruct stan data
dat <- read.csv("data/processed/exp_dat.csv")


# for convenience - can be reconstructed from dat
d_stan <- readRDS("data/processed/d_stan.rds")



# data --------------------------------------------------------------------


d_stan[1:7] = lapply(d_stan[1:7], function(x) { 
  
  # r = as.matrix(x[sub,]); return(r) 
  r = as.matrix(x); return(r) 

  } )
# uncomment if you want to estimate exp based on objective probs
# d_stan$fa = d_stan$pa
# d_stan$fb = d_stan$pb


# d_stan$N = 10;
# d_stan$n = 600
# d_stan$sub = rep(1:10, each = 60)

# stan set up -------------------------------------------------------------

# sampler parameters
n_chains = 4
n_iter = 4e3
n_burn = 1e3
n_thin = 2
n_cores = n_chains

gp_t = c('mu_theta_de', 'mu_theta_ex', 'sig_theta_ex', 'sig_theta_de', 'theta_r')

gp_g1 = c('mu_gamma_de', 'mu_gamma_ex', 'sig_gamma_ex', 'sig_gamma_de', 'gam_r')

gp_l1 = c('mu_lambda_de', 'mu_lambda_ex', 'sig_lambda_ex', 'sig_lambda_de', 'lam_r')

ip_t = c('theta_de', "theta_ex")
ip_g1 = c('gamma_de', 'gamma_ex')
ip_l1 = c('lambda_de', 'lambda_ex')

l_pars = c("log_lik_de", 'log_lik_ex')

# stan models -------------------------------------------------------------

stan_mods = list.files('fit/stan_estimation/stan_mods',
                       full.names = T)
names(stan_mods) = gsub('^0[^_]*_|.stan', '', list.files('fit/stan_estimation/stan_mods'))

# estimations -------------------------------------------------------------

# list for storing the results
est_mods = list()

# loop over the models
for(i in names(stan_mods)) {

  # set up parameters for monitoring
  if(i == 'Lin_LA') { 
    # remove gammas from monitoring in the lin model
    gp_s = c(gp_t, gp_l1); ip_s = c(ip_t, ip_l1)
    
  } else if( grepl('gam_LA', i)) { 
    
    gp_s = c(gp_t, gp_g1, gp_l1); ip_s = c(ip_t, ip_g1, ip_l1)
    
  }
  
  # transalate and compile the model
  trans = stanc(file = stan_mods[[i]])
  compiled = stan_model(stanc_ret = trans, verbose = F)
  
  # get the posterior samples (estimation!!)
  stanfit = sampling(object = compiled,
                     data = d_stan,
                     pars = c( gp_s, ip_s, l_pars ), # parameters to monitor
                     init = '0',
                     chains = n_chains,
                     iter = n_iter,
                     warmup = n_burn,
                     thin = n_thin,
                     cores = n_chains,
                     control = NULL)
  
  # model performance -------------------------------------------------------
  
  perf = lapply(list(de = 1, ex = 2), function(ii) {
    
    ll = ifelse(ii == 1, 'log_lik_de', 'log_lik_ex')
    
    # approximate loo 
    looE = rstan::loo(stanfit,
                      pars = ll,
                      moment_match = F)
    
    # loo balanced accuracy
    ba = binary_accuracy_loo(stanfit,
                             parameter_name = ll,
                             d_stan$choices[,ii],
                             binary_cutoff = .5)
    
    # loo ind accuracy
    ind_ba_loo = ID_binary_accuracy_loo(stanfit,
                                        parameter_name = ll,
                                        y = d_stan$choices[,ii],
                                        N = d_stan$N, # number of participants 
                                        ncp = 60 # number of choices per participant & condition
                                        )
    
    return(list(looE = looE, ba = ba, ind_perf = ind_ba_loo, ll=ll))
    
  })
  

  
  # parameters --------------------------------------------------------------
  
  # print diagnostic plots
  try(stan_diag(stanFit = stanfit,
                n = d_stan$n,
                ind_p = ip_s,
                group_p = gp_s, #[!grepl('sigma', gp_s)],
                write_path = paste0('fit/stan_estimation/stan_diags/', i)
  ))
  
  # posterior samples
  p_pars = rstan::extract(stanfit)[c(gp_s)]
  i_pars = rstan::extract(stanfit)[c(ip_s)]
  
  # summary table
  fit_summary = summary(stanfit,
                        pars = c(gp_s, 'lp__'))[[1]]
  fit_summary = round(fit_summary, 3)
  
  # output ------------------------------------------------------------------
  
  # sampling info
  sampling_info = list(
    data = d_stan,
    pars = c(gp_s, ip_s),
    chains = n_chains,
    iter = n_iter,
    warmup = n_burn,
    thin = n_thin,
    control = NULL,
    model = compiled
  )
  
  # list with results
  est_mods[[i]] = list(pars = list(p_pars = p_pars, # posterior samples of pop-lvl pars
                                   i_pars = i_pars), # posterior samples of ind-lvl pars
                       fit_summary = fit_summary, # quick summary of pop-lvl pars
                       performance = perf, # predictive performance metrics
                       sampling_info = sampling_info # estimation info, including data
                       )
  
  # clean the env
  rm(stanfit, i_pars, p_pars, sampling_info, fit_summary, perf, trans, compiled) 
  
  print(i)

}



for (i in names(est_mods)) {
  stan_fit <- est_mods[[i]]
  performance_de <- stan_fit$performance$de$ba$ba
  performance_ex <- stan_fit$performance$ex$ba$ba
  stan_dat <- stan_fit$pars$i_pars
  
  tryCatch({
    pop_level <- stan_fit$pars$p_pars
    pop_level <- as.data.frame(pop_level)
    write.csv(pop_level, str_c("data/fit/pop_level/", i, "_pop_level.csv"))
  },
  error = function(cond) {
    print(i)
    print("Error saving pop-level")
    print(cond)
  },
  warning=function(cond){print(cond)})
  
  

  
  
  
  
  stan_dat <- as.data.frame( sapply(stan_dat, function(x) apply(x, 2, median) ) )
  stan_dat$id <- unique(dat$participant_id)
  stan_dat$participant_id <- unique(dat$participant_id)
  stan_dat$performance_de_g_level <- rep(performance_de, times=length(stan_dat$id))
  stan_dat$performance_ex_g_level <- rep(performance_ex, times=length(stan_dat$id))
  
  write.csv(stan_dat, str_c("data/fit/", i, "_parameters.csv"))
}
