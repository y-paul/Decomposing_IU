library(rstan)

rm(list=ls())


# CPT functions -----------------------------------------------------------

source('fit/functions/CPT.R')

# data --------------------------------------------------------------------

# for convenience - can be reconstructed from dat
d_stan <- readRDS("data/processed/d_stan.rds")

XA = d_stan$xa; XA[,'a_se'] = -1 * XA[,'a_se']
XB = d_stan$xb; XB[,'b_se'] = -1 * XB[,'b_se']
PA = d_stan$pa; PB = d_stan$pb
FA = d_stan$fa; FB = d_stan$fb

sub = d_stan$sub; N = d_stan$N; n = d_stan$n

# modeling results --------------------------------------------------------

est_mods = readRDS('fit/stan_estimation/posteriors/est_mods_fin.rds')

# main model's id pars
ip = sapply(est_mods$Prelec_gam_LA$pars$i_pars, function(x) apply(x, 2, median))


# choice a probs ----------------------------------------------------------

pa_desc = c(); pa_exp = c()

for(i in 1:n) {
  
  s = sub[i]
  
  # simulate choices of pa
  pa_desc[i] = cpt_mod_pr(XA = XA[i,], XB = XB[i,],
                          PA = PA[i,], PB = PB[i,],
                          g = ip[s,'gamma_de'],
                          l = ip[s,'lambda_de'],
                          t = ip[s,'theta_de'],
                          out = 'pa')
  
  # simulate choices of pa
  pa_exp[i] = cpt_mod_pr(XA = XA[i,], XB = XB[i,],
                         PA = FA[i,], PB = FB[i,],
                         g = ip[s,'gamma_ex'],
                         l = ip[s,'lambda_ex'],
                         t = ip[s,'theta_ex'],
                         out = 'pa')
}

# clean up
rm(XA, XB, PA, PB, FA, FB, i, n, N, s, sub)

# simulate choices --------------------------------------------------------

# no of replications
L = 20
co_sim = replicate(L, apply(cbind(pa_desc, pa_exp), 1:2,
                            rbinom, n = 1, size = 1),
                   simplify = F)


# fit replicated data -----------------------------------------------------

# transalate and compile the model
trans = stanc(file = 'fit/stan_estimation/stan_mods/01_Prelec_gam_LA.stan')
compiled = stan_model(stanc_ret = trans, verbose = F)

#
i_pars = c('theta_de', "theta_ex", 'gamma_de', 'gamma_ex', 'lambda_de', 'lambda_ex')
p_pars = c('mu_theta_de', "mu_theta_ex", 'mu_gamma_de', 'mu_gamma_ex', 
           'mu_lambda_de', 'mu_lambda_ex')

# sampler parameters
n_chains = 8
n_iter = 2e3
n_burn = 1e3
n_thin = 4
n_cores = n_chains

#
REC_PARS = list()

# loop over the models
for(l in 1:L) {
  
  # set data
  d_stan$choices = co_sim[[l]]
  
  # get the posterior samples (estimation!!)
  stanfit = sampling(object = compiled,
                     data = d_stan,
                     pars = c(i_pars, p_pars),
                     init = '0',
                     chains = n_chains,
                     iter = n_iter,
                     warmup = n_burn,
                     thin = n_thin,
                     cores = n_chains,
                     control = NULL)
  
  
  # posterior samples
  p_pars_post = extract(stanfit)[p_pars]
  i_pars_post = extract(stanfit)[i_pars]
  
  # list with results
  REC_PARS[[l]] = list(p_pars = p_pars_post,
                       i_pars = i_pars_post)
  
  rm(p_pars_post, i_pars_post)
  
  print(l)
  
}

saveRDS(REC_PARS, file = 'fit/stan_estimation/posteriors/parameter_recovery.rds')

# results -----------------------------------------------------------------

REC_PARS = readRDS('fit/stan_estimation/posteriors/parameter_recovery.rds')

# get medians of recovered posterior medians
rec_pars_me = sapply(REC_PARS, function(x) {
  
  xx = sapply(x$i_pars, function(y) apply(y, 2, median))
  
}, simplify = 'array')
#
rec_pars_me = apply(rec_pars_me, c(1, 2), median)

# put generating and recovered parameters together
dx = lapply(list(ip = ip, rec = rec_pars_me), function(x) {
  
  x = data.frame(x)
  x_desc = cbind(x[,c(1,3,5)], cond = 'desc'); colnames(x_desc)[1:3] = c('theta', 'gamma', 'lambda')
  x_exp = cbind(x[,c(2,4,6)], cond = 'exp'); colnames(x_exp)[1:3] = c('theta', 'gamma', 'lambda')
  
  return(rbind(x_desc, x_exp))
  
})
colnames(dx$ip)[1:3] = paste0(colnames(dx$ip)[1:3], '_gen')
colnames(dx$rec)[1:3] = paste0(colnames(dx$rec)[1:3], '_rec')
dx = cbind(dx$ip[,-4], dx$rec)

write.csv2(dx, 'fit/parameter_recovery_medians.csv',
           row.names = F)

# plots -------------------------------------------------------------------

dx = read.csv2('fit/parameter_recovery_medians.csv')

#
library(ggplot2)
library(patchwork)

par_rec_plts = lapply(c('theta', 'gamma', 'lambda'), function(p) {
  
  xy = data.frame(x = dx[,paste0(p, '_gen')],
                  y = dx[,paste0(p, '_rec')],
                  cond = dx$cond)
  
  lims = c(0, max(xy[,1:2]))
  
  # titles 
  if(p == 'gamma') tt = '\u03B3'
  if(p == 'lambda') tt = '\u03bb'
  if(p == 'theta') tt = '\u03B8'
  
  plt = ggplot(xy, mapping = aes(x = x, 
                                 y = y,
                                 color = cond)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, lty = 1) +
    geom_vline(xintercept = 1, lty = 3) +
    geom_hline(yintercept = 1, lty = 3) +
    scale_color_manual('Condition', 
                       values = c(desc = rgb(.1, .1, .8, .5),
                                  exp = rgb(.8, .1, .1, .5)),
                       labels = c('Description', 'Experience')) +
    xlab('generating') +
    ylab('recovered') +
    ylim(lims) +
    xlim(lims) +
    ggtitle(tt) +
    theme_bw() +
    theme(plot.title = element_text(hjust = .5))
  
  if(p == 'lambda') plt = plt + 
    scale_y_log10(limits = c(min(xy[,1:2]), max(xy[,1:2]))) +
    scale_x_log10(limits = c(min(xy[,1:2]), max(xy[,1:2])))
  
  return(plt)
  
})

wrap_plots(par_rec_plts, nrow = 1) + 
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

cor(dx$gamma_gen[dx$cond=='desc'], dx$gamma_rec[dx$cond=='desc'])
cor(dx$gamma_gen[dx$cond=='exp'], dx$gamma_rec[dx$cond=='exp'])
cor(dx$lambda_gen[dx$cond=='desc'], dx$lambda_rec[dx$cond=='desc'])
cor(dx$lambda_gen[dx$cond=='exp'], dx$lambda_rec[dx$cond=='exp'])

