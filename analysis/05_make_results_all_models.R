rm(list=ls())
library(tidyverse)



d1 <- read.csv("data/fit/pop_level/Prelec_gam_LA_pop_level.csv") # Model A
d2 <- read.csv("data/fit/pop_level/GE_gam_LA_pop_level.csv") # Model B
d3 <- read.csv("data/fit/pop_level/TK_gam_LA_pop_level.csv") # Model C
d4 <- read.csv("data/fit/pop_level/POW_gam_LA_pop_level.csv") # Model D


d <- rbind(d1, d2, d3, d4)


d$model <- rep(c("Prelec", "GE", "TK", "POW"), each=length(d1$X))
d <- d %>% mutate(de_gap = mu_gamma_de - mu_gamma_ex)



d_sum <- d %>% group_by(model) %>% 
  summarize(m_lambda_de = median(mu_lambda_de),
            bci_lower_lambda_de = quantile(mu_lambda_de, probs=c(0.025)),
            bci_upper_lambda_de = quantile(mu_lambda_de, probs=c(0.975)),
            m_gamma_de = median(mu_gamma_de),
            bci_lower_gamma_de = quantile(mu_gamma_de, probs=c(0.025)),
            bci_upper_gamma_de = quantile(mu_gamma_de, probs=c(0.975)),
            
            m_lambda_ex = median(mu_lambda_ex),
            bci_lower_lambda_ex = quantile(mu_lambda_ex, probs=c(0.025)),
            bci_upper_lambda_ex = quantile(mu_lambda_ex, probs=c(0.975)),
            m_gamma_ex = median(mu_gamma_ex),
            bci_lower_gamma_ex = quantile(mu_gamma_ex, probs=c(0.025)),
            bci_upper_gamma_ex = quantile(mu_gamma_ex, probs=c(0.975)),
            
            m_de_gap = median(de_gap),
            bci_lower_de_gap = quantile(de_gap, probs=c(0.025)),
            bci_upper_de_gap = quantile(de_gap, probs=c(0.975)),
            
            m_r_lam = median(lam_r),
            bci_lower_lam_r = quantile(lam_r, probs=c(0.025)),
            bci_upper_lam_r = quantile(lam_r, probs=c(0.975)),
            
            
            m_gam_r = median(gam_r),
            bci_lower_gam_r = quantile(gam_r, probs=c(0.025)),
            bci_upper_gam_r = quantile(gam_r, probs=c(0.975)),
            )








DATA_FILES_TO_USE <- c("data/fit/Prelec_gam_LA_parameters.csv",
                       "data/fit/GE_gam_LA_parameters.csv",
                       "data/fit/POW_gam_LA_parameters.csv",
                       "data/fit/TK_gam_LA_parameters.csv")



dfs <- list()
for (f in DATA_FILES_TO_USE) {

  dat <- read.csv(f)
  questionnaire_data <- read.csv("data/processed/quest_dat.csv")
  behave_data <- read.csv("data/processed/behave_dat.csv")
  exp_data <- read.csv("data/processed/exp_dat.csv")
  exp_data <- exp_data %>%  filter(row_number() %% 120 == 1)
  
  
  
  dat_complete <- left_join(dat, questionnaire_data, by="participant_id")
  dat_complete <- left_join(dat_complete, behave_data, by="participant_id")
  dat_complete <- left_join(dat_complete, exp_data[,c("participant_id", "de_gap_rule")], by="participant_id")
  dat <- dat_complete
  dfs[[length(dfs) + 1]] <- dat
}


names(dfs) <- c("Prelec", "GE", "POW", "TK")


source('analysis/bayes_rho/spearmanSampler.R')# Spearman's rho function
source('analysis/bayes_rho/rankBasedCommonFunctions.R')

source("analysis/all_model_fitters.R")


library(doParallel)
library(foreach)


n_cores <- detectCores()
cluster <- makeCluster(n_cores-4)
registerDoParallel(cluster)


results <- list()

results <- foreach(i=1:length(names(dfs))) %dopar% {
  source('analysis/bayes_rho/spearmanSampler.R')# Spearman's rho function
  source('analysis/bayes_rho/rankBasedCommonFunctions.R')
  
  source("analysis/all_model_fitters.R")
  
  
  nsamples= 5e3


    results[i] <- do_bayes_data_rho_gam_LA(dfs[[i]],
                                           label=i,
                                           nsamples=nsamples)
 
  
}





stopCluster(cl = cluster)


names(dfs)

for (f in 1:length(results)) {
  df <- results[[f]]
  namemod <- df$model[1]
  namemod <- substr(namemod, 1, nchar(namemod)-4)
  
  filename = str_c("data/correlations/corr_", namemod, ".csv")
  
  
  write.csv(df, filename)
  
  
}








# prelec
names(dfs)
results[[1]]


# GE
results[[2]]


# POWE
results[[3]]


# TK
results[[4]]












































