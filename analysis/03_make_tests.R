rm(list=ls())

# file not included
DATA_FILE_TO_USE <- "data/fit/Prelec_gam_LA_parameters.csv"
RECALC_BAYES <- F # set true to run bayesian tests, might take a while

library(tidyverse)
library(BayesFactor)





# join data for analysis

dat <- read.csv(DATA_FILE_TO_USE)
questionnaire_data <- read.csv("data/processed/quest_dat.csv")
behave_data <- read.csv("data/processed/behave_dat.csv")
exp_data <- read.csv("data/processed/exp_dat.csv")
exp_data <- exp_data %>%  filter(row_number() %% 120 == 1)



dat_complete <- left_join(dat, questionnaire_data, by="participant_id")
dat_complete <- left_join(dat_complete, behave_data, by="participant_id")
dat_complete <- left_join(dat_complete, exp_data[,c("participant_id", "de_gap_rule")], by="participant_id")
dat <- dat_complete


dat_for_analysis <- dat












# ================================================== #
# Test Correlations
# ================================================== #


dat_for_analysis$gamma_de_gap <- dat_for_analysis$gamma_de - dat_for_analysis$gamma_ex


# Scripts from doi: 10.1080/02664763.2019.1709053
# not included in repo
source('analysis/bayes_rho/spearmanSampler.R')# Spearman's rho function
source('analysis/bayes_rho/rankBasedCommonFunctions.R')




do_bayes_data_rho <- function(dataf, label, nsamples=5e3) {
  
  d <- dataf
  bf_gde <- NA
  
  bf_gex <- NA
  
  bf_ggap <- NA
  
  cors <- c()
  if ("gamma_de" %in% colnames(d)  ) {
    
    
    
    rhoSamples <- spearmanGibbsSampler(xVals = d$gamma_de,
                                       yVals = d$UIS_GESAMT, 
                                       nSamples = nsamples)
    
    bf_gde <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                        whichTest = "Spearman",
                                        priorParameter = 1)
    
    
    
    rhoSamples <- spearmanGibbsSampler(xVals = d$gamma_ex,
                                       yVals = d$UIS_GESAMT, 
                                       nSamples = nsamples)
    bf_gex <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                        whichTest = "Spearman",
                                        priorParameter = 1)
    
    d$gamma_de_gap <- d$gamma_de - d$gamma_ex
    
    
    rhoSamples <- spearmanGibbsSampler(xVals = d$gamma_de_gap,
                                       yVals = d$UIS_GESAMT, 
                                       nSamples = nsamples)
    
    bf_ggap <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                         whichTest = "Spearman",
                                         priorParameter = 1)
    
    cors <- c(cors, cor(d$gamma_de, d$UIS_GESAMT, method="spearman"),
              cor(d$gamma_ex, d$UIS_GESAMT, method="spearman"),
              cor(d$gamma_de_gap, d$UIS_GESAMT, method="spearman"))
  }
  
  
  
  
  bf_lde <- NA
  
  bf_lex <- NA
  
  bf_lgap <- NA
  
  
  if ("lambda_de" %in% colnames(d)  ) {
    
    rhoSamples <- spearmanGibbsSampler(xVals = d$lambda_de,
                                       yVals = d$UIS_GESAMT, 
                                       nSamples = nsamples)
    bf_lde <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                        whichTest = "Spearman",
                                        priorParameter = 1)
    
    
    
    rhoSamples <- spearmanGibbsSampler(xVals = d$lambda_ex,
                                       yVals = d$UIS_GESAMT, 
                                       nSamples = nsamples)
    bf_lex <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                        whichTest = "Spearman",
                                        priorParameter = 1)
    
    
    
    d$lambda_de_gap <- d$lambda_de - d$lambda_ex
    
    rhoSamples <- spearmanGibbsSampler(xVals = d$lambda_ex,
                                       yVals = d$UIS_GESAMT, 
                                       nSamples = nsamples) 
    
    bf_lgap <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                         whichTest = "Spearman",
                                         priorParameter = 1)
    
    cors <- c(cors, cor(d$lambda_de, d$UIS_GESAMT, method="spearman"),
              cor(d$lambda_ex, d$UIS_GESAMT, method="spearman"),
              cor(d$lambda_de_gap, d$UIS_GESAMT, method="spearman"))
  }
  
  
  
  
  df_res <- data.frame(
    variable= c("gamma_de", "gamma_ex", "gamma_de_gap", "lambda_de", "lambda_ex", "lambda_de_gap"),
    bf = c(bf_gde, bf_gex, bf_ggap, bf_lde, bf_lex, bf_lgap),
    model = rep(label, times=6),
    cors=cors
    
    
  )
  
  return(df_res)
  
}





if (RECALC_BAYES) {
  res <- do_bayes_data_rho(dat_for_analysis, "analysis")
  write.csv(res, "data/res_bayes_rho.csv")
} else {
  res <- read.csv("data/res_bayes_rho.csv")
}


res





# ================================================== #
# Test Behavioral Results with UIS
# ================================================== #

# Scripts from doi: 10.1080/02664763.2019.1709053
# not included in repo
source('analysis/bayes_rho/spearmanSampler.R')# Spearman's rho function
source('analysis/bayes_rho/rankBasedCommonFunctions.R')



N_SAMPLES = 5e3

dat_for_analysis$de_gap <- dat_for_analysis$gamma_de-dat_for_analysis$gamma_ex



rhoSamples <- spearmanGibbsSampler(xVals = dat_for_analysis$relative_switching,
                                   yVals = dat_for_analysis$UIS_GESAMT, 
                                   nSamples = N_SAMPLES)

rho_switching_UIS <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                    whichTest = "Spearman",
                                    priorParameter = 1)

rhoSamples <- spearmanGibbsSampler(xVals = dat_for_analysis$sample_mean,
                                   yVals = dat_for_analysis$UIS_GESAMT, 
                                   nSamples = N_SAMPLES)

rho_msamples_UIS <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                     whichTest = "Spearman",
                                     priorParameter = 1)

rho_switching_UIS
cor.test(dat_for_analysis$relative_switching, dat_for_analysis$UIS_GESAMT, method="spearman")

rho_msamples_UIS
cor.test(dat_for_analysis$sample_mean, dat_for_analysis$UIS_GESAMT, method="spearman")





rhoSamples <- spearmanGibbsSampler(xVals = dat_for_analysis$de_gap,
                                   yVals = dat_for_analysis$sample_mean, 
                                   nSamples = N_SAMPLES)

gap_samples <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                               whichTest = "Spearman",
                                               priorParameter = 1)

gap_samples

cor.test(dat_for_analysis$sample_mean, dat_for_analysis$de_gap, method="spearman")










rhoSamples <- spearmanGibbsSampler(xVals = dat_for_analysis$de_gap_rule,
                                   yVals = dat_for_analysis$UIS_GESAMT, 
                                   nSamples = N_SAMPLES)

gap_rule_UIS <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                         whichTest = "Spearman",
                                         priorParameter = 1)

gap_rule_UIS

cor.test(dat_for_analysis$de_gap_rule, dat_for_analysis$UIS_GESAMT, method="spearman")



cor.test(dat_for_analysis$de_gap_rule,
         dat_for_analysis$sample_mean)








dat_for_analysis$lambda_gap <- dat_for_analysis$lambda_de - dat_for_analysis$lambda_ex
rhoSamples <- spearmanGibbsSampler(xVals = dat_for_analysis$lambda_gap,
                                   yVals = dat_for_analysis$UIS_GESAMT, 
                                   nSamples = N_SAMPLES)

lambda_dap_uis <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                          whichTest = "Spearman",
                                          priorParameter = 1)

lambda_dap_uis

cor.test(dat_for_analysis$lambda_gap, dat_for_analysis$UIS_GESAMT, method="spearman")

# ================================================== #
# Test UIS and DERS
# ================================================== #

# Scripts from doi: 10.1080/02664763.2019.1709053
# not included in repo
source('analysis/bayes_rho/spearmanSampler.R')# Spearman's rho function
source('analysis/bayes_rho/rankBasedCommonFunctions.R')



N_SAMPLES = 5e3

rhoSamples <- spearmanGibbsSampler(xVals = dat_for_analysis$DERS_GESAMT,
                                   yVals = dat_for_analysis$UIS_GESAMT, 
                                   nSamples = N_SAMPLES)

rho_switching_UIS <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                               whichTest = "Spearman",
                                               priorParameter = 1)
rho_switching_UIS
cor.test(dat_for_analysis$DERS_GESAMT, dat_for_analysis$UIS_GESAMT, method="spearman")



