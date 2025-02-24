rm(list=ls())


# files not included in Repo
DATA_FILE_TO_USE <- "data/fit/Prelec_gam_LA_parameters.csv"

library(tidyverse)
library(vtable)



#==========================================#
# Preprocess Behavioral Results and quest. #
#==========================================#


dat <- read.csv(DATA_FILE_TO_USE)
questionnaire_data <- read.csv("data/processed/quest_dat.csv")
behave_data <- read.csv("data/processed/behave_dat.csv")
exp_data <- read.csv("data/processed/exp_dat.csv")
exp_data <- exp_data %>%  filter(row_number() %% 120 == 1)



dat_complete <- left_join(dat, questionnaire_data, by="participant_id")
dat_complete <- left_join(dat_complete, behave_data, by="participant_id")
dat_complete <- left_join(dat_complete, exp_data[,c("participant_id", "de_gap_rule")], by="participant_id")
dat <- dat_complete











#==========================================#
#             model fit                    #
#==========================================#

# description
dat$performance_de_g_level[1]

# experience
dat$performance_ex_g_level[1]







#==========================================#
# Tables about variable reported           #
#==========================================#



# General table (more variables then reported in the paper)
sumtable(dat[,c("sample_mean", "relative_switching", "optimal_ev_choice_desc", "optimal_ev_choice_samp",
                "minimax_choice_desc", "minimax_choice_samp", "minimax_se_desc",
                "minimax_se_samp", "minimax_treatment_desc", "minimax_treatment_samp")], add.median = T)




# tables reported in analysis

sumtable(dat[,c("sample_mean", "relative_switching")])

sumtable(dat[,c("Sex", "Age", "UIS_GESAMT", "DERS_GESAMT")])

dat$de_gap <- dat$gamma_de-dat$gamma_ex
sumtable(dat[,c("lambda_de", "lambda_ex", "gamma_de", "gamma_ex", "de_gap", "de_gap_rule")])
cor.test(dat$de_gap, dat$de_gap_rule)












#==========================================#
# BCIs on pop level                        #
#==========================================#


pop <- read.csv("data/fit/pop_level/Prelec_gam_LA_pop_level.csv")



quantile(pop$mu_lambda_de, probs=c(0.025, .5, 0.975))


quantile(pop$mu_lambda_ex, probs=c(0.025, .5, 0.975))



quantile(pop$mu_gamma_de, probs=c(0.025, .5, 0.975))


quantile(pop$mu_gamma_ex, probs=c(0.025, .5, 0.975))


quantile(pop$mu_gamma_de-pop$mu_gamma_ex, probs=c(0.025, .5, 0.975))


