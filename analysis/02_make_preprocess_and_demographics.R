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



sumtable(dat[,c("UIS_GESAMT", "UIS_A", "UIS_B", "UIS_C", "DERS_GESAMT")],
         digits=NA, add.median=T)








#==========================================#
# BCIs on pop level                        #
#==========================================#


pop <- read.csv("data/fit/pop_level/Prelec_gam_LA_pop_level.csv")



quantile(pop$mu_lambda_de, probs=c(0.025, .5, 0.975))


quantile(pop$mu_lambda_ex, probs=c(0.025, .5, 0.975))



quantile(pop$mu_gamma_de, probs=c(0.025, .5, 0.975))


quantile(pop$mu_gamma_ex, probs=c(0.025, .5, 0.975))


quantile(pop$mu_gamma_de-pop$mu_gamma_ex, probs=c(0.025, .5, 0.975))
quantile(pop$mu_lambda_de-pop$mu_lambda_ex, probs=c(0.025, .5, 0.975))

quantile(pop$mu_theta_de, probs=c(0.025, .5, 0.975))
quantile(pop$mu_theta_ex, probs=c(0.025, .5, 0.975))


quantile(pop$theta_r, probs=c(0.025, .5, 0.975))
quantile(pop$gam_r, probs=c(0.025, .5, 0.975))
quantile(pop$lam_r, probs=c(0.025, .5, 0.975))



#==========================================#
# Supplement SE and Pains                  #
#==========================================#


behave_dat <- read.csv("data/processed/behave_dat.csv")
behave_dat %>% select(mean_ses,mild_pain, moderate_pain, severe_pain) %>% 
  pivot_longer(cols=all_of(c("mild_pain", "moderate_pain", "severe_pain", "mean_ses"))) %>% 
  mutate(name = recode(name,mean_ses = "Side Effects", mild_pain ="mild Pain", moderate_pain = "moderate Pain", severe_pain = "Severe Pain")) %>% 
  ggplot(aes(x = name, fill=name, y = value)) + 
  geom_boxplot(outlier.alpha=1) + 
  theme_bw() +
  labs(x = "Side Effect/Pain", y = "Mean Rating") +
  guides(fill="none")

behave_dat %>% select(Blähungen, Depression, Durchfall, Fieber, Gedächtnisverlust,Halluzinationen, Juckreiz, Schlaflosigkeit, Schwindel, Sprachstörungen) %>% 
  pivot_longer(all_of(c("Blähungen", "Depression", "Durchfall", "Fieber", "Gedächtnisverlust","Halluzinationen", "Juckreiz", "Schlaflosigkeit", "Schwindel", "Sprachstörungen"))) %>% 
  mutate(name = recode(name, Blähungen = "Flatulence",
                       Depression = "Depression",
                       Durchfall = "Diarrhea",
                       Fieber = "Fever",
                       Gedächtnisverlust ="Memory Loss",
                       Halluzinationen = "Hallucinations",
                       Juckreiz = "Itching",
                       Schlaflosigkeit = "Insomnia",
                       Schwindel = "Dizziness",
                       Sprachstörungen = "Speech Disorder")) %>% 
  mutate(name = fct_reorder(name,value)) %>% 
  ggplot(aes(x= name, y= value)) + 
  geom_boxplot(fill="white", color="black", notch=T) + theme_bw() +
  labs(x= "Side Effect", y = "Median Rating")




