rm(list=ls())
library(tidyverse)
library(latex2exp)

RECALC_BAYES=F

DATA_FILE_TO_USE <- "data/fit/Prelec_gam_LA_parameters.csv"





dat <- read.csv(DATA_FILE_TO_USE)
questionnaire_data <- read.csv("data/processed/quest_dat.csv")
behave_data <- read.csv("data/processed/behave_dat.csv")
exp_data <- read.csv("data/processed/exp_dat.csv")
exp_data <- exp_data %>%  filter(row_number() %% 120 == 1)



# make a) main corr results of plot 5 and b) SI2 and SI3 





# first preprocess

prob_data <- read.csv("data/prediction_check/Prelec_gam_LA_probs.csv")
prob_data <- prob_data %>% group_by(participant_id, cond) %>% 
  summarize(
    chosen_optim_ev=mean(chosen_optim_ev, na.rm=T),
    chosen_optim_be_ev=mean(chosen_optim_be_ev, na.rm=T),
    chosen_optim_se_raw=mean(chosen_optim_se_raw, na.rm=T),
    chosen_optim_se_freq=mean(chosen_se_freq, na.rm=T)
  ) %>% pivot_wider(names_from =cond,
                    values_from=c(
                      chosen_optim_ev, chosen_optim_be_ev, chosen_optim_se_raw, chosen_optim_se_freq
                    ))

dat_complete <- left_join(dat, questionnaire_data, by="participant_id")
dat_complete <- left_join(dat_complete, behave_data, by="participant_id")
dat_complete <- left_join(dat_complete, exp_data[,c("participant_id", "de_gap_rule")], by="participant_id")
dat_complete <- left_join(dat_complete, prob_data, by="participant_id")

dat <- dat_complete


vars_of_int <- dat %>%
  mutate(de_gap = gamma_de-gamma_ex,
         lambda_de_gap = lambda_de-lambda_ex) %>% 
  select(UIS_GESAMT, UIS_A, UIS_B, UIS_C,
         DERS_GESAMT, gamma_de, gamma_ex,
         lambda_de, lambda_ex,
         theta_de, theta_ex, 
         de_gap, de_gap_rule, lambda_de_gap, sample_mean, relative_switching,
         chosen_optim_ev_desc,chosen_optim_ev_exp,
         chosen_optim_be_ev_desc, chosen_optim_be_ev_exp,
         chosen_optim_se_raw_desc, chosen_optim_se_raw_exp,
         chosen_optim_se_freq_desc, chosen_optim_se_freq_exp)



# this is going to take a long time if recalcing
if (RECALC_BAYES) {
  
  
  library(rngtools)
  library(progressr)
  library(doFuture)
  registerDoFuture()
  options(doFuture.rng.onMisuse = "ignore")
  plan(multisession, workers=30)
  
  
  # Initialize matrix to store correlations
  n <- ncol(vars_of_int)
  
  nsamples = 5e3
  
  
  range = 1:length(colnames(vars_of_int))
  results <- NA
  
  rng <- RNGseq(n * (n+1)/2, 1234 )
  
  with_progress({
    p <- progressor(along=1:(max(range) * max(range)))
    
    results <- foreach(i = range, .combine = rbind) %:%
      foreach(j = range,
              .combine = rbind,
              r = rng[(i - 1) * i/2 + 1:i]) %dopar% {
                p()
                rngtools::setRNG(r)
                
                # Scripts from doi: 10.1080/02664763.2019.1709053
                source('analysis/bayes_rho/spearmanSampler.R')# Spearman's rho function
                source('analysis/bayes_rho/rankBasedCommonFunctions.R')
                x <- vars_of_int[[i]]
                y <- vars_of_int[[j]]
                
                name_x <- colnames(vars_of_int)[i]
                name_y <- colnames(vars_of_int)[j]
                rhoSamples <- spearmanGibbsSampler(xVals = x,
                                                   yVals = y, 
                                                   nSamples = nsamples)
                
                bf <- computeBayesFactorOneZero(rhoSamples$rhoSamples, 
                                                whichTest = "Spearman",
                                                priorParameter = 1)
                
                
                return(c(cor = cor(x, y, method="spearman"), bf=bf, name_x=name_x, name_y = name_y))
              }
    
    
    
  })
  
  plan(sequential)
  
  
  
  
  
  
  # View the result
  
  fname <- paste0("data/correlations/all_cors_",substr(DATA_FILE_TO_USE, 10, nchar(DATA_FILE_TO_USE)))
  
  results <- as.data.frame(results)
  
  write.csv(results, fname)
  
  
  
} else {
  results <- read.csv(paste0("data/correlations/all_cors_",substr(DATA_FILE_TO_USE, 10, nchar(DATA_FILE_TO_USE))))
}

















cormat <- results %>% 
  rename(Var1 = name_x, Var2 = name_y, value=cor)


cormat_processed <- cormat %>%
  mutate(bf = as.numeric(bf),
         value = as.numeric(value)) %>% 
  mutate(bf = ifelse(bf == Inf, 900000, bf)) %>% 
  mutate(Var1 = recode(Var1,
                       UIS_GESAMT = "IUS Total Score",
                       UIS_A ="IUS Impaired Ability",
                       UIS_B = "IUS Distress",
                       UIS_C = "IUS Vigilance",
                       DERS_GESAMT = "DERS Total Score",
                       lambda_de = "Lambda De",
                       lambda_ex = "Lambda Ex",
                       gamma_de = "Gamma De",
                       gamma_ex = "Gamma Ex",
                       theta_de = "Theta De",
                       theta_ex = "Theta Ex",
                       de_gap = "DE gap",
                       lambda_de_gap = "Lambda DE gap",
                       de_gap_rule = "DE gap (discrete)",
                       sample_mean = "Median Samples",
                       relative_switching = "Relative Switching",
                       optimal_ev_choice_desc = "Choice Rule: Maximize EV De",
                       optimal_ev_choice_samp = "Choice Rule: Maximize EV Ex",
                       minimax_se_desc = "CR: Minimize weighted SE De",
                       minimax_se_samp = "CR: Minimize weighted SE Ex",
                       minimax_choice_desc = "CR: Maximize weighted BE De",
                       minimax_choice_samp = "CR: Maximize weighted BE Ex",
                       chosen_optim_ev_desc = "CR: Maximize EV De",
                       chosen_optim_ev_exp = "CR: Maximize EV Ex",
                       chosen_optim_be_ev_desc = "CR: Maximize Treat De",
                       chosen_optim_be_ev_exp = "CR: Maximize Treat Ex",
                       chosen_optim_se_raw_desc = "CR: Minimize SEI De",
                       chosen_optim_se_raw_exp = "CR: Minimize SEI Ex",
                       chosen_optim_se_freq_desc = "CR: Minimize SEP De",
                       chosen_optim_se_freq_exp = "CR: Minimize SEP Ex")) %>% 
  mutate(Var2 = recode(Var2,
                       UIS_GESAMT = "IUS Total Score",
                       UIS_A ="IUS Impaired Ability",
                       UIS_B = "IUS Distress",
                       UIS_C = "IUS Vigilance",
                       DERS_GESAMT = "DERS Total Score",
                       lambda_de = "Lambda De",
                       lambda_ex = "Lambda Ex",
                       gamma_de = "Gamma De",
                       gamma_ex = "Gamma Ex",
                       theta_de = "Theta De",
                       theta_ex = "Theta Ex",
                       lambda_de_gap = "Lambda DE gap",
                       sample_mean = "Median Samples",
                       relative_switching = "Relative Switching",
                       optimal_ev_choice_desc = "Choice Rule: Maximize EV De",
                       optimal_ev_choice_samp = "Choice Rule: Maximize EV Ex",
                       minimax_se_desc = "CR: Minimize weighted SE De",
                       minimax_se_samp = "CR: Minimize weighted SE Ex",
                       minimax_choice_desc = "CR: Maximize weighted BE De",
                       minimax_choice_samp = "CR: Maximize weighted BE Ex",
                       chosen_optim_ev_desc = "CR: Maximize EV De",
                       chosen_optim_ev_exp = "CR: Maximize EV Ex",
                       chosen_optim_be_ev_desc = "CR: Maximize Treat De",
                       chosen_optim_be_ev_exp = "CR: Maximize Treat Ex",
                       chosen_optim_se_raw_desc = "CR: Minimize SEI De",
                       chosen_optim_se_raw_exp = "CR: Minimize SEI Ex",
                       chosen_optim_se_freq_desc = "CR: Minimize SEP De",
                       chosen_optim_se_freq_exp = "CR: Minimize SEP Ex")) %>% 
  mutate(bf = ifelse(bf>99.99, ">99.99", sprintf("%.2f",round(bf,2)))) %>%
  mutate(label = str_c(sprintf("%.2f",round(value,2)), "\n(", bf, ")")) %>% 
  mutate(value = ifelse(Var1==Var2, NA, value)) %>% 
  mutate(label = ifelse(Var1==Var2, "", label ))  %>% 
  mutate(Var1 = factor(Var1, levels=unique(Var1)),
         Var2 = factor(Var2, levels=unique(Var2)))










# do fig. 5

for_main_paper_vars <- list(
  Var1 = c("IUS Total Score", "IUS Impaired Ability",
           "IUS Vigilance", "IUS Distress under IU", "DERS Total Score"),
  Var2 = c("Lambda De", "Lambda Ex", "Gamma De", "Gamma Ex",
           "DE gap", "DE gap (discrete)", "Lambda DE gap", "Median Samples", "Relative Switching")
)



matx <- cormat_processed %>% 
  filter(Var2 %in% for_main_paper_vars$Var1) %>% 
  filter(Var1 %in% for_main_paper_vars$Var2) %>% 
  mutate(Var1 = factor(as.character(Var1),
                       levels=c(
                         "Lambda De", "Lambda Ex", "Lambda DE gap",
                         "Gamma De", "Gamma Ex", "DE gap",
                         "Median Samples", "Relative Switching", "DE gap (discrete)"
                       ))) %>% 
  mutate(cat = recode(Var1,
                      `Lambda De`= "Loss Aversion",
                      `Lambda Ex`= "Loss Aversion",
                      `Lambda DE gap`= "Loss Aversion",
                      `Gamma De`= "Nonlinear Probability\nWeighting",
                      `Gamma Ex`= "Nonlinear Probability\nWeighting",
                      `DE gap`= "Nonlinear Probability\nWeighting",
                      `Median Samples`= "Behavior",
                      `Relative Switching`= "Behavior",
                      `DE gap (discrete)`= "Behavior")) %>% 
  mutate(cat = factor(as.character(cat), 
                      levels=c("Loss Aversion", "Nonlinear Probability\nWeighting", "Behavior"))) %>% 
  ggplot(aes(y=Var2, x=Var1, fill=value, label=label)) + 
  geom_tile() +
  geom_text(size=3) +
  facet_wrap(~cat, strip.position="top", drop=TRUE, dir="v", scales="free_x", nrow=1) +
  scale_fill_gradient2(low="blue", high="red", mid="lightgrey", 
                       midpoint=0, limit=c(-1,1), space="Lab", 
                       name="Spearman Correlation") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        text = element_text(size=12),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title.align = 0.5,
        legend.title = element_text(margin = margin(b = 5)),  # This adds spacing between title and scale
        legend.box.spacing = unit(0.2, "cm")) +               # This manages spacing to other elements
  labs(x="Model Parameters/Behavioral Indices", y="Questionnaire Score [Sum]") +
  scale_x_discrete(labels = c(
    "Lambda De" = TeX(r'(${\lambda_{D}}$)'),
    "Lambda Ex" = TeX(r'(${\lambda_{E}}$)'),
    "Gamma De"  = TeX(r'(${\gamma_{D}}$)'),
    "Gamma Ex"  = TeX(r'(${\gamma_{E}}$)'),
    "Lambda DE gap" = expression(lambda ~ "DE gap")
  )) +
  scale_y_discrete(labels = c(
    "DERS Total Score" = "DERS\nTotal Score",
    "IUS Vigilance" = "IUS\nVigilance",
    "IUS Impaired Ability" = "IUS\nImpaired Ability",
    "IUS Total Score" = "IUS\nTotal Score"
  )) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = unit(4, "cm"),
    barheight = unit(0.4, "cm"),
    title.theme = element_text(margin = margin(b = 5))
  )) +
  theme(
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
    text = element_text(size=12),
    legend.position = c(-0.2, -0.58),           # slightly below plot, left-aligned
    legend.justification = c(0, 0),          # anchor legend's top-left to the point
    legend.direction = "horizontal",
    legend.title.align = 0.5,
    legend.title = element_text(margin = margin(b = 5)),
    plot.margin = margin(10, 10, 30, 10)     # bottom margin enlarged to fit legend nicely
  )
matx


ggsave("main_mat.png",plot=matx, units="cm", width=16, height=12, dpi=200)






# do fig. SI2 and SI3
# only IU, DERS and Parameters








# only IU, DERS and Choice Rules


vars <- c("IUS Total Score", "IUS Distress under IU",
          "IUS Vigilance", "DERS Total Score",
          "CR: Maximize EV De",                     
          "CR: Maximize EV Ex",                     
          "CR: Maximize Treat De",      
          "CR: Maximize Treat Ex",      
          "CR: Minimize SEI De",  
          "CR: Minimize SEI Ex",  
          "CR: Minimize SEP De",
          "CR: Minimize SEP Ex")
mat1 <- cormat_processed %>% 
  
  filter(as.numeric(Var2) <= as.numeric(Var1)) %>% 
  filter((Var1 %in% vars)&(Var2 %in% vars)) %>% 
  mutate(
    Var1 = recode(Var1, 
                  "CR: Maximize EV De" = "Maximize Expected Value De",
                  "CR: Maximize EV Ex" = "Maximize Expected Value Ex",
                  "CR: Maximize Treat De" = "Maximize Treatment De",
                  "CR: Maximize Treat Ex" = "Maximize Treatment Ex",
                  "CR: Minimize SEI De" = "Minimize Side Effect De",
                  "CR: Minimize SEI Ex" = "Minimize Side Effect Ex",
                  "CR: Minimize SEP De" = "Minimize Side Effect Prob. De",
                  "CR: Minimize SEP Ex" = "Minimize Side Effect Prob. Ex"
    ),
    Var2 = recode(Var2, 
                  "CR: Maximize EV De" = "Maximize Expected Value De",
                  "CR: Maximize EV Ex" = "Maximize Expected Value Ex",
                  "CR: Maximize Treat De" = "Maximize Treatment De",
                  "CR: Maximize Treat Ex" = "Maximize Treatment Ex",
                  "CR: Minimize SEI De" = "Minimize Side Effect De",
                  "CR: Minimize SEI Ex" = "Minimize Side Effect Ex",
                  "CR: Minimize SEP De" = "Minimize Side Effect Prob. De",
                  "CR: Minimize SEP Ex" = "Minimize Side Effect Prob. Ex"
    )
  ) %>% 
  ggplot(aes(x=Var1, y = Var2, fill=value, label=label)) + 
  geom_tile() +
  geom_text(size=1.9) +
  theme_bw() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "lightgrey", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  theme(text = element_text(size=10)) + 
  labs(x="Variable 1", y = "Variable 2") +
  theme(legend.position = "bottom") 

ggsave("mat_cr_quest.png",plot=mat2, units="cm", width=16, height=16, dpi=300)





# only parameters and Choice Rules


vars <- c("Lambda De", "Lambda Ex", "Gamma De","Gamma Ex",
          "Theta De", "Theta Ex", "DE Gap", "DE Gap Lambda",
          "CR: Maximize EV De",                     
          "CR: Maximize EV Ex",                     
          "CR: Maximize Treat De",      
          "CR: Maximize Treat Ex",      
          "CR: Minimize SEI De",  
          "CR: Minimize SEI Ex",  
          "CR: Minimize SEP De",
          "CR: Minimize SEP Ex")
mat2 <- cormat_processed %>% 
  filter(as.numeric(Var2) <= as.numeric(Var1)) %>% 
  filter((Var1 %in% vars)&(Var2 %in% vars)) %>% 
  mutate(
    Var1 = recode(Var1, 
                  "CR: Maximize EV De" = "Maximize Expected Value De",
                  "CR: Maximize EV Ex" = "Maximize Expected Value Ex",
                  "CR: Maximize Treat De" = "Maximize Treatment De",
                  "CR: Maximize Treat Ex" = "Maximize Treatment Ex",
                  "CR: Minimize SEI De" = "Minimize Side Effect De",
                  "CR: Minimize SEI Ex" = "Minimize Side Effect Ex",
                  "CR: Minimize SEP De" = "Minimize Side Effect Prob. De",
                  "CR: Minimize SEP Ex" = "Minimize Side Effect Prob. Ex"
    ),
    Var2 = recode(Var2, 
                  "CR: Maximize EV De" = "Maximize Expected Value De",
                  "CR: Maximize EV Ex" = "Maximize Expected Value Ex",
                  "CR: Maximize Treat De" = "Maximize Treatment De",
                  "CR: Maximize Treat Ex" = "Maximize Treatment Ex",
                  "CR: Minimize SEI De" = "Minimize Side Effect De",
                  "CR: Minimize SEI Ex" = "Minimize Side Effect Ex",
                  "CR: Minimize SEP De" = "Minimize Side Effect Prob. De",
                  "CR: Minimize SEP Ex" = "Minimize Side Effect Prob. Ex"
    )
  ) %>% 
  ggplot(aes(x=Var1, y = Var2, fill=value, label=label)) + 
  geom_tile() +
  geom_text(size=1.9) +
  theme_bw() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "lightgrey", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  theme(text = element_text(size=10)) + 
  labs(x="Variable 1", y = "Variable 2") +
  theme(legend.position = "bottom")





ggsave("mat1.png", mat1, dpi=300, units="cm", width=16, height = 16)
ggsave("mat2.png", mat2, dpi=300, units="cm", width=16, height = 16)



