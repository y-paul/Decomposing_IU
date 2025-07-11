



rm(list=ls())

DATA_FILE_TO_USE <- "data/prediction_check/Prelec_gam_la_probs.csv"

library(tidyverse)
library(patchwork)
dat <- read.csv(DATA_FILE_TO_USE)




# code for plot 3 in manuscript.

# first do posterior predictive checks in 3a:


# optim EV
# Description
exp_ev_max_1 <- dat %>% filter(cond=="desc") %>% 
  filter(!is.na(chosen_optim_ev)) %>% group_by(participant_id) %>% 
  summarize(m_behave = mean(chosen_optim_ev),
            sem_behave = sd(chosen_optim_ev)/sqrt(length(chosen_optim_ev)),
            m_model = mean(modprob_optim_ev),
            sem_model = sd(modprob_optim_ev)/sqrt(length(modprob_optim_ev))) %>% 
  arrange(m_behave) %>% 
  mutate(participant_id = as.factor(1:length(participant_id))) %>% 
  pivot_longer(cols=c(m_behave, m_model)) %>% 
  mutate(name = recode(name, m_behave = "Behavioral Proportions",
                       m_model ="Model Predictions")) %>% 
  mutate(upper = ifelse(name=="Behavioral Proportions", value + sem_behave, value + sem_model),
         lower = ifelse(name=="Behavioral Proportions", value - sem_behave, value - sem_model)) %>% 
  ggplot(aes(x = participant_id, color=name, y = value, ymax = upper, ymin = lower)) +
  geom_hline(yintercept=.5)+
  geom_point(size = 1, alpha=.5) +
  theme_classic() + ylim(0, 1) +
  labs(subtitle="Maximize\nExpected Value",
       x = "",
       y = "Proportion of Choices", color="") +
  scale_color_manual(values=c("black", "darkblue")) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size=12)) +
  guides(color="none")



# Experie
exp_ev_max_2 <- dat %>% filter(cond=="exp") %>% group_by(participant_id) %>%  
  filter(!is.na(chosen_optim_ev)) %>% 
  summarize(m_behave = mean(chosen_optim_ev),
            sem_behave = sd(chosen_optim_ev)/sqrt(length(chosen_optim_ev)),
            m_model = mean(modprob_optim_ev),
            sem_model = sd(modprob_optim_ev)/sqrt(length(modprob_optim_ev))) %>% 
  arrange(m_behave) %>% 
  mutate(participant_id = as.factor(1:length(participant_id))) %>% 
  pivot_longer(cols=c(m_behave, m_model)) %>% 
  mutate(name = recode(name, m_behave = "Behavioral Proportions",
                       m_model ="Model Predictions")) %>% 
  mutate(upper = ifelse(name=="Behavioral Proportions", value + sem_behave, value + sem_model),
         lower = ifelse(name=="Behavioral Proportions", value - sem_behave, value - sem_model)) %>% 
  ggplot(aes(x = participant_id, color=name, y = value, ymax = upper, ymin = lower)) +
  geom_hline(yintercept=.5)+
  geom_point(size = 1, alpha=.5) +
  theme_classic() + ylim(0, 1) +
  labs(subtitle="",
       x = "Participant",
       y = "Proportions\nof Choices", color="") +
  scale_color_manual(values=c("black", "darkred")) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position="bottom",
        text = element_text(size=12)) +
  guides(color="none")






# SEP







# optim EV



# Description
exp_se_freq1 <- dat %>% filter(cond=="desc") %>%
  filter(!is.na(chosen_se_freq)) %>% group_by(participant_id) %>% 
  summarize(m_behave = mean(chosen_se_freq),
            sem_behave = sd(chosen_se_freq)/sqrt(length(chosen_se_freq)),
            m_model = mean(modpob_se_freq),
            sem_model = sd(modpob_se_freq)/sqrt(length(modpob_se_freq))) %>% 
  arrange(m_behave) %>% 
  mutate(participant_id = as.factor(1:length(participant_id))) %>% 
  pivot_longer(cols=c(m_behave, m_model)) %>% 
  mutate(name = recode(name, m_behave = "Behavioral Proportions",
                       m_model ="Model Predictions")) %>% 
  mutate(upper = ifelse(name=="Behavioral Proportions", value + sem_behave, value + sem_model),
         lower = ifelse(name=="Behavioral Proportions", value - sem_behave, value - sem_model)) %>% 
  ggplot(aes(x = participant_id, color=name, y = value, ymax = upper, ymin = lower)) +
  geom_hline(yintercept=.5)+
  geom_point(size = 1, alpha=.5) +
  theme_classic() + ylim(0, 1) +
  labs(subtitle="Minimize\nSide Effect Prob.",
       x = "",
       y = "", color="") +
  scale_color_manual(values=c("black", "darkblue")) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=12)) +
  guides(color="none")



# Experience
exp_se_freq2 <- dat %>% filter(cond=="exp") %>%
  filter(!is.na(chosen_se_freq)) %>% 
  group_by(participant_id) %>% 
  summarize(m_behave = mean(chosen_se_freq),
            sem_behave = sd(chosen_se_freq)/sqrt(length(chosen_se_freq)),
            m_model = mean(modpob_se_freq),
            sem_model = sd(modpob_se_freq)/sqrt(length(modpob_se_freq))) %>% 
  arrange(m_behave) %>% 
  mutate(participant_id = as.factor(1:length(participant_id))) %>% 
  pivot_longer(cols=c(m_behave, m_model)) %>% 
  mutate(name = recode(name, m_behave = "Behavioral Proportions",
                       m_model ="Model Predictions")) %>% 
  mutate(upper = ifelse(name=="Behavioral Proportions", value + sem_behave, value + sem_model),
         lower = ifelse(name=="Behavioral Proportions", value - sem_behave, value - sem_model)) %>% 
  ggplot(aes(x = participant_id, color=name, y = value, ymax = upper, ymin = lower)) +
  geom_hline(yintercept=.5)+
  geom_point(size = 1, alpha=.5) +
  theme_classic() + ylim(0, 1) +
  labs(subtitle="",
       x = "Participant",
       y = "Proportions of Choices", color="") +
  scale_color_manual(values=c("black", "darkred")) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="bottom",
        text = element_text(size=12))+
  guides(color="none")





exp_se_freq1/
  exp_se_freq2








# optim EB




# Description
exp_ev_be_max_1 <- dat %>% 
  filter(!is.na(chosen_optim_be_ev)) %>% 
  filter(cond=="desc")  %>% group_by(participant_id) %>% 
  summarize(m_behave = mean(chosen_optim_be_ev),
            sem_behave = sd(chosen_optim_be_ev)/sqrt(length(chosen_optim_be_ev)),
            m_model = mean(modprob_optim_be_ev),
            sem_model = sd(modprob_optim_be_ev)/sqrt(length(modprob_optim_be_ev))) %>% 
  arrange(m_behave) %>% 
  mutate(participant_id = as.factor(1:length(participant_id))) %>% 
  pivot_longer(cols=c(m_behave, m_model)) %>% 
  mutate(name = recode(name, m_behave = "Behavioral Proportions",
                       m_model ="Model Predictions")) %>% 
  mutate(upper = ifelse(name=="Behavioral Proportions", value + sem_behave, value + sem_model),
         lower = ifelse(name=="Behavioral Proportions", value - sem_behave, value - sem_model)) %>% 
  ggplot(aes(x = participant_id, color=name, y = value, ymax = upper, ymin = lower)) +
  geom_hline(yintercept=.5)+
  geom_point(size = 1, alpha=.5) +
  theme_classic() + ylim(0, 1) +
  labs(subtitle="Maximize\nTreatment",
       x = "",
       y = "", color="") +
  scale_color_manual(values=c("black", "darkblue")) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=12)) +
  guides(color="none")



# Experience
exp_ev_be_max_2 <- dat %>% 
  filter(!is.na(chosen_optim_be_ev)) %>% 
  filter(cond=="exp") %>% group_by(participant_id) %>% 
  summarize(m_behave = mean(chosen_optim_be_ev),
            sem_behave = sd(chosen_optim_be_ev)/sqrt(length(chosen_optim_be_ev)),
            m_model = mean(modprob_optim_be_ev),
            sem_model = sd(modprob_optim_be_ev)/sqrt(length(modprob_optim_be_ev))) %>% 
  arrange(m_behave) %>% 
  mutate(participant_id = as.factor(1:length(participant_id))) %>% 
  pivot_longer(cols=c(m_behave, m_model)) %>% 
  mutate(name = recode(name, m_behave = "Behavioral Proportions",
                       m_model ="Model Predictions")) %>% 
  mutate(upper = ifelse(name=="Behavioral Proportions", value + sem_behave, value + sem_model),
         lower = ifelse(name=="Behavioral Proportions", value - sem_behave, value - sem_model)) %>% 
  ggplot(aes(x = participant_id, color=name, y = value, ymax = upper, ymin = lower)) +
  geom_hline(yintercept=.5)+
  geom_point(size = 1, alpha=.5) +
  theme_classic() + ylim(0, 1) +
  labs(subtitle="",
       x = "Participant",
       y = "", color="") +
  scale_color_manual(values=c("black", "darkred")) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="bottom",
        text = element_text(size=12))+
  guides(color="none")




exp_ev_be_max_1/
  exp_ev_be_max_2






#  Minimize Worst Side Effect



# Description
exp_se_raw_1 <- dat %>% 
  filter(!is.na(chosen_optim_se_raw)) %>% 
  filter(cond=="desc") %>% group_by(participant_id) %>% 
  summarize(m_behave = mean(chosen_optim_se_raw),
            sem_behave = sd(chosen_optim_se_raw)/sqrt(length(chosen_optim_se_raw)),
            m_model = mean(modprob_optim_se_raw),
            sem_model = sd(modprob_optim_se_raw)/sqrt(length(modprob_optim_se_raw))) %>% 
  arrange(m_behave) %>% 
  mutate(participant_id = as.factor(1:length(participant_id))) %>% 
  pivot_longer(cols=c(m_behave, m_model)) %>% 
  mutate(name = recode(name, m_behave = "Behavioral Proportions",
                       m_model ="Model Predictions")) %>% 
  mutate(upper = ifelse(name=="Behavioral Proportions", value + sem_behave, value + sem_model),
         lower = ifelse(name=="Behavioral Proportions", value - sem_behave, value - sem_model)) %>% 
  ggplot(aes(x = participant_id, color=name, y = value, ymax = upper, ymin = lower)) +
  geom_hline(yintercept=.5)+
  geom_point(size = 1, alpha=.5) +
  theme_classic() + ylim(0, 1) +
  labs(subtitle="Minimize\nSide Effect",
       x = "",
       y = "", color="") +
  scale_color_manual(values=c("black", "darkblue")) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=12))+
  guides(color="none")



# Experience
exp_se_raw_2 <- dat %>%
  filter(!is.na(chosen_optim_se_raw)) %>% 
  filter(cond=="exp") %>% group_by(participant_id) %>% 
  summarize(m_behave = mean(chosen_optim_se_raw),
            sem_behave = sd(chosen_optim_se_raw)/sqrt(length(chosen_optim_se_raw)),
            m_model = mean(modprob_optim_se_raw),
            sem_model = sd(modprob_optim_se_raw)/sqrt(length(modprob_optim_se_raw))) %>%
  arrange(m_behave) %>% 
  mutate(participant_id = as.factor(1:length(participant_id))) %>% 
  pivot_longer(cols=c(m_behave, m_model)) %>% 
  mutate(name = recode(name, m_behave = "Behavioral Proportions",
                       m_model ="Model Predictions")) %>% 
  mutate(upper = ifelse(name=="Behavioral Proportions", value + sem_behave, value + sem_model),
         lower = ifelse(name=="Behavioral Proportions", value - sem_behave, value - sem_model)) %>% 
  ggplot(aes(x = participant_id, color=name, y = value, ymax = upper, ymin = lower)) +
  geom_hline(yintercept=.5)+
  geom_point(size = 1, alpha=.5) +
  theme_classic() + ylim(0, 1) +
  labs(subtitle="",
       x = "Participant",
       y = "", color="") +
  scale_color_manual(values=c("black", "darkred")) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="bottom",
        text = element_text(size=12))+
  guides(color="none")





exp_se_raw_1/
  exp_se_raw_2










# do loo acc, 3b
fit_dat <- read.csv("data/fit/Prelec_gam_LA_parameters.csv")
plot_data <- fit_dat %>% select(iperf_de, iperf_ex)


ml1 = mean(plot_data$iperf_de)
legend_data <- data.frame(x = c(1, 1), y = c(99, 99), label = c("Description", "Experience"))

loo1 <- plot_data %>%
  arrange(iperf_de) %>%
  mutate(id = 1:length(iperf_de)) %>%
  ggplot(aes(x = id, y = iperf_de)) +
  geom_hline(yintercept = 0.5, color = "black") +
  geom_hline(yintercept = ml1, color = "grey", linewidth = 1.4, alpha = .5) +
  geom_point(aes(color = "Description"), size = 1, alpha = .5) +  # simulate color mapping
  geom_point(data = legend_data, aes(x = x, y = y, color = label)) +  # adds legend entries
  ylim(c(0, 1)) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 12, color = "black"),
        plot.tag = element_text(size = 14, face = "bold")) +
  labs(x = "Participant",
       y = "LOO Accuracy",
       subtitle = "Approximate out of\nsample choice accuracy",
       tag = "B)") +
  scale_color_manual(name = "Condition", values = c("Description" = "darkblue", "Experience" = "darkred")) +
  theme(legend.position = "bottom") +  # move legend to bottom
  guides(color = guide_legend(override.aes = list(size = 4)))  +# enlarge legend points
  theme(legend.justification = c(0.3, 0))

loo1




ml2 = mean(plot_data$iperf_ex)

loo2 <- plot_data %>% 
  arrange(iperf_ex) %>% 
  mutate(id = 1:length(iperf_ex)) %>% 
  ggplot(aes(x = id, y = iperf_ex)) + 
  geom_hline(yintercept=0.5, color="black")+
  geom_hline(yintercept=ml2, color="grey", linewidth=1.4, alpha=.5) +
  geom_point( size = 1, alpha=.5,  color="darkred") + 
  ylim(c(0,1)) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),) + 
  theme(text=element_text(size=12, color="black")) +
  labs(x="Participant", y = "") 
loo2






# do parameter recovery, 3c
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
    geom_point(size = 1, alpha=.5) +
    geom_abline(slope = 1, intercept = 0, lty = 1) +
    geom_vline(xintercept = 1, lty = 3) +
    geom_hline(yintercept = 1, lty = 3) +
    scale_color_manual('Condition', 
                       values = c(desc = "darkblue",
                                  exp = "darkred"),
                       labels = c('Description', 'Experience')) +
    xlab('Generated') +
    ylab('Recovered') +
    ylim(lims) +
    xlim(lims) +
    labs(subtitle=tt) +
    theme_bw() +
    theme(plot.subtitle = element_text(hjust = .5))
  
  if(p == 'lambda') plt = plt + 
    scale_y_log10(limits = c(min(xy[,1:2]), max(xy[,1:2]))) +
    scale_x_log10(limits = c(min(xy[,1:2]), max(xy[,1:2])))
  
  return(plt)
  
})



gamma_plot_recovery <- par_rec_plts[[2]] + guides(color="none") +
  theme(text=element_text(size=12, color="black")) +
  labs(y = "") +
  theme(axis.title.y=element_blank()) +
  scale_x_continuous(breaks=c(0, 1, 2)) + 
  scale_y_continuous(breaks=c(0, 1, 2), limits=c(0,2)) 


lambda_plot_recovery <- par_rec_plts[[3]]+ guides(color="none") +
  theme(text=element_text(size=12, color="black"), plot.tag = element_text(size = 14, face = "bold")) +
  labs(tag = "C)") 










 # the layout is a bit complex to fit everything



negative_height <- -0.45
just_label <- 1
expand_fc = 2


maximize_expected_value <- wrap_plots(
  exp_ev_max_1 + scale_x_discrete(expand=expansion(add = expand_fc)) +
    labs(tag = "A)") +
    theme(
      plot.tag = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(margin = margin(r = 8)),
      plot.margin = margin(2, 5, 1, 5)
    ) + theme(axis.title.y = element_text(hjust =just_label)), plot_spacer(),
  exp_ev_max_2 +scale_x_discrete(expand=expansion(add = expand_fc))+
    labs(y = NULL) +
    theme(
      plot.margin = margin(1, 5, 2, 5)
    ),
  ncol = 1,
  heights = c(1,negative_height, 1)
)

minimize_side_effect <- ((exp_se_raw_1 +scale_x_discrete(expand=expansion(add = expand_fc)) +
                            theme(plot.margin = margin(5, 5, 0, 5))) / plot_spacer() /
                           (exp_se_raw_2 +scale_x_discrete(expand=expansion(add = expand_fc)) +
                              theme(plot.margin = margin(0, 5, 0, 5))) ) + plot_layout(heights=c(1,negative_height, 1))

minimize_side_effect_p <- ((exp_se_freq1 +scale_x_discrete(expand=expansion(add = expand_fc)) +
                              theme(plot.margin = margin(5, 5, 0, 5))) /
                             plot_spacer() /
                             (exp_se_freq2 +scale_x_discrete(expand=expansion(add = expand_fc)) +
                                theme(plot.margin = margin(0, 5, 0, 5))) ) + plot_layout(heights=c(1,negative_height, 1))


maximize_treatment <-( (exp_ev_be_max_1 +scale_x_discrete(expand=expansion(add = expand_fc)) +
                          theme(plot.margin = margin(5, 5, 0, 5))) / plot_spacer() /
                         (exp_ev_be_max_2 +scale_x_discrete(expand=expansion(add = expand_fc)) +
                            theme(plot.margin = margin(0, 5, 0, 5))) ) + plot_layout(heights=c(1,negative_height, 1))

# Make the final layout
top_row <- (maximize_expected_value | 
              minimize_side_effect |
              minimize_side_effect_p |
              maximize_treatment) +
  plot_layout(ncol = 4, widths = c(1, 1, 1, 1)) & 
  theme(plot.margin = margin(2, 5, 2, 5))  # tight margins between plots

bottom_row <- loo1 | loo2 | lambda_plot_recovery | gamma_plot_recovery

# Combine with relative heights
main_plot <- top_row / bottom_row + 
  plot_layout(heights = c(2, 1)) +  # 2:1 height ratio
  plot_annotation(theme = theme(plot.margin = margin(2, 2, 2, 2)))  # tighten spacing

main_plot




ggsave("plot3.png",plot=main_plot, units="cm", width=16, height=12, dpi=300)




