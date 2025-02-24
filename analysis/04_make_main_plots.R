rm(list=ls())
DATA_FILE_TO_USE <- "data/fit/Prelec_gam_LA_parameters.csv"


library(tidyverse)
library(cowplot)
library(BayesFactor)
library(latex2exp)





dat <- read.csv(DATA_FILE_TO_USE)
questionnaire_data <- read.csv("data/processed/quest_dat.csv")
behave_data <- read.csv("data/processed/behave_dat.csv")
exp_data <- read.csv("data/processed/exp_dat.csv")
exp_data <- exp_data %>%  filter(row_number() %% 120 == 1)



dat_complete <- left_join(dat, questionnaire_data, by="participant_id")
dat_complete <- left_join(dat_complete, behave_data, by="participant_id")
dat_complete <- left_join(dat_complete, exp_data[,c("participant_id", "de_gap_rule")], by="participant_id")
dat <- dat_complete















# ================================================== #
# make curve plots
# ================================================== #



# plot probability weighting first
df_function <- dat %>% select(participant_id, gamma_de, gamma_ex)


probability_weighting_function <- function(p, gamma) {
  return(  exp( - (  - log(p) )^gamma  )  )
}

x <- seq(from=0, to=1, by=0.001)
d <- data.frame(id=c(), para=c(), probs=c(), p=c())
for (i in 1:length(df_function$participant_id)) {
  id <- df_function$participant_id[i]
  para_de <- df_function$gamma_de[i]
  para_ex <- df_function$gamma_ex[i]
  
  
  
  probsde <- probability_weighting_function(x, para_de)
  probsex <- probability_weighting_function(x, para_ex)
  
  id <- rep(id, times=length(probsde))
  para <- rep(para_de, times=length(probsde))
  
  d <- rbind(d, data.frame(id, para, probsde, probsex , p=x))
}

pop_level <- read.csv("data/fit/pop_level/Prelec_gam_LA_pop_level.csv")

mu_gamma_de <- median(pop_level$mu_gamma_de)
mu_gamma_ex <- median(pop_level$mu_gamma_ex)
pop_level_gamma <- data.frame(
  probsde = probability_weighting_function(x, mu_gamma_de),
  probsex = probability_weighting_function(x, mu_gamma_ex),
  p = x
) %>% pivot_longer(cols=c(probsde, probsex)) %>% 
  mutate(name=recode(name, probsde="Description", probsex="Experience"))




plot_B_2_1 <- d %>%
  pivot_longer(cols = c(probsde, probsex)) %>%
  mutate(name = recode(name, probsde = "Description", probsex = "Experience")) %>%
  filter(name == "Description") %>% 
  ggplot(aes(x = p, y = value, group = id, color = name)) +
  geom_line(alpha = 0.05, size = 1.2, show.legend = FALSE) +
  facet_wrap(~name, ncol = 1) +
  theme_bw() +
  labs(
    x = "Objective Probability",
    y = "Subjective Decision Weight",
    title = "NPW"
  ) +
  theme(
    text = element_text(size = 10),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 10),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.title = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  scale_color_manual(values = c("darkblue", "darkred")) +
  geom_line(
    data = pop_level_gamma %>% filter(name == "Description"),
    aes(x = p, y = value, group = NULL),
    inherit.aes = FALSE, # Ensures global aesthetics don't interfere
    size = 1.2, color = "black", alpha=0.8 # Make it distinct
  )

plot_B_2_2 <- d %>%
  pivot_longer(cols = c(probsde, probsex)) %>%
  mutate(name = recode(name, probsde = "Description", probsex = "Experience")) %>%
  filter(name == "Experience") %>% 
  ggplot(aes(x = p, y = value, group = id, color = name)) +
  geom_line(alpha = 0.05, size = 1.2, show.legend = FALSE) +
  facet_wrap(~name, ncol = 1) +
  theme_bw() +
  labs(
    x = "Objective Probability",
    y = "Subjective Decision Weight") +
  theme(
    text = element_text(size = 10),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 10),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.title = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  scale_color_manual(values = c("darkred")) +
  geom_line(
    data = pop_level_gamma %>% filter(name == "Experience"),
    aes(x = p, y = value, group = NULL),
    inherit.aes = FALSE, # Ensures global aesthetics don't interfere
    size = 1.2, color = "black", alpha=0.8 # Make it distinct
  )

# plot description experience gap
m_de <- median(pop_level$mu_gamma_de)
m_ex <- median(pop_level$mu_gamma_ex)

y_de <- probability_weighting_function(x, m_de)
y_ex <- probability_weighting_function(x, m_ex)


df_degap <- data.frame(p=x, yde = y_de, yex=y_ex)
plot_B_3 <- df_degap %>% mutate(lower = yde, upper=yex) %>% 
  pivot_longer(cols=c(yde, yex)) %>% 
  mutate(name=recode(name, yde="Description", yex="Experience", title="DE Gap")) %>% 
  ggplot(aes(x=p, ymax=upper, ymin =lower, y=value, color=name)) + 
  geom_ribbon(alpha=.5, color="grey20") + 
  geom_line(size=1.5, alpha=.7, show.legend=T) + 
  theme_bw() + 
  theme(text=element_text(size=10),
        legend.position=c(0.65, 0.2)) + 
  theme(strip.background=element_rect(fill=NULL))+ 
  theme(strip.text=element_text(face="bold", size=10)) +
  theme(axis.text=element_text(angle=0, hjust=.5, size=10),
        axis.title=element_text(size=10),
        legend.box.background = element_rect(colour = NA,fill=NA, size=.5),
        legend.spacing.y = unit(0, "mm"),
        legend.spacing.x = unit(0, "mm"))  +
  theme(legend.title=element_blank(),
        legend.margin=margin(c(0.1,0.1,0.1,0.1)))+
  labs(x="Objective Probability",
       y=" Subjective Decision Weight",
       title="DE Gap")+
  scale_x_continuous(breaks=c(0,.5,1)) +
  scale_y_continuous(breaks=c(0,.5,1)) + 
  scale_color_manual(values=c("darkblue", "darkred")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme(legend.key=element_blank(),
        legend.background = element_rect(fill=alpha("white", 0)))
plot_B_3




# plot lambda functions 
df_function <- dat %>% select(participant_id, lambda_de, lambda_ex)

value_function <- function (v, lambda) {
  
  subj_v <- sapply(v, FUN= function(x) {
    if (x > 0) {
      subjective_value <- x
    } else if (x < 0) {
      subjective_value <- lambda * x
    } else {
      subjective_value <- 0
    }
    return(subjective_value)
  })
  
  return(subj_v)
  
}

x <- seq(from=-100, to=100, by=.005)
d <- data.frame(id=c(), vde=c(),vex=c(),para=c(), v=c())
for (i in 1:length(df_function$participant_id)) {
  id <- df_function$participant_id[i]
  para_de <- df_function$lambda_de[i]
  para_ex <- df_function$lambda_ex[i]
  
  
  
  vde <- value_function(x, para_de)
  vex <- value_function(x, para_ex)
  
  id <- rep(id, times=length(vde))
  para <- rep(para_de, times=length(vde))
  
  d <- rbind(d, data.frame(id, vde, vex, para, v=x))
}


m_lambda_de <- median(pop_level$mu_lambda_de)
m_lambda_ex <- median(pop_level$mu_lambda_ex)

pop_lambda <- data.frame(
  v = x,
  vde = value_function(x, m_lambda_de),
  vex = value_function(x, m_lambda_ex)
)%>% pivot_longer(cols=c(vde, vex)) %>% 
  mutate(name=recode(name, vde="Description", vex="Experience"))





plot_B_1_1 <- d %>% pivot_longer(cols=c(vde, vex)) %>% 
  mutate(name=recode(name, vde="Description", vex="Experience")) %>% 
  filter(name == "Description") %>% 
  ggplot(aes(x = v, y = value, group=id, color=name))+
  geom_line(alpha=.05, size=1.2, show.legend=F) +
  facet_wrap(~name, ncol=1) +
  theme(aspect.ratio=1) +
  #ylim(-400,200) +
  theme_bw() +
  labs(x="Objective Value", y="Subjective Value", title="Loss Aversion") +
  theme(text=element_text(size=10)) + 
  theme(strip.background=element_rect(fill="white"))+ 
  theme(strip.text=element_text(face="bold", size=10)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        axis.text.x = NULL,
        axis.text.y=element_text(size=10)) +
  scale_x_continuous(breaks=c(0)) +
  scale_y_continuous(breaks=c(0)) +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_line(
    data = pop_lambda %>% filter(name == "Description"),
    aes(x = v, y = value, group = NULL),
    inherit.aes = FALSE, # Ensures global aesthetics don't interfere
    size = 1.2, color = "black", alpha=0.8 # Make it distinct
  )


plot_B_1_2 <- d %>% pivot_longer(cols=c(vde, vex)) %>% 
  mutate(name=recode(name, vde="Description", vex="Experience")) %>% 
  filter(name == "Experience") %>% 
  ggplot(aes(x = v, y = value, group=id, color=name))+
  geom_line(alpha=.05, size=1.2, show.legend=F) +
  facet_wrap(~name, ncol=1) +
  theme(aspect.ratio=1) +
  #ylim(-400,200) +
  theme_bw() +
  labs(x="Objective Value", y="Subjective Value") +
  theme(text=element_text(size=10)) + 
  theme(strip.background=element_rect(fill="white"))+ 
  theme(strip.text=element_text(face="bold", size=10)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        axis.text.x = NULL,
        axis.text.y=element_text(size=10)) +
  scale_x_continuous(breaks=c(0)) +
  scale_y_continuous(breaks=c(0)) +
  scale_color_manual(values=c("darkred")) +
  geom_line(
    data = pop_lambda %>% filter(name == "Experience"),
    aes(x = v, y = value, group = NULL),
    inherit.aes = FALSE, # Ensures global aesthetics don't interfere
    size = 1.2, color = "black", alpha=0.8 # Make it distinct
  )















# ================================================== #
# make comparison boxplots
# ================================================== #

# tests for labels
res1 <- ttestBF(x=dat$lambda_de, y=dat$lambda_ex, paired=T)
res2 <- ttestBF(x=dat$gamma_de, y=dat$gamma_ex, paired=T)


# Prep labels
res1 <- round(as.vector(res1),2)
res2 <- round(as.vector(res2),2)
if (res1 > 1000) {
  res1 <- ">1000.00"
} else {
  res1 <- str_c("=", res1)
}
if (res2 > 1000) {
  res2 <- ">1000.00"
} else {
  res2 <- str_c("=", res1)
}



pdat <- dat %>% pivot_longer(cols=c("lambda_de", "lambda_ex")) %>% 
  mutate(name=recode(name, lambda_ex = "Experience", lambda_de = "Description"))
pdat$name <- factor(as.character(pdat$name), levels=c("Description", "Experience"))
plot_A_1 <- ggplot(data=pdat, aes(x=name, y=value, fill=name)) + 
  geom_boxplot(notch=T, outlier.alpha=0, alpha=.5) + 
  geom_line(aes(x=name, y=value, group=participant_id), alpha=.15)+ 
  geom_point(alpha=.1, position=position_jitter(w=0.05, h=0)) + theme_bw() + 
  labs(y=TeX(r'(${\lambda}$)'), x="Condition", title=str_c("Loss Aversion\n(BF", res1, ")")) + 
  theme(axis.text=element_text(hjust=.5, size=10),
        axis.title=element_text(size=10),
        axis.text.x=element_text(size=10, hjust = c(0.55, 0.45))) +
  scale_y_continuous(breaks=c(2,4)) +
  scale_fill_manual(values=c("darkblue", "darkred")) +
  guides(fill="none") +
  theme(text=element_text(size=10))

plot_A_1
pdat2 <- dat %>% pivot_longer(cols=c("gamma_de", "gamma_ex")) %>% 
  mutate(name=recode(name, gamma_ex = "Experience", gamma_de = "Description"))
pdat2$name <- factor(as.character(pdat2$name), levels=c("Description", "Experience"))

plot_A_2 <- ggplot(data=pdat2, aes(x=name, y=value, fill=name)) + 
  geom_boxplot(notch=T, outlier.alpha=0, alpha=.5) + 
  geom_line(aes(x=name, y=value, group=participant_id), alpha=.15)+ 
  geom_point(alpha=.1, position=position_jitter(w=0.05, h=0)) + theme_bw() + 
  labs(y=TeX(r'(${\gamma}$)'), x="Condition", title=str_c("NPW\n(BF", res2, ")")) + 
  theme(axis.text=element_text(hjust=.5, size=10),
        axis.title=element_text(size=10),
        axis.text.x=element_text(size=10, hjust = c(0.55, 0.45))) +
  scale_fill_manual(values=c("darkblue", "darkred")) +
  guides(fill="none") 
plot_A_2




plot_A_3 <- dat %>% mutate(de_gap = gamma_de-gamma_ex) %>% 
  ggplot(aes(y = de_gap, x=1)) + 
  geom_boxplot(notch=T, outlier.alpha=0, alpha=.5, fill="darkviolet") +
  geom_point(alpha=.1, position=position_jitter(w=0.1, h=0)) +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        axis.text.x = NULL,
        axis.text.y=element_text(size=10)) +
  scale_x_continuous(breaks=NULL)+
  labs(y = TeX(r'(${\gamma_D}-{\gamma_E}$)'), title="DE Gap\n", x="") +
  theme(text=element_text(size=10))


plot_A_3








# ================================================== #
# make correlation plots
# ================================================== #



# read from file 03_make_tests.R, for labels of plots
dat$gamma_de_gap <- dat$gamma_de - dat$gamma_ex
dat$lambda_de_gap <- dat$lambda_de - dat$lambda_ex
res <- read.csv("data/res_bayes_rho.csv")




labels <- c(
  str_c("γ Description\nρ = ",
        format(round(res$cors[1], 2), nsmall=2),
        " (BF=",
        sub("^.", "0",round(res$bf[1],2)),
        ")"),
  str_c("γ Experience\nρ = ",
        round(res$cors[2], 2),
        " (BF=",
        sub("^.", "0",round(res$bf[2],2)),
        ")"),
  str_c("DE Gap\nρ = ",
        round(res$cors[3], 2),
        " (BF=",
        sub("^.", "0",round(res$bf[3],2)),
        ")"),
  str_c("λ Description\nρ = ",
        format(round(res$cors[4], 2), nsmall=2),
        " (BF=",
        sub("^.", "0",round(res$bf[4],2)),
        ")"),
  str_c("λ Experience\nρ = ",
        round(res$cors[5], 2),
        " (BF=",
        sub("^.", "0",round(res$bf[5],2)),
        ")")
)




# recode for order in plots and correct labels
dat_for_plot_C <- pivot_longer(dat,
                               cols=c("lambda_de",
                                      "lambda_ex",
                                      "gamma_de",
                                      "gamma_ex",
                                      "gamma_de_gap")) %>% 
  mutate(name=recode(name,
                     gamma_de = "Gamma Description",
                     gamma_ex = "Gamma Experience",
                     lambda_de = "Lambda Description",
                     lambda_ex = "Lambda Experience",
                     gamma_de_gap = "DE Gap")) %>% 
  mutate(name = factor(as.character(name),
                       levels=c("Lambda Description",
                                "Lambda Experience",
                                "Gamma Description",
                                "Gamma Experience",
                                "DE Gap")))
library(BayesFactor)



# lambda correlation plot

plot_C_1 <- ggplot(dat_for_plot_C %>% filter(name %in% labels[4:5]), aes(x=value, y=UIS_GESAMT, color=name)) + 
  stat_smooth(geom="line", se=F, method="lm", alpha=.5, size=2) + 
  geom_point(size=2, alpha=.4) +
  theme_bw() +
  labs(x=TeX(r'($\lambda$)'), y="IU", color="Condition", title="Loss Aversion") +
  theme(text=element_text(size=10), legend.position = "bottom") + 
  theme(strip.background=element_rect(fill="white"))+ 
  theme(strip.text=element_text(face="bold", size=10)) +
  theme(axis.text=element_text(angle=0, hjust=.5, size=10),
        axis.title=element_text(size=10)) +
  guides(color=guide_legend(nrow=2, byrow=T)) +
  scale_color_manual(values=c("darkblue", "darkred")) +
  guides(color="none")
plot_C_1


# gamma correlation plot
plot_C_2 <- ggplot(dat_for_plot_C %>% filter(name %in% labels[1:2]), aes(x=value, y=UIS_GESAMT, color=name)) + 
  stat_smooth(geom="line", se=F, method="lm", alpha=.5, size=2) + 
  geom_point(size=2, alpha=.4) +
  theme_bw() +
  labs(x=TeX(r'($\gamma$)'), y="IU", color="Condition", title="Nonlinear Probability\nWeighting") +
  theme(text=element_text(size=10)) + 
  theme(strip.background=element_rect(fill="white"))+ 
  theme(strip.text=element_text(face="bold", size=10)) +
  theme(axis.text=element_text(angle=0, hjust=.5, size=10),
        axis.title=element_text(size=10),
        legend.position="bottom") +
  scale_color_manual(values=c("darkblue", "darkred"))+
  guides(color=guide_legend(nrow=2, byrow=T)) +
  guides(color="none")
plot_C_2


# only DE Gap plot
plot_C_3 <- ggplot(dat_for_plot_C %>% filter(name==labels[3]), aes(x=value, y=UIS_GESAMT,color=labels[3])) + 
  stat_smooth(geom="line", se=F, method="lm", alpha=.5, size=2) + 
  geom_point(size=2, alpha=.4) +
  theme_bw() +
  labs(x=TeX(r'($\mu_{\gamma,D}-\mu_{\gamma,E}$)'), y="IU", title="DE Gap") +
  theme(text=element_text(size=10)) + 
  theme(axis.text=element_text(angle=0, hjust=.5, size=10),
        axis.title=element_text(size=10),
        legend.position="bottom") +
  guides(color=guide_legend(nrow=2, byrow=T)) + 
  labs(color="") +
  scale_color_manual(values=c("darkviolet")) +
  guides(color="none")
plot_C_3

plot_C_4 <- ggplot(dat, aes(x=sample_mean, y=UIS_GESAMT)) + 
  stat_smooth(geom="line", se=F, method="lm", alpha=.5, size=2) + 
  geom_point(size=2, alpha=.4) +
  theme_bw() +
  labs(x="Median Sampling", y=" IU", title="Median Sampling") +
  theme(text=element_text(size=10)) + 
  theme(axis.text=element_text(angle=0, hjust=.5, size=10),
        axis.title=element_text(size=10),
        legend.position="bottom") +
  guides(color=guide_legend(nrow=2, byrow=T)) + 
  labs(color="") +
  scale_color_manual(values=c("grey")) +
  guides(color="none")
plot_C_4




plot_C_5 <- ggplot(dat, aes(x=lambda_de_gap, y=UIS_GESAMT)) + 
  stat_smooth(geom="line", se=F, method="lm", alpha=.5, size=2) + 
  geom_point(size=2, alpha=.4) +
  theme_bw() +
  labs(x="Mean Sampling", y=" IU", title="Lambda Gap") +
  theme(text=element_text(size=10)) + 
  theme(axis.text=element_text(angle=0, hjust=.5, size=10),
        axis.title=element_text(size=10),
        legend.position="bottom") +
  guides(color=guide_legend(nrow=2, byrow=T)) + 
  labs(color="", x=TeX(r'($\mu_{\lambda,D}-\mu_{\lambda,E}$)')) +
  scale_color_manual(values=c("grey")) +
  guides(color="none")
plot_C_5


plot_C_6 <- ggplot(dat, aes(x=de_gap_rule, y=UIS_GESAMT)) + 
  stat_smooth(geom="line", se=F, method="lm", alpha=.5, size=2) + 
  geom_point(size=2, alpha=.4) +
  theme_bw() +
  labs(x="Mean Sampling", y=" IU", title="DE Gap (discrete)") +
  theme(text=element_text(size=10)) + 
  theme(axis.text=element_text(angle=0, hjust=.5, size=10),
        axis.title=element_text(size=10),
        legend.position="bottom") +
  guides(color=guide_legend(nrow=2, byrow=T)) + 
  labs(color="", x="DE Gap") +
  scale_color_manual(values=c("grey")) +
  guides(color="none")
plot_C_6




# Pop level


pop_level_d <- read.csv("data/fit/pop_level/Prelec_gam_LA_pop_level.csv")


pop_level_d$gap <- pop_level_d$mu_gamma_de - pop_level_d$mu_gamma_ex

m_pop <- mean(pop_level_d$gap)
lower_upper <- t.test(pop_level_d$gap)$conf.int
lower_upper <- c(m_pop-sd(pop_level_d$gap),
                 m_pop+sd(pop_level_d$gap))

pop_plot <- ggplot(data=pop_level_d, aes(x=gap)) + 
  geom_histogram(bins=round(sqrt(1000)),
                 aes(y = after_stat(count / sum(count))),
                 fill="white", color="black") + 
  theme_bw() + labs(y="Relative Frequency",x=TeX(r'($\mu_{\gamma,D}-\mu_{\gamma,E}$)')) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),) +
  scale_x_continuous(breaks=c(-.5, -0.4, -.3, -.2, -.1)) +
  theme(text=element_text(size=10)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())


# ================================================== #
# Full plot with all facets
# ================================================== #











library(patchwork)
plot_A_1 + labs(tag="A)") + theme(plot.caption = element_text(hjust = 0),
                                  plot.tag.position = c(0.055, 1)) +
  plot_A_2 + plot_A_3 +
  plot_B_1_1 + labs(tag="B)") + theme(plot.caption = element_text(hjust = 0),
                                      plot.tag.position = c(0.055, 1)) +
  plot_B_2_1 + plot_B_3 + labs(tag="C)")+ theme(plot.caption = element_text(hjust = 0),
                                                plot.tag.position = c(0.055, 1)) +
  plot_spacer() + plot_spacer() + plot_spacer() +
  plot_B_1_2 + plot_B_2_2 + pop_plot+ labs(tag="D)") + theme(plot.caption = element_text(hjust = 0),
                                                             plot.tag.position = c(0.055, 1))+
  
  plot_layout(heights=c(1.5, 2, -0.6,  2), axis_titles = "collect", nrow=4)


ggsave("main_plot.png",units="cm", width=16, height=16.8, dpi=300)





plot_C_1 + plot_C_2 + plot_C_3 + plot_C_4 + plot_C_6 + plot_C_5 +
  plot_layout(nrow=2, axis_titles = "collect")

ggsave("main_plot2.png", units="cm", width=16, height=13.5, dpi=300)






# p1 <- egg::ggarrange(plot_A_1, plot_A_2, plot_A_3,
#                plot_B_1, plot_B_2, plot_B_3,
#                nrow=2, widths=c(1,1,1), heights=c(1,1.1), 
#                labels = c("A)","", "", "B)", "", ""),
#                label.args=list(gp=grid::gpar(font=14, fontsize=24)))
# p1
#ggsave("main_plot.png", plot=arrangeGrob(final_grid),units="cm", width=16.5, height=17, dpi=300)



p2 <- egg::ggarrange(plot_C_1, plot_C_2, plot_C_3, plot_C_4,
                     nrow=2, widths=c(1,1,1,1))



ggsave("main_plot2.png",plot=p2, units="cm", width=16, height=14, dpi=300)
