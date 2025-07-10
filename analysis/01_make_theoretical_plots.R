library(tidyverse)
library(gridExtra)
RECALC_BAYES <- F
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

x <- seq(from=-100, to=100, by=.01)
y1 <- value_function(x, 1)
y2 <- value_function(x, 2)


df_value <- data.frame(x, y1, y2)
df_value <- df_value %>% pivot_longer(c("y1", "y2")) %>% mutate(name=recode(name, y1 = "λ = 1 (no LA)", y2="λ = 2 (high LA)"))



p1 <- ggplot(df_value, aes(x=x, y = value, color=name)) + 
  geom_vline(xintercept=0, alpha=.5, linetype="dotted") +
  geom_hline(yintercept=0, alpha=.5, linetype="dotted") +
  geom_line( size=2, alpha=.5) +
  theme_classic() + 
  ylim(c(-100, 100)) +
  xlim(c(-100,100)) + 
  scale_y_continuous(breaks=c(0), limits=c(-200,150))+
  scale_x_continuous(breaks=c(0), limits=c(-100,100))+
  labs(x="Objective Value", y="Subjective Value", color="Loss Aversion (LA)", title="") +
  theme(axis.text = element_text(size=16),
        axis.title=element_text(size=18),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18),
        plot.title=element_text(size=22),
        legend.position=c(.7, .2),
        legend.box.background = element_rect(colour = "black", size=.5),
        legend.spacing.y = unit(0, "mm"),
        aspect.ratio=1) +
  scale_color_manual(values=c("darkgrey", "darkgreen")) + 
  annotate("text", y=-120, x=-50, angle=50, size=6, label="Overweighting") + 
  annotate("text", y=120, x=-60, size=7, label="Losses")+ 
  annotate("text", y=120, x=60, size=7, label="Gains")

p1



probability_weighting_function <- function(p, gamma) {
  return(  exp( - (  - log(p) )^gamma  )  )
}


p <- seq(0, 1, by=.0001)
yp1 <- probability_weighting_function(p, .35)
yp2 <- probability_weighting_function(p, 1)

dfp <- data.frame(p, yp1, yp2) %>% pivot_longer(c("yp1","yp2"))  %>%
  mutate(name=recode(name, yp2 = "γ = 1.0 (no PD)", yp1="γ = 0.4 (high PD)"))

p2 <- ggplot(dfp, aes(x=p, y=value, color=name)) + 
  geom_vline(xintercept=0.5, alpha=.5, linetype="dotted") +
  geom_hline(yintercept=0.5, alpha=.5, linetype="dotted") +
  geom_line(size=2, alpha=.5) + 
  theme_classic() + 
  scale_y_continuous(breaks=c(0,.5,1))+
  scale_x_continuous(breaks=c(0,.5,1))+
  labs(x="Objective Probability", y="Decision Weight", color="Probability distortion (PD)", title="") +
  theme(axis.text = element_text(size=16),
        axis.title=element_text(size=18),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18),
        plot.title=element_text(size=22),
        legend.position=c(.67, .16),
        legend.box.background = element_rect(colour = "black", size=.5),
        legend.spacing.y = unit(0, "mm"),
        aspect.ratio=1) +
  scale_color_manual(values=c("darkgreen", "darkgrey")) +
  annotate("text", y=0.35, x=0.15, angle=25, size=6, color="darkgreen", label="Overweighting")+
  annotate("text", y=0.50, x=0.85, angle=40, size=6, color="darkgreen", label="Underweighting")


p2









p <- seq(0, 1, by=.0001)
yp1 <- probability_weighting_function(p, .3)
yp2 <- probability_weighting_function(p, .6)
yp3 <- probability_weighting_function(p, 1.7)
dfp <- data.frame(p, yp1, yp2, cond = rep("1", times=length(p))) %>% mutate(higher = ifelse(yp1>yp2, yp1, yp2), lower=ifelse(yp1<yp2, yp1, yp2)) %>% 
  pivot_longer(c("yp1","yp2"))  %>% mutate(name=recode(name, yp1 = "Experience A", yp2="Description"))
dfp2 <- data.frame(p, yp2, yp3, cond = rep("2", times=length(p))) %>% mutate(higher = ifelse(yp3>yp2, yp3, yp2), lower=ifelse(yp3<yp2, yp3, yp2)) %>% 
  pivot_longer(c("yp3","yp2"))  %>% mutate(name=recode(name, yp3 = "Experience B", yp2="Description"))

dfp <- rbind(dfp, dfp2)



p3 <- ggplot(dfp, aes(x=p, y=value, color=name, fill=cond,ymin=higher, ymax=lower)) +
  geom_ribbon(alpha=.1) +
  geom_vline(xintercept=0.5, alpha=.5, linetype="dotted") +
  geom_hline(yintercept=0.5, alpha=.5, linetype="dotted") +
  geom_line(size=2) + 
  theme_classic() + 
  scale_y_continuous(breaks=c(0,.5,1))+
  scale_x_continuous(breaks=c(0,.5,1))+
  labs(x="Objective Probability", y="Decision Weight", color="Decision Condition", title="") +
  theme(axis.text = element_text(size=16),
        axis.title=element_text(size=18),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18),
        plot.title=element_text(size=22),
        legend.position=c(.7, .18),
        legend.box.background = element_rect(colour = "black", size=.5),
        legend.spacing.y = unit(0, "mm"),
        aspect.ratio=1) +
  scale_color_manual(values=c("darkgrey", "darkgreen", "deeppink"))+
  scale_fill_manual(values=c("darkgreen", "deeppink")) +
  annotate("segment", x=.85, y=.45, yend=.85, xend=.85, size=1.3) + 
  annotate("segment", x=.85, y=.45, yend=.55, xend=.75, size=1.3) + 
  annotate("text", x=.85, y=.4, label="DE-Gaps",size=6) + 
  guides(fill="none")








p3


library(gridExtra)
library(grid)

t <- grid.arrange(p1, p2, p3, nrow=1, top = textGrob("",gp=gpar(fontsize=30,font=2)))

grid.arrange(p1, p2, p3, nrow=1, top = textGrob("",gp=gpar(fontsize=30,font=2)))

egg::ggarrange(p1, p2, p3, nrow=1)

#ggsave("test.emf",
#       plot=t,
#       width=13.33,
#       height=3.75,
#       units="in",
#       dpi=150
#       )



p1
p2
p3
