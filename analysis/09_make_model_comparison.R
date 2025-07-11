rm(list = ls())

# make the model comparison plots in SI1


library(tidyverse)






# estimates ---------------------------------------------------------------

# 
est_mods = readRDS('fit/stan_estimation/posteriors/est_mods_fin.rds')

# get performance of each model
perf = lapply(est_mods, function(x) x$performance )

# get elpds
elpds = lapply(names(perf), function(xx) {
  
  x = perf[[xx]]
  
  #
  rr = lapply(names(x), function(yy) {
    
    y = x[[yy]]
    
    # get loo estimates
    ee = y$looE$estimates[1,]
    # into M and 95% CI
    e = c(ee[1], ee[1] + ee[2] * c(1.96, -1.96))
    
    # get accuracy
    ba = y$ba$ba
    #
    r = data.frame(matrix(c(e, ba), nrow = 1))
    colnames(r) = c('elpd', 'elpd_li', 'elpd_ui', 'loo_accuracy')
    r$cond = yy
    return(r)
    
  }); 
  r = rbind(rr[[1]], rr[[2]])
  r$model = xx
  
  return(r)
  
}); elpds = do.call(rbind, elpds)

elpds$model = factor(elpds$model,
                     levels = c('Lin_LA', 'TK_gam_LA', 'GE_gam_LA',
                                'POW_gam_LA', 'Prelec_gam_LA'),
                     labels = c('Linear', 'TK', 'GE', 'Power', 'Prelec'),
                     ordered = T)
elpds$cond = factor(elpds$cond,
                    levels = c('de', 'ex'),
                    labels = c('Description', 'Experience'))

write.csv2(elpds, 
           'analysis/models_oos_performance.csv',
           row.names = F)


# plots -------------------------------------------------------------------

dx = read.csv2('analysis/models_oos_performance.csv')

library(ggplot2)
library(patchwork)
elpds <- dx


elpds <- elpds %>%
  mutate(model = recode(model, Linear = "Linear",
                        Prelec = "Model A",
                        GE = "Model B",
                        TK = "Model C",
                        Power = "Model D")) %>% 
  mutate(model = fct_relevel(model, "Model A", "Model B", "Model C", "Model D", "Linear"))
# elpds
elpds_plt = ggplot(elpds,
                   mapping = aes(x = model,
                                 y = elpd,
                                 ymin = elpd_li,
                                 ymax = elpd_ui)) +
  labs(x= "Model", y="ELPD") + 
  geom_pointrange() +
  facet_wrap(~cond) +
  theme_bw() +
  theme(axis.title.x = element_blank())

looAcc_plt = ggplot(elpds,
                    mapping = aes(x = model,
                                  y = loo_accuracy)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~cond) +
  ylab('LOO Accuracy') +
  theme_bw() +
  coord_cartesian(ylim = c(.5, .85))

(comp_perf_plt = wrap_plots(elpds_plt, looAcc_plt, nrow = 2))

ggsave("model_comparison.png", plot=comp_perf_plt, unit="cm", dpi=300,
       width=16, height = 10)

