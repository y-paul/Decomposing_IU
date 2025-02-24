dat_tp <- dat

calc_choice_max_treat <- function(dat_tp) {
  library(tidyverse)

  # which problem maximizes treatment EU?
  dat_tp$treat_eu_A <- dat_tp$a_be * dat_tp$pa_be
  dat_tp$treat_eu_B <- dat_tp$b_be * dat_tp$pb_be
  dat_tp$which_p_max_treat_eu <- ifelse(dat_tp$treat_eu_A > dat_tp$treat_eu_B, "A", "B")
  dat_tp$option_chosen <- recode(dat_tp$option_chosen, `0`="B", `1`="A")

  # add to exp
  dat_tp[dat_tp$cond == "exp",]$treat_eu_A <- dat_tp[dat_tp$cond == "exp",]$a_be * dat_tp[dat_tp$cond == "exp",]$a.b.f
  dat_tp[dat_tp$cond == "exp",]$treat_eu_B <- dat_tp[dat_tp$cond == "exp",]$b_be * dat_tp[dat_tp$cond == "exp",]$b.b.f
  
  
  dat_tp$which_p_max_treat_eu <- ifelse(dat_tp$treat_eu_A > dat_tp$treat_eu_B, "A", "B")
  dat_tp$option_chosen <- recode(dat_tp$option_chosen, `0`="B", `1`="A")
  opt <- c()
  for (i in 1:dim(dat_tp)[1]) {
    if (dat_tp[i,"which_p_max_treat_eu"] == dat_tp[i,"option_chosen"]) {
      opt[i] <- T
    } else {
      opt[i] <- F
    }
    
  }
  
  
  dat_tp$opt_max_treat_eu <- opt 
  return(dat_tp %>% select(which_p_max_treat_eu, opt_max_treat_eu))
  
}

calc_choice_min_se <- function(dat_tp) {
  library(tidyverse)
  
  # which problem maximizes se EU?
  dat_tp$se_eu_A <- dat_tp$a_se * dat_tp$pa_se
  dat_tp$se_eu_B <- dat_tp$b_se * dat_tp$pb_se
  dat_tp$which_p_max_se_eu <- ifelse(dat_tp$se_eu_A < dat_tp$se_eu_B, "A", "B")
  dat_tp$option_chosen <- recode(dat_tp$option_chosen, `0`="B", `1`="A")
  
  # add to exp
  dat_tp[dat_tp$cond == "exp",]$se_eu_A <- dat_tp[dat_tp$cond == "exp",]$a_se * dat_tp[dat_tp$cond == "exp",]$a.se.f
  dat_tp[dat_tp$cond == "exp",]$se_eu_B <- dat_tp[dat_tp$cond == "exp",]$b_se * dat_tp[dat_tp$cond == "exp",]$b.se.f
  
  dat_tp$which_p_max_se_eu <- ifelse(dat_tp$se_eu_A < dat_tp$se_eu_B, "A", "B")
  dat_tp$option_chosen <- recode(dat_tp$option_chosen, `0`="B", `1`="A")
  opt <- c()
  for (i in 1:dim(dat_tp)[1]) {
    if (dat_tp[i,"which_p_max_se_eu"] == dat_tp[i,"option_chosen"]) {
      opt[i] <- T
    } else {
      opt[i] <- F
    }
    
  }
  
  
  dat_tp$opt_max_se_eu <- opt 
  return(dat_tp %>% select(which_p_max_se_eu, opt_max_se_eu))
}

calc_choice_max_eu <- function(dat_tp) {
  library(tidyverse)
  
  # which problem maximizes treatment EU?
  dat_tp$treat_eu_A <- dat_tp$a_be * dat_tp$pa_be - dat_tp$a_se * dat_tp$pa_se
  dat_tp$treat_eu_B <- dat_tp$b_be * dat_tp$pb_be - dat_tp$b_se * dat_tp$pb_se
  dat_tp$which_p_max_treat_eu <- ifelse(dat_tp$treat_eu_A > dat_tp$treat_eu_B, "A", "B")
  dat_tp$option_chosen <- recode(dat_tp$option_chosen, `0`="B", `1`="A")
  
  # add to exp
  dat_tp[dat_tp$cond == "exp",]$treat_eu_A <- dat_tp[dat_tp$cond == "exp",]$a_be * dat_tp[dat_tp$cond == "exp",]$a.b.f -
    dat_tp[dat_tp$cond == "exp",]$a_se * dat_tp[dat_tp$cond == "exp",]$a.se.f
  dat_tp[dat_tp$cond == "exp",]$treat_eu_B <- dat_tp[dat_tp$cond == "exp",]$b_be * dat_tp[dat_tp$cond == "exp",]$b.b.f -
    dat_tp[dat_tp$cond == "exp",]$b_se * dat_tp[dat_tp$cond == "exp",]$b.se.f
  
  dat_tp$which_p_max_treat_eu <- ifelse(dat_tp$treat_eu_A > dat_tp$treat_eu_B, "A", "B")
  dat_tp$option_chosen <- recode(dat_tp$option_chosen, `0`="B", `1`="A")
  opt <- c()
  for (i in 1:dim(dat_tp)[1]) {
    if (dat_tp[i,"which_p_max_treat_eu"] == dat_tp[i,"option_chosen"]) {
      opt[i] <- T
    } else {
      opt[i] <- F
    }
    
  }
  
  
  dat_tp$opt_max_treat_eu <- opt 
  return(dat_tp %>% select(which_p_max_treat_eu, opt_max_treat_eu))
}


calc_choice_max_treat_value <- function(dat_tp) {
  library(tidyverse)
  
  # which problem maximizes treatment EU?
  dat_tp$treat_eu_A <- dat_tp$a_be 
  dat_tp$treat_eu_B <- dat_tp$b_be 
  dat_tp$which_p_max_treat_eu <- ifelse(dat_tp$treat_eu_A > dat_tp$treat_eu_B, "A", "B")
  dat_tp$option_chosen <- recode(dat_tp$option_chosen, `0`="B", `1`="A")
  
  # add to exp
  dat_tp[dat_tp$cond == "exp",]$treat_eu_A <- dat_tp[dat_tp$cond == "exp",]$a_be
  dat_tp[dat_tp$cond == "exp",]$treat_eu_B <- dat_tp[dat_tp$cond == "exp",]$b_be
  
  
  dat_tp$which_p_max_treat_eu <- ifelse(dat_tp$treat_eu_A > dat_tp$treat_eu_B, "A", "B")
  dat_tp$option_chosen <- recode(dat_tp$option_chosen, `0`="B", `1`="A")
  opt <- c()
  for (i in 1:dim(dat_tp)[1]) {
    if (dat_tp[i,"which_p_max_treat_eu"] == dat_tp[i,"option_chosen"]) {
      opt[i] <- T
    } else {
      opt[i] <- F
    }
    
  }
  
  
  dat_tp$opt_max_treat_eu <- opt 
  return(dat_tp %>% select(which_p_max_treat_eu, opt_max_treat_eu))
}


calc_choice_min_se_value <- function(dat_tp) {
  library(tidyverse)
  
  # which problem maximizes se EU?
  dat_tp$se_eu_A <- dat_tp$a_se
  dat_tp$se_eu_B <- dat_tp$b_se
  dat_tp$which_p_max_se_eu <- ifelse(dat_tp$se_eu_A < dat_tp$se_eu_B, "A", "B")
  dat_tp$option_chosen <- recode(dat_tp$option_chosen, `0`="B", `1`="A")
  
  # add to exp
  dat_tp[dat_tp$cond == "exp",]$se_eu_A <- dat_tp[dat_tp$cond == "exp",]$a_se
  dat_tp[dat_tp$cond == "exp",]$se_eu_B <- dat_tp[dat_tp$cond == "exp",]$b_se
  
  dat_tp$which_p_max_se_eu <- ifelse(dat_tp$se_eu_A < dat_tp$se_eu_B, "A", "B")
  dat_tp$option_chosen <- recode(dat_tp$option_chosen, `0`="B", `1`="A")
  opt <- c()
  for (i in 1:dim(dat_tp)[1]) {
    if (dat_tp[i,"which_p_max_se_eu"] == dat_tp[i,"option_chosen"]) {
      opt[i] <- T
    } else {
      opt[i] <- F
    }
    
  }
  
  
  dat_tp$opt_max_se_eu <- opt 
  return(dat_tp %>% select(which_p_max_se_eu, opt_max_se_eu))
}



