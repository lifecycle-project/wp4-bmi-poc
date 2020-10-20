################################################################################
## Project: bmi-poc
## Script purpose: Make tables for paper
## Date: 29th April 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

# Here we put the results from "bmi-poc-analysis.R" into some tables for the
# paper
#
# Remaining functionality required is to be able to show median - I haven't
# worked out how to do this in DS yet

library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)

install.packages("lubridate")
install.packages("tidyverse")
install.packages("tidyr")

ls("package:dsBaseClient")

################################################################################
# METHODS  
################################################################################
################################################################################
# List participating cohorts  
################################################################################
names(opals)

################################################################################
# Maximum sample size
################################################################################
cohort_ns <- descriptives_ss[[1]] %>%
  filter(variable == "sex" & category == 1) %>%
  select(cohort, cohort_n) %>%
  arrange(cohort) %>%
  mutate(
    cohort_n = as.character(cohort_n),
    cohort_neat <- c("CHOP", "DNBC", "GECKO", "Gen-R", "INMA", "MoBa", "NINFEA", 
                     "RAINE", "Combined"),
    comb = paste0(cohort_neat, " (n=", cohort_n, ")"))


################################################################################
# Table S2: Covariate descriptive statistics  
################################################################################
cov_cat.tab <- descriptives_ss$categorical %>%
  filter(variable %in% c("sex", "parity_bin", "preg_smk_rev", "preg_ht_rev",
         "ethn3_m_rev")) %>%
  mutate(n_perc = paste0(value, " (", valid_perc, ")"), 
         missing = paste0(missing_n, " (", missing_perc, ")")) %>%
  select(cohort, variable, category, n_perc, missing) %<>% 
  pivot_wider(names_from = c(variable, category),  
              values_from = c(n_perc, missing))

cov_green.tab <- green.tab$categorical %>%
  filter(variable == "areases_tert_0_1_f") %>%
  mutate(n_perc = paste0(value, " (", valid_perc, ")"), 
         missing = paste0(missing_n, " (", missing_perc, ")")) %>%
  select(cohort, variable, category, n_perc, missing) %<>% 
  pivot_wider(names_from = c(variable, category),  
              values_from = c(n_perc, missing))

cov_cat.tab <- merge(cov_cat.tab, cov_green.tab, by = "cohort")

cov_cont.tab <- descriptives_ss$continuous %>%
  filter(variable %in% c("prepreg_bmi", "agebirth_m_y")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")"),
         missing = paste0(missing_n, " (", missing_perc, ")")) %>%
  select(cohort, variable, med_range, missing) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, missing))

cov.tab <- cbind(cov_cat.tab, select(cov_cont.tab, -cohort)) %>%
  select(cohort, n_perc_sex_1, missing_sex_1, n_perc_parity_bin_0,
         missing_parity_bin_0, n_perc_ethn3_m_rev_1, n_perc_ethn3_m_rev_2,
         n_perc_ethn3_m_rev_3, missing_ethn3_m_rev_1, 
         n_perc_areases_tert_0_1_f_1, n_perc_areases_tert_0_1_f_2, 
         n_perc_areases_tert_0_1_f_3, missing_areases_tert_0_1_f_1, 
         n_perc_preg_smk_rev_1, missing_preg_smk_rev_1, med_range_prepreg_bmi, 
         missing_prepreg_bmi, med_range_agebirth_m_y, missing_agebirth_m_y, 
         n_perc_preg_ht_rev_1, missing_preg_ht_rev_1) %>%
  arrange(cohort) %>%
  mutate(cohort = cohort_ns$comb) %>%
  as_tibble()

write.csv(cov.tab)


################################################################################
# Table 1: Exposures descriptive statistics  
################################################################################
exposure_cat <- descriptives_ss$categorical %>%
  filter(variable %in% c("edu_m", "preg_dia")) %>%
  mutate(n_perc = paste0(value, " (", valid_perc, ")")) %>%
  select(cohort, variable, category, n_perc) %>%
  pivot_wider(names_from = c(variable, category), values_from = n_perc) 

exposure_cont <- descriptives_ss$continuous %>%
  filter(variable == "ga_all") %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range) %>%
  pivot_wider(names_from = variable, values_from = med_range)

exposure_green_cont <- green.tab$continuous %>%
  filter(variable == "ndvi300_0_1") %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range) %>%
  pivot_wider(names_from = variable, values_from = med_range)

exposure_cont <- merge(exposure_cont, exposure_green_cont)

exposure.tab <- cbind(exposure_cat, select(exposure_cont, -cohort)) %>%
  select(cohort, ndvi300_0_1, edu_m_1, edu_m_2, edu_m_3, ga_all, preg_dia_1) %>%
  arrange(cohort) %>%
  mutate(cohort = cohort_ns$comb) %>%
  as_tibble()

write.csv(exposure.tab)

################################################################################
# Table 2: Outcomes descriptive statistics  
################################################################################
outcomes.tab <- descriptives_ss$continuous %>%
  filter(variable %in% c("bmi.730", "bmi.1461", "bmi.2922", "bmi.5113", 
         "bmi.6544", "age_months.24", "age_months.48", "age_months.96", 
         "age_months.168", "age_months.215")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  select(cohort, valid_n_bmi.730, med_range_age_months.24, med_range_bmi.730, 
         valid_n_bmi.1461, med_range_age_months.48, med_range_bmi.1461,
         valid_n_bmi.2922, med_range_age_months.96, med_range_bmi.2922,
         valid_n_bmi.5113, med_range_age_months.168, med_range_bmi.5113, 
         valid_n_bmi.6544, med_range_age_months.215, med_range_bmi.6544) %>%
  arrange(cohort) %>%
  mutate(cohort = cohort_ns$comb)

write.csv(outcomes.tab)

## ---- Available n by cohort --------------------------------------------------
ds.summary("analysis_df", datasources = coh())


################################################################################
# Additional analysis: height and weight by age to check BMI validity  
################################################################################
ht_wt.tab <- descriptives_ss$continuous %>%
  filter(variable %in% c("ht.730", "ht.1461", "ht.2922", "ht.5113", "ht.6544", 
                         "wt.730", "wt.1461", "wt.2922", "wt.5113", "wt.6544")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
select(cohort, valid_n_ht.730, med_range_ht.730, valid_n_ht.1461, 
       med_range_ht.1461, valid_n_ht.2922, med_range_ht.2922, valid_n_ht.5113, 
       med_range_ht.5113, valid_n_ht.6544, med_range_ht.6544, valid_n_wt.730, 
       med_range_wt.730, valid_n_wt.1461, med_range_wt.1461, valid_n_wt.2922, 
       med_range_wt.2922, valid_n_wt.5113, med_range_wt.5113, valid_n_wt.6544, 
       med_range_wt.6544) %>%
  arrange(cohort) %>%
  mutate(cohort = cohort_ns$comb)
  
write.csv(ht_wt.tab)


################################################################################
# Table 3: Main analysis  
################################################################################
mat_ed.tab <- mat_ed.fit[[1]] %>% 
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) 

ndvi.tab <- ndvi.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "ndvi300_0_1") 

preg_dia.tab <- preg_dia.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "preg_dia1") 

ipd_reg.tab <- bind_rows(mat_ed.tab, ndvi.tab, preg_dia.tab)

colnames(ipd_reg.tab) <- c(
  "variable", "age_0_24", "age_25_48", "age_49_96", "age_96_168")

write.csv(ipd_reg.tab)

# Need sample size for each analysis.
mat_ed_n <- mat_ed.fit[[1]] %>% map_int(function(x){x$Nvalid})
ndvi_n <- ndvi.fit[[1]] %>% map_int(function(x){x$Nvalid})
preg_dia_n <- preg_dia.fit[[1]] %>% map_int(function(x){x$Nvalid})

ipd_ns.tab <- bind_rows(mat_ed_n, ndvi_n, preg_dia_n) %>%
  mutate(exposure = c("mat_ed", "ndvi", "preg_dia")) %>%
  select(exposure, everything())

write.csv(ipd_ns.tab)


################################################################################
# Forest plot function
################################################################################

dh.forestData <- function(obj, mod){
  
## By study
  betas <- as_tibble(t(obj$betamatrix.valid)) %>%
    mutate(cohort = mod$cohort) %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "beta",
      cols = -cohort)
  
  ses <- as_tibble(t(obj$sematrix.valid)) %>%
    mutate(cohort = mod$cohort) %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "se",
      cols = -cohort)
  
## Combined
  beta_comb <- as_tibble(t(obj$SLMA.pooled.ests.matrix[, "pooled.ML"])) %>%
    mutate(cohort = "combined") %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "beta",
      cols = -cohort)
  
  se_comb <- as_tibble(t(obj$SLMA.pooled.ests.matrix[, "se.ML"])) %>%
    mutate(cohort = "combined") %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "se",
      cols = -cohort)
  
  out_sep <- left_join(betas, ses, by = c("cohort", "variable"))
  out_comb <- left_join(beta_comb, se_comb, by = c("cohort", "variable"))
  
  out <- bind_rows(out_sep, out_comb) %>%
    mutate(
      ci_5 = beta - 1.96*se,
      ci_95 = beta + 1.96*se)
  
  return(out)
  
}

mat_ed.pdata <- list(mat_ed.fit[[2]], mat_ed.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "mat_ed")
  
ndvi.pdata <- list(ndvi.fit[[2]], ndvi.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "ndvi")

preg_dia.pdata <- list(preg_dia.fit[[2]], preg_dia.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "preg_dia")
  
slma.pdata <- bind_rows(mat_ed.pdata, ndvi.pdata, preg_dia.pdata) %>%
  mutate(age = factor(
    age, 
    ordered = TRUE, 
    levels = c("bmi_24", "bmi_48", "bmi_96", "bmi_168")))

table(slma.pdata$cohort)

library(ggplot2)


################################################################################
# Theme  
################################################################################
forest_theme <-   theme(
  plot.background = element_rect(fill =scales::alpha("#CCCCCC", 0.3)),  #Background outside of plot
  panel.background = element_rect(fill="white"),  #Background for plot
  panel.grid.major=element_line(colour="grey"), #Major and minor gridlines
  panel.grid.minor=element_line(colour="white"), 
  panel.spacing = unit(1, "lines"),
  plot.title = element_text(hjust = 0, vjust=0, size=11, face="bold"), #Plot title, thought don't tend to use
  text=element_text(size=10), #General text 
  axis.title.y = element_text(family="Akzidenz Grotesk Reg", size=11, margin = margin(t = 0, r = 10, b = 0, l = 0)), #Axis labels
  axis.title.x = element_text(family="Akzidenz Grotesk Reg", size=11, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x = element_text(family="Akzidenz Grotesk Light", size=8, margin = margin(t = 4, r=0, b=0, l=0), colour="black"), #Axis text
  axis.text.y = element_text(family="Akzidenz Grotesk Light", size=8, margin = margin(t = 0, r=4, b=0, l=0), colour="black"),
  axis.ticks.length=unit(0.3, "cm"),
  axis.ticks = element_line(colour = "grey"),
  strip.text.x = element_text(family="Akzidenz Grotesk Reg", size=11),
  strip.background = element_blank(),
  plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
  legend.position = "none") 


################################################################################
# Education  
################################################################################
cohort_neat <- c("CHOP", "DNBC", "GECKO", "Gen-R", "INMA", "MoBa", "NINFEA", 
                 "Raine", "Combined")

slma.pdata$cohort

slma.pdata %<>%
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "gecko" ~ "GECKO",
      cohort == "genr" ~ "Gen-R", 
      cohort == "inma" ~ "INMA", 
      cohort == "moba" ~ "MoBa", 
      cohort == "ninfea" ~ "NINFEA",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined"), 
    cohort = factor(cohort, levels = rev(cohort_neat), ordered = TRUE))

ggplot(data = slma.pdata %>% filter(exposure == "mat_ed" & variable %in% c("edu_m2", "edu_m3")),
       aes(x = cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = cohort), size = 0.3) +
  geom_hline(aes(fill=cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Change in BMI per unit change of exposure (95% Confidence Interval)") +
  facet_grid(age ~ variable, scales = "fixed") + 
  forest_theme +
  coord_flip() 


## ---- ndvi -------------------------------------------------------------------
ggplot(data = slma.pdata %>% filter(exposure == "ndvi" & variable == "ndvi300_0_1"),
       aes(x = cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = cohort), size = 0.2) +
  geom_hline(aes(fill=cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Change in BMI per unit change of exposure (95% Confidence Interval)") +
  facet_wrap(~age, strip.position = "left", nrow = 4) + 
  geom_errorbar(aes(ymin=ci_5, ymax=ci_95,colour=cohort),width=0.2,cex=1) + 
  forest_theme +
  coord_flip() +
  ylim(-6, 4)

## ---- Pregnancy diabetes -----------------------------------------------------
ggplot(data = slma.pdata %>% filter(variable == "preg_dia1" & exposure == "preg_dia"),
       aes(x = cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = cohort), size = 0.2) +
  geom_hline(aes(fill=cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Change in BMI per unit change of exposure (95% Confidence Interval)") +
  facet_wrap(~age, strip.position = "left", nrow = 4) + 
  geom_errorbar(aes(ymin=ci_5, ymax=ci_95,colour=cohort),width=0.2,cex=1) + 
  forest_theme +
  coord_flip() +
  ylim(-1, 2)


################################################################################
# October 2020 GA sample size visualisation  
################################################################################
cohort_neat <- c("CHOP", "DNBC", "GECKO", "Gen-R", "INMA", "MoBa", "NINFEA", 
                 "Raine", "Combined")

test <- descriptives_ss$continuous %>%
  filter(variable %in% c("bmi.730", "bmi.1461", "bmi.2922", "bmi.5113", 
                         "bmi.6544", "age_months.24", "age_months.48", "age_months.96", 
                         "age_months.168", "age_months.215")) 
  
testy <- test %>% 
  separate(variable, c("var", "age"), sep = "([.])") %>% print(n = 90) %>%
  filter(var == "age_months") %>%
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "gecko" ~ "GECKO",
      cohort == "genr" ~ "Gen-R", 
      cohort == "inma" ~ "INMA", 
      cohort == "moba" ~ "MoBa", 
      cohort == "ninfea" ~ "NINFEA",
      cohort == "raine" ~ "Raine",
      cohort == "Combined" ~ "Combined"), 
    cohort = factor(cohort, levels = rev(cohort_neat), ordered = TRUE))

ggplot(data = testy, aes(x = perc_50, y = cohort, size = valid_n, colour = cohort)) +
geom_point() + 
  geom_vline(xintercept = 0, linetype=2, size = 0.3) +
  geom_vline(xintercept = 24, linetype=2, size = 0.3) +
  geom_vline(xintercept = 48, linetype=2, size = 0.3) + 
  geom_vline(xintercept = 96, linetype=2, size = 0.3) + 
  geom_vline(xintercept = 168, linetype=2, size = 0.3) +
  geom_vline(xintercept = 215, linetype=2, size = 0.3) +
  xlab("Child age (months)") +
  ylab("Cohort") +
  forest_theme + 
  theme(panel.grid.major.x = element_line(colour="white"),
        panel.grid.minor.x =element_line(colour="white"),
        axis.ticks.x = element_line(colour = "grey"), 
        legend.position = "top") +
  scale_x_continuous(limits = c(0, 215), breaks = c(0, 24, 48, 96, 168, 215), expand = c(0.01, 0))



x_axis <- scale_x_continuous(expand = c(0, 0)) # Removes space before 0
y_axis <- scale_y_continuous(expand = c(0, 0))

geom_errorbarh(
  aes(xmin = perc_5, xmax = perc_95, colour = cohort), 
  size=0.2, cex=1, height = 0.3) +

geom_rect(xmin = 0, xmax = 24, ymin = 0, ymax = Inf, size = 0, alpha = 0.002) +
  geom_rect(xmin = 48, xmax = 96, ymin = 0, ymax = Inf, size = 0, alpha = 0.002) +
  geom_rect(xmin = 168, xmax = 215, ymin = 0, ymax = Inf, size = 0, alpha = 0.002) +
