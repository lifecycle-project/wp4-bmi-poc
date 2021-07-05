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
library(ggplot2)
library(dsHelper)
library(here)
library(readr)
library(gtable)
library(gridExtra)

conns <- datashield.login(logindata, restore = "bmi_poc_sec_12")

################################################################################
# METHODS  
################################################################################
################################################################################
# List participating cohorts  
################################################################################
names(conns) %>% sort()

################################################################################
# Maximum sample size
################################################################################
original_n <- dh.getStats(
  df = "nonrep", 
  vars = "sex",
  conns = conns
)

## ---- Original dataset -------------------------------------------------------
max_n <- original_n[[1]] %>%
  filter(variable == "sex" & category == 1) %>%
  select(cohort, cohort_n)

## ---- Analysis dataset -------------------------------------------------------
cohort_ns <- descriptives[[1]] %>%
  filter(variable == "sex" & category == 1) %>%
  select(cohort, cohort_n) 

## ---- Participants with no eligible data -------------------------------------
max_n %>% filter(cohort == "combined") %>% pull(cohort_n) - 
cohort_ns %>% filter(cohort == "combined") %>% pull(cohort_n)

################################################################################
# Data prep  
################################################################################

## Make tibble of cohort names and sample sizes as I want to display in paper
ref_tab <- cohort_ns %>%
  left_join(., names_neat, by = "cohort") %>%
  mutate(
    names_neat = paste0(cohort_neat, " (n=", cohort_n, ")")) %>%
  select(-cohort_n)


################################################################################
# Table S3: Ns for complete cases  
################################################################################
miss_descriptives$categorical %>%
  mutate(variable = str_remove(variable, "_m_fact")) %>%
  separate(variable, sep = "\\.", into = c("variable", "age")) %>%
  filter(cohort == "combined" & category != "missing") %>%
  mutate(n_perc = paste0(value, " (", perc_valid, ")"))

  

select(variable, age)
           
           as.character(variable)) %>%
  str_remove(string = variable, pattern = "_m_fact")
  



################################################################################
# RESULTS  
################################################################################
################################################################################
# Table S4: Covariate descriptive statistics  
################################################################################
cov_cat.tab <- descriptives$categorical %>%
  filter(variable %in% c("sex", "parity_bin", "preg_smk", "preg_ht", 
                         "ethn3_m")) %>%
  mutate(n_perc = paste0(value, " (", perc_valid, ")")) %>%
  select(cohort, variable, category, n_perc) %>% 
  pivot_wider(
    names_from = c(variable, category),  
    values_from = n_perc)

cov_cont.tab <- descriptives$continuous %>%
  filter(variable %in% c("prepreg_bmi", "agebirth_m_y")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")"),
         missing = paste0(missing_n, " (", missing_perc, ")")) %>%
  select(cohort, variable, med_range, missing) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, missing))

cov.tab <- bind_cols(cov_cat.tab, select(cov_cont.tab, -cohort)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(names_neat, sex_1, sex_missing, parity_bin_0, parity_bin_missing, 
         ethn3_m_1, ethn3_m_2, ethn3_m_3, ethn3_m_missing, preg_smk_1, 
         preg_smk_missing, preg_ht_1, preg_ht_missing, med_range_prepreg_bmi, 
         missing_prepreg_bmi, med_range_agebirth_m_y, missing_agebirth_m_y) %>%
  arrange(names_neat) 

write_csv(cov.tab, path = here("tables", "covariates.csv"))




################################################################################
# Table 2: Outcomes descriptive statistics  
################################################################################
outcomes.tab <- descriptives$continuous %>%
  filter(variable %in% c("bmi.730", "bmi.1461", "bmi.2922", "bmi.5113", 
                         "bmi.6544", "age_months.24", "age_months.48", 
                         "age_months.96", "age_months.168", "age_months.215")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(names_neat, valid_n_bmi.730, med_range_age_months.24, med_range_bmi.730, 
         valid_n_bmi.1461, med_range_age_months.48, med_range_bmi.1461,
         valid_n_bmi.2922, med_range_age_months.96, med_range_bmi.2922,
         valid_n_bmi.5113, med_range_age_months.168, med_range_bmi.5113, 
         valid_n_bmi.6544, med_range_age_months.215, med_range_bmi.6544) %>%
  arrange(names_neat)

write_csv(outcomes.tab, path = here("tables", "outcomes.csv"))

################################################################################
# Table S5 height and weight by age to check BMI validity  
################################################################################
ht.tab <- descriptives$continuous %>%
  filter(variable %in% c("ht.730", "ht.1461", "ht.2922", "ht.5113", "ht.6544")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  left_join(., ref_tab, by = "cohort") %>%
select(names_neat, valid_n_ht.730, med_range_ht.730, valid_n_ht.1461, 
       med_range_ht.1461, valid_n_ht.2922, med_range_ht.2922, valid_n_ht.5113, 
       med_range_ht.5113, valid_n_ht.6544, med_range_ht.6544) %>%
  arrange(names_neat) 

wt.tab <- descriptives$continuous %>%
  filter(variable %in% c("wt.730", "wt.1461", "wt.2922", "wt.5113", "wt.6544")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(names_neat, valid_n_wt.730, med_range_wt.730, valid_n_wt.1461, 
         med_range_wt.1461, valid_n_wt.2922, med_range_wt.2922, valid_n_wt.5113, 
         med_range_wt.5113, valid_n_wt.6544, med_range_wt.6544) %>%
  arrange(names_neat) 
  
write_csv(ht.tab, path = here("tables", "height.csv"))
write_csv(wt.tab, path = here("tables", "weight.csv"))


################################################################################
# Table 3: Main analysis  
################################################################################
mat_ed.tab <- mat_ed.fit[[1]] %>% 
  map(dh.lmTab, type = "ipd", ci_format = "paste", direction = "wide") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) 

area_dep.tab <- area_dep.fit[[1]] %>%
  map(dh.lmTab, type = "ipd", ci_format = "paste", direction = "wide") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("area_dep2", "area_dep3")) 
  
ndvi.tab <- ndvi.fit[[1]] %>%
  map(dh.lmTab, type = "ipd", ci_format = "paste", direction = "wide") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "ndvi") 

preg_dia.tab <- preg_dia.fit[[1]] %>%
  map(dh.lmTab, type = "ipd", ci_format = "paste", direction = "wide") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "preg_dia1") 

ipd_reg.tab <- bind_rows(mat_ed.tab, area_dep.tab, ndvi.tab, preg_dia.tab)

colnames(ipd_reg.tab) <- c(
  "variable", "age_0_24", "age_25_48", "age_49_96", "age_96_168", "age_169_215")

write.csv(ipd_reg.tab)

# Need sample size for each analysis.
mat_ed_n <- mat_ed.fit[[1]] %>% map_int(function(x){x$Nvalid})
area_dep_n <- area_dep.fit[[1]] %>% map_int(function(x){x$Nvalid})
ndvi_n <- ndvi.fit[[1]] %>% map_int(function(x){x$Nvalid})
preg_dia_n <- preg_dia.fit[[1]] %>% map_int(function(x){x$Nvalid})

ipd_ns.tab <- bind_rows(mat_ed_n, area_dep_n, ndvi_n, preg_dia_n) %>%
  mutate(exposure = c("mat_ed", "area_dep", "ndvi", "preg_dia")) %>%
  select(exposure, everything())

write.csv(ipd_ns.tab)

################################################################################
# Prepare plot data  
################################################################################

## Would probably be better to generalise forestplot function to prepare data
## for IPD and SLMA.

## ---- IPD plots --------------------------------------------------------------
mat_ed_ipd.plotdata <- mat_ed.fit[[1]] %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) %>%
  mutate(
    exposure = "mat_ed", 
    variable = 
      case_when(
        variable == "edu_m2" ~ "Medium education (ref = high)", 
        variable == "edu_m3" ~ "Low education (ref = high)")) %>%
  mutate(
    variable = factor(
      variable, 
      levels = c("Medium education (ref = high)", 
                 "Low education (ref = high)"),
      ordered = TRUE), 
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  dplyr::rename(
    beta = est,
    ci_5 = lowci,
    ci_95 = uppci) %>%
  mutate(beta_si = paste0(beta, " (", ci_5, ", ", ci_95, ")"))

test <- mat_ed_ipd.plotdata %>%
  ggplot(aes(y = age)) +
  geom_text(aes(x = 2, label = age), size = 7*0.3571429) +
  geom_text(aes(x = 3, label = beta_si), size = 7*0.3571429) +
  scale_colour_identity() +
  facet_wrap(~variable, ncol = 1) +
  xlab("") +
  theme_f2 +
  scale_x_continuous(limits = c(1.5, 3.5)) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_text(colour = "white"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank())


test_2 <- mat_ed_ipd.plotdata %>%
  ggplot(aes(x = age, y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(size = 1) +
  geom_errorbar(
    size = 0.5, 
    width = 0.05) +
  xlab('') + 
  ylab("Difference in childhood BMI by category of maternal education") +
  facet_wrap(~variable, ncol = 1, scales = "fixed", strip.position = "top") + 
  theme_f2 +
  coord_flip() +
  scale_y_continuous(
    limits = c(-1, 2),
    breaks = seq(-1, 1, 1),
    expand = c(0, 0)) +
  theme(
    axis.line = element_line(),
    axis.text.y = element_blank(), 
    strip.text.x = element_text(colour = "white"), 
    strip.background = element_rect("white"))

grid.arrange(test, test_2, ncol = 2)

# Ns on plot
# squares rather than blobs
# estimates and confidence intervals
# weights
# heterogeneity

## ---- Create plot data -------------------------------------------------------
mat_ed.pdata <- list(mat_ed.fit[[2]], mat_ed.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "mat_ed")

mat_ed.pdata %>% print(n = Inf)

area_dep.pdata <- list(area_dep.fit[[2]], area_dep.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "area_dep")
  
ndvi.pdata <- list(ndvi.fit[[2]], ndvi.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "ndvi")

preg_dia.pdata <- list(preg_dia.fit[[2]], preg_dia.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "preg_dia")
  
slma.pdata <- bind_rows(mat_ed.pdata, area_dep.pdata, ndvi.pdata, preg_dia.pdata) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(-cohort) %>%
  mutate(
    age = factor(age, levels = c("bmi_24", "bmi_48", "bmi_96", "bmi_168", "bmi_215"),
                 labels = c("Age 0-24", "Age 25-48", "Age 49-96", "Age 97-168", 
                            "Age 169-215")),
    Cohort = factor(cohort_neat, levels = rev(unique(cohort_neat)), ordered = TRUE))

################################################################################
# Figure 2: Forest plots  
################################################################################

age_orig <- c("bmi_24", "bmi_48", "bmi_96", "bmi_168", "bmi_215")
age_new <- c("0-24 months", "25-48 months", "49-96 months", "97-168 months", 
             "169-215 months")

## ---- Prepare data -----------------------------------------------------------
mat_ed_ipd <- mat_ed.fit[[1]] %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate") %>%
  bind_rows(.id = "age") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) %>%
  mutate(
    type = "IPD", 
    cohort = "combined", 
    exposure = "mat_ed") %>%
  dplyr::rename(
    beta = est,
    ci_5 = lower,
    ci_95 = upper)

mat_ed_slma <- mat_ed.pdata %>% mutate(type = "SLMA")

mat_ed_comb <- bind_rows(mat_ed_ipd, mat_ed_slma) %>% select(-se)

mat_ed_comb.pdata <- mat_ed_comb %>% 
  filter(exposure == "mat_ed" & variable %in% c("edu_m2", "edu_m3") & cohort == "combined") %>%
  mutate(  
    variable = case_when(
      variable == "edu_m2" ~ "Medium education (ref = high)", 
      variable == "edu_m3" ~ "Low education (ref = high)"),
    variable = factor(
      variable, 
      levels = c("Medium education (ref = high)", 
                 "Low education (ref = high)"),
      ordered = TRUE), 
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)
  )


################################################################################
# Figure S1: maternal education SLMA
################################################################################
source('~/useful-code-r/code/themes/forest-theme.R')
palette_n <- c("#ff2600", rep("#005690", 10))

## ---- Prepare data -----------------------------------------------------------
mat_ed.pdata <- slma.pdata %>% 
  filter(exposure == "mat_ed" & variable %in% c("edu_m2", "edu_m3")) %>%
  mutate(  
    variable = case_when(
      variable == "edu_m2" ~ "Medium education (ref = high)", 
      variable == "edu_m3" ~ "Low education (ref = high)"),
    variable = factor(
      variable, 
      levels = c("Medium education (ref = high)", 
                 "Low education (ref = high)"),
      ordered = TRUE)
  )

## ---- Plot -------------------------------------------------------------------
mat_ed.plot <- ggplot(data = mat_ed.pdata, aes(x = Cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = Cohort), size = 0.15) +
  geom_hline(aes(fill=Cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Difference in childhood BMI by category of maternal education") +
  facet_grid(age ~ variable, scales = "fixed", switch = "y") + 
  forest_theme +
  coord_flip() +
  scale_colour_manual(values = palette_n) +
  theme(
    axis.text.y = element_text(
      family="ArialMT", 
      size=6, 
      margin = margin(t = 0, r=4, b=0, l=0), 
      colour="black")
  )

## ---- Save plot --------------------------------------------------------------
ggsave(
  filename="./figures/mat_ed.png", 
  plot = mat_ed.plot,
  h = 25, w = 15.92, units="cm", dpi=1200,
  device="png")


################################################################################
# Figure S2: Area deprivation SLMA  
################################################################################

## ---- Prepare data -----------------------------------------------------------
area_dep.pdata <- slma.pdata %>% 
  filter(exposure == "area_dep" & variable %in% c("area_dep2", "area_dep3")) %>%
  mutate(
    variable = factor(variable, levels = c("area_dep2", "area_dep3"),
                      labels = c("Medium deprivation (ref = low)", 
                                 "High deprivation (ref = low)")))

## ---- Plot -------------------------------------------------------------------
area_dep.plot <- ggplot(data = area_dep.pdata,
                    aes(x = Cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = Cohort), size = 0.3) +
  geom_hline(aes(fill=Cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Difference in childhood BMI by category of area deprivation") +
  facet_grid(age ~ variable, scales = "fixed", switch = "y") + 
  forest_theme +
  coord_flip() +
  ylim(-2, 2) +
  scale_colour_manual(values = palette_n)

## ---- Save plot --------------------------------------------------------------
ggsave(
  filename="./figures/area_dep.png", 
  plot = area_dep.plot, 
  h = 12, w = 15.92, units="cm", dpi=1200,
  device="png")


################################################################################
# Figure S3: NDVI  
################################################################################

## ---- Prepare data -----------------------------------------------------------
ndvi.pdata <- slma.pdata %>% filter(exposure == "ndvi" & variable == "ndvi") 

## ---- Plot -------------------------------------------------------------------
ndvi.plot <- ggplot(data = ndvi.pdata,
       aes(x = Cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = Cohort), size = 0.3) +
  geom_hline(aes(fill=Cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Difference in childhood BMI by unit change in NDVI") +
  facet_wrap(~age, strip.position = "left", nrow = 4) + 
  forest_theme +
  coord_flip() +
  ylim(-6, 4) +
  scale_colour_manual(values = palette_n)

## ---- Save plot --------------------------------------------------------------
ggsave(
  filename="./figures/ndvi.png", 
  plot = ndvi.plot, 
  h = 12, w = 15.92, units="cm", dpi=1200,
  device="png")


################################################################################
# Figure S4: Gestational diabetes  
################################################################################

## ---- Prepare data -----------------------------------------------------------
preg_dia.pdata <- slma.pdata %>% 
  filter(variable == "preg_dia1" & exposure == "preg_dia")
  
## ---- Plot -------------------------------------------------------------------
preg_dia.plot <- ggplot(data = preg_dia.pdata,
       aes(x = Cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = Cohort), size = 0.2) +
  geom_hline(aes(fill=Cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Difference in chilhood BMI where gestational diabetes present") +
  facet_wrap(~age, strip.position = "left", nrow = 4) + 
  forest_theme +
  coord_flip() +
  ylim(-1, 2) +
  scale_colour_manual(values = palette_n)

## ---- Save plot --------------------------------------------------------------
ggsave(
  filename="./figures/preg_dia.png", 
  plot = preg_dia.plot, 
  h = 20, w = 15.92, units="cm", dpi=1200,
  device="png")

################################################################################
# SENSITIVITY ANALYSES
################################################################################
################################################################################
# Sex interactions
################################################################################
mat_ed_int.tab <- mat_ed_int.fit[[1]] %>% 
  map(dh.glmTab, type = "ipd")

area_dep_int.tab <- area_dep_int.fit[[1]] %>%
  map(dh.glmTab, type = "ipd")

ndvi_int.tab <- ndvi_int.fit[[1]] %>%
  map(dh.glmTab, type = "ipd")

preg_dia_int.tab <- preg_dia_int.fit[[1]] %>%
  map(dh.glmTab, type = "ipd")

################################################################################
# Removeing DNBC and MoBa  
################################################################################

## ---- Table of coefficients --------------------------------------------------
mat_ed_remove.tab <- mat_ed_remove.fit[[1]] %>% 
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) 

area_dep_remove.tab <- area_dep_remove.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("area_dep2", "area_dep3")) 

ndvi_remove.tab <- ndvi_remove.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "ndvi") 

preg_dia_remove.tab <- preg_dia_remove.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "preg_dia1") 

ipd_remove.tab <- bind_rows(mat_ed_remove.tab, area_dep_remove.tab, 
                            ndvi_remove.tab, preg_dia_remove.tab)

colnames(ipd_remove.tab) <- c(
  "variable", "age_0_24", "age_25_48", "age_49_96", "age_96_168", "age_169_215")

write.csv(ipd_remove.tab)


## ---- Sample sizes -----------------------------------------------------------
mat_ed_remove_n <- mat_ed_remove.fit[[1]] %>% map_int(function(x){x$Nvalid})
area_dep_remove_n <- area_dep_remove.fit[[1]] %>% map_int(function(x){x$Nvalid})
ndvi_remove_n <- ndvi_remove.fit[[1]] %>% map_int(function(x){x$Nvalid})
preg_dia_remove_n <- preg_dia_remove.fit[[1]] %>% map_int(function(x){x$Nvalid})

ipd_remove_ns.tab <- bind_rows(mat_ed_remove_n, area_dep_remove_n, 
                               ndvi_remove_n, preg_dia_remove_n) %>%
  mutate(exposure = c("mat_ed", "area_dep", "ndvi", "preg_dia")) %>%
  select(exposure, everything())

write.csv(ipd_remove_ns.tab)


################################################################################
# PRESENTATIONS
################################################################################

################################################################################
# Prepare dataframe  
################################################################################
cohort_neat <- c("CHOP", "DNBC", "ELFE", "GECKO", "Gen-R", "INMA", "MoBa", 
                 "NFBC86", "NINFEA", "Raine", "SWS", "Combined")

plot_data_cont <- descriptives$continuous %>% 
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "elfe" ~ "ELFE",
      cohort == "gecko" ~ "GECKO",
      cohort == "genr" ~ "Gen-R", 
      cohort == "inma" ~ "INMA", 
      cohort == "moba" ~ "MoBa", 
      cohort == "nfbc86" ~ "NFBC86", 
      cohort == "ninfea" ~ "NINFEA",
      cohort == "raine" ~ "Raine",
      cohort == "sws" ~ "SWS",
      cohort == "combined" ~ "Combined"), 
    cohort = factor(cohort, levels = rev(cohort_neat), ordered = TRUE))

plot_data_cat <- descriptives$categorical %>% 
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "elfe" ~ "ELFE",
      cohort == "gecko" ~ "GECKO",
      cohort == "genr" ~ "Gen-R", 
      cohort == "inma" ~ "INMA", 
      cohort == "moba" ~ "MoBa", 
      cohort == "nfbc86" ~ "NFBC86", 
      cohort == "ninfea" ~ "NINFEA",
      cohort == "raine" ~ "Raine",
      cohort == "sws" ~ "SWS",
      cohort == "combined" ~ "Combined"), 
    cohort = factor(cohort, levels = rev(cohort_neat), ordered = TRUE))

################################################################################
# Sample size visualisation  
################################################################################
sample_data <- plot_data_cont %>%
  filter(variable %in% c("bmi.730", "bmi.1461", "bmi.2922", "bmi.5113", 
                         "bmi.6544", "age_months.24", "age_months.48", "age_months.96", 
                         "age_months.168", "age_months.215")) %>%
  separate(variable, c("var", "age"), sep = "([.])") %>% print(n = 90) %>%
  filter(var == "age_months") 
  
palette_samp <- c("#ff2600", rep("#005690", 11))

sample.plot <- ggplot(data = sample_data, aes(x = perc_50, y = cohort, size = valid_n, colour = cohort)) +
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
        legend.position = "none", 
        legend.title = element_blank(), 
        legend.justification='left') +
  scale_x_continuous(
    limits = c(0, 215), 
    breaks = c(0, 24, 48, 96, 168, 215), 
    expand = c(0.01, 0)) +
  scale_colour_manual(values = palette_samp, guide = FALSE) 

ggsave(
  filename = "~/wp4-bmi-poc/figures/sample_n_mar21.png", 
  plot = sample.plot, 
  dpi = 1200, 
  h = 12,
  w = 20,
  units = "cm",
  device="png")

################################################################################
# Exposures  
################################################################################
coolors <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51", "#9e7682", 
             "#aba9c3", "#8b2635", "#ec9192", "#553739")

## ---- NDVI -------------------------------------------------------------------
ndvi_plotdata <- plot_data_cont %>%
  filter(variable == "ndvi" & !is.na(mean) & cohort != "Combined")

ndvi.plot <- ndvi_plotdata %>%
  ggplot(aes(x = variable, y = mean, fill = cohort)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cohort, ncol = 3) +
  theme_traj +
  labs(x = "", y = "", title = "") +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey"), 
        axis.ticks.length = unit(0, "cm"), 
        plot.margin=unit(c(-0.3, 0.5, 0, -0.3),"cm")) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.5),
    expand = c(0, 0)) +
  scale_fill_manual(values = coolors)

ggsave(
  filename = "~/wp4-bmi-poc/figures/ndvi.png", 
  plot = ndvi.plot,
  h = 4, w = 10, units="cm", dpi=1200,
  device="png")


## ---- Area deprivation -------------------------------------------------------
area_plotdata <- plot_data_cat %>%
  filter(variable == "area_dep" & !is.na(valid_perc) & cohort != "Combined")

area.plot <- area_plotdata %>%
  ggplot(aes(x = category, y = valid_perc, fill = cohort)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cohort, ncol = 5) +
  theme_traj +
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(colour="grey"),        
        plot.margin=unit(c(0.3, 0.5, -0.5, -0.3),"cm")) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 20), expand = c(0, 0)) +
  scale_fill_manual(values = coolors)

ggsave(
  filename = "~/wp4-bmi-poc/figures/area.png", 
  plot = area.plot,
  h = 4, w = 10, units="cm", dpi=1200,
  device="png")

## ---- Maternal education -----------------------------------------------------
mated_plotdata <- plot_data_cat %>%
  filter(variable == "edu_m" & !is.na(mean) & cohort != "Combined" & cohort != "SWS")

mated.plot <- mated_plotdata %>%
  ggplot(aes(x = category, y = valid_perc, fill = cohort)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cohort, ncol = 5) +
  theme_traj +
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(colour="grey"),
        plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm")) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(
    limits = c(0, 80), breaks = seq(0, 80, 20), expand = c(0, 0)) +
  scale_fill_manual(values = coolors)

ggsave(
  filename = "~/wp4-bmi-poc/figures/mated.png", 
  plot = mated.plot,
  h = 9, w = 20, units="cm", dpi=1200,
  device="png")

## ---- Gestational diabetes ---------------------------------------------------
pregdia_plotdata <- plot_data_cat %>%
  filter(variable == "preg_dia" & valid_n > 0 & cohort != "Combined" & category == 1)

pregdia.plot <- pregdia_plotdata %>%
  ggplot(aes(x = variable, y = valid_perc, fill = cohort)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cohort, ncol = 9) +
  theme_traj +
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey"),
        axis.ticks.length = unit(0, "cm"), 
        plot.margin=unit(c(0.3, 0.5, 0, -0.3),"cm")) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(
    limits = c(0, 16),
    breaks = seq(0, 16, 4),
    expand = c(0, 0)) +
  scale_fill_manual(values = coolors)

ggsave(
  filename = "~/wp4-bmi-poc/figures/pregdia.png", 
  plot = pregdia.plot,
  h = 4, w = 22, units="cm", dpi=1200,
  device="png")

################################################################################
# Forest plots with SLMA and IPD combined  
################################################################################
################################################################################
# Maternal education  
################################################################################
age_orig <- c("bmi_24", "bmi_48", "bmi_96", "bmi_168", "bmi_215")
age_new <- c("0-24 months", "25-48 months", "49-96 months", "97-168 months", 
             "169-215 months")
  
## ---- Prepare data -----------------------------------------------------------
mat_ed_ipd <- mat_ed.fit[[1]] %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate") %>%
  bind_rows(.id = "age") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) %>%
  mutate(
    type = "IPD", 
    cohort = "combined", 
    exposure = "mat_ed") %>%
  dplyr::rename(
    beta = est,
    ci_5 = lower,
    ci_95 = upper)

mat_ed_slma <- mat_ed.pdata %>% mutate(type = "SLMA")

mat_ed_comb <- bind_rows(mat_ed_ipd, mat_ed_slma) %>% select(-se)

mat_ed_comb.pdata <- mat_ed_comb %>% 
  filter(exposure == "mat_ed" & variable %in% c("edu_m2", "edu_m3") & cohort == "combined") %>%
  mutate(  
    variable = case_when(
      variable == "edu_m2" ~ "Medium education (ref = high)", 
      variable == "edu_m3" ~ "Low education (ref = high)"),
    variable = factor(
      variable, 
      levels = c("Medium education (ref = high)", 
                 "Low education (ref = high)"),
      ordered = TRUE), 
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)
  )

beta, lower_ci, upper_ci, estimate_type

## ---- Plot -------------------------------------------------------------------
mat_ed_comb.plot <- ggplot(data = mat_ed_comb.pdata, aes(x = age, y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(
    aes(colour = type), size = 2.5, 
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  geom_errorbar(
    aes(colour = type),
      size = 0.5, 
      width = 0.1,
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  xlab('Child age') + 
  ylab("Difference in childhood BMI by category of maternal education") +
  facet_wrap(~variable, scales = "fixed", strip.position = "top") + 
  forest_theme +
  coord_flip() +
  theme(legend.title = element_blank(), 
        legend.position = "none") + 
  scale_y_continuous(
    limits = c(-1, 2),
    breaks = seq(-1, 2, 1),
    expand = c(0, 0)) +
  scale_colour_manual(values = coolors[c(1, 5)])


################################################################################
# NDVI  
################################################################################

## ---- Prepare data -----------------------------------------------------------
ndvi_ipd <- ndvi.fit[[1]] %>%
  map(dh.glmTab, type = "ipd", format = "separate") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "ndvi") %>%
  mutate(
    type = "IPD", 
    cohort = "combined", 
    exposure = "ndvi") %>%
  dplyr::rename(
    beta = est,
    ci_5 = lower,
    ci_95 = upper)

ndvi_slma <- ndvi.pdata %>% mutate(type = "SLMA")

ndvi_comb <- bind_rows(ndvi_ipd, ndvi_slma) %>% select(-se)

ndvi_comb.pdata <- ndvi_comb %>% 
  filter(exposure == "ndvi" & variable == "ndvi" & cohort == "combined") %>%
  mutate(  
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new), 
      ordered = TRUE),
    variable = "NDVI"
  )

## ---- Plot -------------------------------------------------------------------
ndvi_comb.plot <- ggplot(data = ndvi_comb.pdata, aes(x = age, y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(
    aes(colour = type), size = 2.5, 
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  geom_errorbar(
    aes(colour = type),
    size = 0.5, 
    width = 0.1,
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  xlab('Child age') + 
  ylab("Difference in childhood BMI by unit change in NDVI") +
  forest_theme +
  coord_flip() +
  theme(legend.title = element_blank(), 
        legend.position = "none") + 
  scale_y_continuous(
    limits = c(-1, 2),
    breaks = seq(-1, 2, 1),
    expand = c(0, 0)) +
  scale_colour_manual(values = coolors[c(1, 5)])


################################################################################
# Area deprivation  
################################################################################

## ---- Prepare data -----------------------------------------------------------
area_ipd <- area_dep.fit[[1]] %>%
  map(dh.glmTab, type = "ipd", format = "separate") %>%
  bind_rows(.id = "age") %>%
  filter(variable %in% c("area_dep2", "area_dep3")) %>%
  mutate(
    type = "IPD", 
    cohort = "combined", 
    exposure = "area_dep") %>%
  dplyr::rename(
    beta = est,
    ci_5 = lower,
    ci_95 = upper)

area_slma <- area_dep.pdata %>% mutate(type = "SLMA")

area_comb <- bind_rows(area_ipd, area_slma) %>% select(-se)

area_comb.pdata <- area_comb %>% 
  filter(exposure == "area_dep" & variable %in% c("area_dep2", "area_dep3") & cohort == "combined") %>%
  mutate(  
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new), 
      ordered = TRUE), 
    variable = case_when(
      variable == "area_dep2" ~ "Medium area deprivation (ref = low)", 
      variable == "area_dep3" ~ "High area deprivation (ref = low)"),
    variable = factor(
      variable, 
      levels = c("Medium area deprivation (ref = low)", 
                 "High area deprivation (ref = low)"),
        ordered = TRUE))

## ---- Plot -------------------------------------------------------------------
area_comb.plot <- ggplot(data = area_comb.pdata, aes(x = age, y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(
    aes(colour = type), size = 2.5, 
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  geom_errorbar(
    aes(colour = type),
    size = 0.5, 
    width = 0.1,
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  xlab('Child age') + 
  ylab("Difference in childhood BMI by category of area deprivation") +
  labs(colour = "Type") +
  facet_wrap(~variable, scales = "fixed", strip.position = "top") + 
  forest_theme +
  coord_flip() +
  theme(legend.title = element_blank(),
         legend.position = "none") + 
  scale_y_continuous(
    limits = c(-1, 2),
    breaks = seq(-1, 2, 1),
    expand = c(0, 0)) +
  scale_colour_manual(values = coolors[c(1, 5)])

################################################################################
# Gestational diabetes  
################################################################################

## ---- Prepare data -----------------------------------------------------------
dia_ipd <- preg_dia.fit[[1]] %>%
  map(dh.glmTab, type = "ipd", format = "separate") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "preg_dia1") %>%
  mutate(
    type = "IPD", 
    cohort = "combined", 
    exposure = "preg_dia") %>%
  dplyr::rename(
    beta = est,
    ci_5 = lower,
    ci_95 = upper)

dia_slma <- preg_dia.pdata %>% mutate(type = "SLMA")

dia_comb <- bind_rows(dia_ipd, dia_slma) %>% select(-se)

dia_comb.pdata <- dia_comb %>% 
  filter(exposure == "preg_dia" & variable == "preg_dia1" & cohort == "combined") %>%
  mutate(  
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new), 
      ordered = TRUE), 
    variable = "Presence of gestational diabetes"
  )

## ---- Plot -------------------------------------------------------------------
dia_comb.plot <- ggplot(data = dia_comb.pdata, aes(x = age, y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(
    aes(colour = type), size = 2.5, 
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  geom_errorbar(
    aes(colour = type),
    size = 0.5, 
    width = 0.1,
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  xlab('Child age') + 
  ylab("Difference in chilhood BMI where gestational diabetes present") +
  forest_theme +
  coord_flip() +
  theme(legend.title = element_blank(),
        legend.position = "none") + 
  scale_y_continuous(
    limits = c(-1, 2),
    breaks = seq(-1, 2, 1),
    expand = c(0, 0)) +
  scale_colour_manual(values = coolors[c(1, 5)])

################################################################################
# Save plots  
################################################################################
ggsave(
  filename = "~/wp4-bmi-poc/figures/mat_ed_comb.png", 
  plot = mat_ed_comb.plot,
  h = 10, w = 16, units="cm", dpi=1200,
  device="png")

ggsave(
  filename = "~/wp4-bmi-poc/figures/ndvi_comb.png", 
  plot = ndvi_comb.plot,
  h = 10, w = 16, units="cm", dpi=1200,
  device="png")

ggsave(
  filename = "~/wp4-bmi-poc/figures/area_comb.png", 
  plot = area_comb.plot,
  h = 10, w = 16, units="cm", dpi=1200,
  device="png")

ggsave(
  filename = "~/wp4-bmi-poc/figures/dia_comb.png", 
  plot = dia_comb.plot,
  h = 10, w = 16, units="cm", dpi=1200,
  device="png")


