################################################################################
## Project: bmi-poc
## Script purpose: Figures for manuscript
## Date: 1st July 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(rlang)
library(tidyr)
library(ggplot2)
library(metafor)
library(gridExtra)
library(here)

source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")

################################################################################
# Figure S1: missingness models
################################################################################
################################################################################
# Helper functions 
################################################################################

## ---- Function to do prepare data --------------------------------------------
formatMissPlot <- function(x){
  
  x %>%
    map(
      dh.lmTab, 
      type = "ipd", 
      ci_format = "separate", 
      direction = "wide", 
      round = 50) %>%
    bind_rows(.id = "age") %>%
    filter(str_detect(variable, "bmi\\.")) %>%
    mutate(across(est:uppci, ~exp(.x))) %>%
    mutate(
      age = 
        factor(
          case_when(
            age == "bmi_24" ~ "0 - 24", 
            age == "bmi_48" ~ "25 - 48", 
            age == "bmi_96" ~ "49 - 96", 
            age == "bmi_168" ~ "97 - 168", 
            age == "bmi_215" ~ "169 - 215"), 
          levels = c("0 - 24", "25 - 48", "49 - 96", "97 - 168", "169 - 215"), 
          ordered = TRUE))
}

## ---- Wrapper function for forest plot ---------------------------------------
forestWrap <- function(
  x, title, x_limits, axis_limits, type = c("logistic", "linear"), digits = 3){
 
  ref_value <- ifelse(type == "logistic", 1, 0)
  
  forest(
    x = x %>% pull(est), 
    ci.lb = x %>% pull(lowci),
    ci.ub = x %>% pull(uppci), 
    slab = x %>% pull(age), 
    xlab = title, 
    cex = 0.8, 
    cex.axis = 0.8,
    header = c("BMI Age period", "Estimate [95% CI]"), 
    refline = ref_value, 
    xlim = x_limits,
    alim = axis_limits, 
    steps = 5, 
    digits = c(digits, 2))
}

################################################################################
# Plots  
################################################################################

## ---- Maternal education -----------------------------------------------------
png(
  file = here("figures", "mat_ed_miss_forest.png"), 
  width = word_land_half, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,4,0,2))

mat_ed_miss.fit %>% 
  formatMissPlot %>%
  forestWrap(
    title = "Odds ratio for being complete cases per unit change in BMI",
    x_limits = c(0.6, 1.4), 
    axis_limits = c(0.8, 1.2), 
    type = "logistic"
  )

dev.off()

## ---- Area deprivation -------------------------------------------------------
png(
  file = here("figures", "area_dep_miss_forest.png"), 
  width = word_land_half, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,4,0,2))

area_dep_miss.fit %>%
  formatMissPlot %>%
  forestWrap(
    title = "Odds ratio for being complete cases per unit change in BMI",
    x_limits = c(0.6, 1.4), 
    axis_limits = c(0.8, 1.2), 
    type = "logistic"
  )

dev.off()

## ---- NDVI -------------------------------------------------------------------
png(
  file = here("figures", "ndvi_miss_forest.png"), 
  width = word_land_half, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,4,0,2))

ndvi_miss.fit %>%
  formatMissPlot %>%
  forestWrap(
    title = "Odds ratio for being complete cases per unit change in BMI",
    x_limits = c(0.6, 1.4), 
    axis_limits = c(0.8, 1.2), 
    type = "logistic"
  )

dev.off()

## ---- Pregnancy Diabetes -----------------------------------------------------
png(
  file = here("figures", "preg_dia_forest.png"), 
  width = word_land_half, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,4,0,2))

preg_dia_miss.fit %>%
  formatMissPlot %>%
  forestWrap(
    title = "Odds ratio for being complete cases per unit change in BMI",
    x_limits = c(0.6, 1.4), 
    axis_limits = c(0.8, 1.2), 
    type = "logistic"
  )

dev.off()

################################################################################
# Figure 1: Exposures descriptive statistics  
################################################################################

## ---- Maternal education -----------------------------------------------------
edu_desc.plot <- descriptives$categorical %>%
  filter(variable == "edu_m" & !cohort == "combined") %>%
  mutate(
    category = 
      factor(
        case_when(
          category == 1 ~ "High", 
          category == 2 ~ "Medium", 
          category == 3 ~ "Low", 
          category == "missing" ~ "Missing"), 
        levels = c("Low", "Medium", "High", "Missing"), 
        ordered = TRUE)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  mutate(perc_total = ifelse(perc_total == 100, 0, perc_total)) %>%
  ggplot(aes(x = cohort_neat, y = perc_total, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Percentage") +
  coord_cartesian(
    ylim = c(0, 100), 
    expand = FALSE) +
  scale_fill_manual(values = palette_std) +
  theme_std +  
  theme_word + 
  theme_bar_stack


## ---- Area deprivation -------------------------------------------------------
area_dep_desc.plot <- descriptives$categorical %>%
  filter(
    variable == "area_dep" & !cohort == "combined") %>%
  mutate(
    category = 
      factor(
        case_when(
          category == 1 ~ "Low", 
          category == 2 ~ "Medium", 
          category == 3 ~ "High", 
          category == "missing" ~ "Missing"), 
        levels = c("Low", "Medium", "High", "Missing"), 
        ordered = TRUE)) %>%
  mutate(perc_total = ifelse(perc_total == 100, 0, perc_total)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  ggplot(aes(x = cohort_neat, y = perc_total, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Percentage") +
  coord_cartesian(
    ylim = c(0, 100), 
    expand = FALSE) +
  scale_fill_manual(values = palette_std) +
  theme_std +  
  theme_word + 
  theme_bar_stack

## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_desc.plot <- descriptives$categorical %>%
  filter(
    variable == "preg_dia" & !cohort == "combined") %>%
  mutate(
    category = 
      factor(
        case_when(
          category == 0 ~ "No", 
          category == 1 ~ "Yes", 
          category == "missing" ~ "Missing"), 
        levels = c("No", "Yes", "Missing"), 
        ordered = TRUE)) %>%
  mutate(perc_total = ifelse(perc_total == 100, 0, perc_total)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  ggplot(aes(x = cohort_neat, y = perc_total, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Percentage") +
  coord_cartesian(
    ylim = c(0, 100), 
    expand = FALSE) +
  scale_fill_manual(values = palette_std) +
  theme_std +  
  theme_word + 
  theme_bar_stack

## ---- NDVI -------------------------------------------------------------------
ndvi_desc.plot <- descriptives$continuous %>%
  filter(
    variable == "ndvi" & !cohort == "combined") %>%
  left_join(., ref_tab, by = "cohort") %>%
  ggplot(aes(x = cohort_neat, y = value)) +
  geom_boxplot(
    aes(
      ymin = perc_5, 
      lower = perc_25, 
      middle = perc_50, 
      upper = perc_75, 
      ymax = perc_95), 
    stat = "identity") +
  ylab("NDVI") +
  coord_cartesian(
    ylim = c(0, 1), 
    expand = FALSE) +
  theme_std + 
  theme_word + 
  theme_bar_stack

## ---- Save plots -------------------------------------------------------------
ggsave(
  plot = edu_desc.plot,
  filename = here("figures", "edu_desc.png"),
  h = 6, w = word_half, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = area_dep_desc.plot,
  filename = here("figures", "area_dep_desc.png"),
  h = 6, w = word_half, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = preg_dia_desc.plot,
  filename = here("figures", "preg_dia_desc.png"),
  h = 6, w = word_half, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = ndvi_desc.plot,
  filename = here("figures", "ndvi_desc.png"),
  h = 6, w = word_half,
  units="cm", 
  dpi=1200, type="cairo")


################################################################################
# Figures 2-6: main results  
################################################################################
################################################################################
# Prepare plot data  
################################################################################
age_orig <- c("bmi_24", "bmi_48", "bmi_96", "bmi_168", "bmi_215")
age_new <- c("0-24", "25-48", "49-96", "97-168", "169-215")

## ---- Function ---------------------------------------------------------------
prepCatPlotdata <- function(
  x, lev_1_name = NULL, lev_1_lab = NULL, lev_2_name = NULL, 
  lev_2_lab = NULL){
  
  x %>%
    filter(variable %in% c(lev_1_name, lev_2_name)) %>%
    mutate(
      age = factor(
        age, 
        levels = rev(age_orig),
        labels = rev(age_new),
        ordered = TRUE)) %>%
  mutate(
    variable = 
      case_when(
        variable == lev_1_name ~ lev_1_lab, 
        variable == lev_2_name ~ lev_2_lab)) %>%
  mutate(
    variable = factor(
      variable, 
      levels = c(lev_1_lab, 
                 lev_2_lab),
      ordered = TRUE))

}
  
## ---- Maternal education -----------------------------------------------------
mat_ed_ipd.plotdata <- mat_ed.fit$ipd %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  arrange(desc(variable), desc(age))

## ---- Area deprivation -------------------------------------------------------
area_dep_ipd.plotdata <- area_dep.fit$ipd %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  arrange(desc(variable), desc(age))

## ---- NDVI -------------------------------------------------------------------
ndvi_ipd.plotdata <- ndvi.fit$ipd %>% 
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "ndvi") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(variable = "NDVI")

## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_ipd.plotdata <- preg_dia.fit$ipd %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "preg_dia1") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(variable = "Pregnancy diabetes reported")

################################################################################
# Plots  
################################################################################

## ---- Maternal education -----------------------------------------------------
png(
  file = here("figures", "mat_ed_ipd_forest.png"), 
  width = word_full, 
  height = 12, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_ipd.plotdata %>% pull(est), 
  ci.lb = mat_ed_ipd.plotdata %>% pull(lowci),
  ci.ub = mat_ed_ipd.plotdata %>% pull(uppci), 
  slab = mat_ed_ipd.plotdata %>% pull(age), 
  xlab = "Difference in childhood BMI by category of maternal education", 
  ilab =  cbind(
    rep(mat_ed.fit[[1]] %>% map_int(function(x){x$Nvalid}), 2),
    mat_ed.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})),
  ilab.xpos = c(-2, -1.2),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(1, 24),
  alim = c(-1, 1.5), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(8, 6.5, 5, 3.5, 2, 18.5, 17, 15.5, 14, 12.5))

text(-2, 23, "N subjects", cex = 0.8, font = 2)
text(-1.2, 23, "N studies", cex = 0.8, font = 2)

text(-2.92, 20.3, "Medium education", cex = 0.8, font = 2)
text(-3.02, 9.8, "Low education", cex = 0.8, font = 2)

dev.off()


## ---- Area deprivation -------------------------------------------------------
png(
  file = here("figures", "area_dep_ipd_forest.png"), 
  width = word_full, 
  height = 12, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_ipd.plotdata %>% pull(est), 
  ci.lb = area_dep_ipd.plotdata %>% pull(lowci),
  ci.ub = area_dep_ipd.plotdata %>% pull(uppci), 
  slab = area_dep_ipd.plotdata %>% pull(age), 
  xlab = "Difference in childhood BMI by category of area deprivation", 
  ilab =  cbind(
    rep(area_dep.fit[[1]] %>% map_int(function(x){x$Nvalid}), 2),
    area_dep.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})),
  ilab.xpos = c(-2, -1.2),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(2, 22),
  alim = c(-1, 1.5), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(8, 6.5, 5, 3.5, 17, 15.5, 14, 12.5))

text(-2, 21, "N subjects", cex = 0.8, font = 2)
text(-1.2, 21, "N studies", cex = 0.8, font = 2)

text(-2.88, 18.5, "Medium deprivation", cex = 0.8, font = 2)
text(-2.96, 9.5, "High deprivation", cex = 0.8, font = 2)

dev.off()

## ---- NDVI -------------------------------------------------------------------
png(
  file = here("figures", "ndvi_ipd_forest.png"), 
  width = word_full, 
  height = 7, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = ndvi_ipd.plotdata %>% pull(est), 
  ci.lb = ndvi_ipd.plotdata %>% pull(lowci),
  ci.ub = ndvi_ipd.plotdata %>% pull(uppci), 
  slab = ndvi_ipd.plotdata %>% pull(age), 
  xlab = "Difference in childhood BMI by one unit change NDVI", 
  ilab =  cbind(
    ndvi.fit[[1]] %>% map_int(function(x){x$Nvalid}),
    ndvi.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})),
  ilab.xpos = c(-2.5, -1.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(13, 20),
  alim = c(-1.5, 1), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(17, 16, 15, 14))

text(-2.5, 19, "N subjects", cex = 0.8, font = 2)
text(-1.7, 19, "N studies", cex = 0.8, font = 2)

dev.off()

## ---- Pregnancy diabetes -----------------------------------------------------
png(
  file = here("figures", "preg_dia_ipd_forest.png"), 
  width = word_full, 
  height = 7, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = preg_dia_ipd.plotdata %>% pull(est), 
  ci.lb = preg_dia_ipd.plotdata %>% pull(lowci),
  ci.ub = preg_dia_ipd.plotdata %>% pull(uppci), 
  slab = preg_dia_ipd.plotdata %>% pull(age), 
  xlab = "Difference in childhood BMI where pregnancy diabetes reported", 
  ilab =  cbind(
    preg_dia.fit[[1]] %>% map_int(function(x){x$Nvalid}),
    preg_dia.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})),
  ilab.xpos = c(-2.5, -1.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(13, 20),
  alim = c(-1.5, 1), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(17, 16, 15, 14))

text(-2.5, 19, "N subjects", cex = 0.8, font = 2)
text(-1.7, 19, "N studies", cex = 0.8, font = 2)

dev.off()

################################################################################
# Supplementary figures S2 - S5: SLMA forest plots
################################################################################
################################################################################
# Prepare data  
################################################################################

## ---- Function ---------------------------------------------------------------
forestDataSLMA <- function(obj, mod){
  
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

## ---- Maternal education -----------------------------------------------------
mat_ed_slma.plotdata <- list(mat_ed.fit$slma, mat_ed.mod) %>% 
  pmap(forestData) %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  arrange(desc(variable), desc(age)) %>%
  left_join(., ref_tab, by = "cohort")


## ---- Area deprivation -------------------------------------------------------
area_dep_slma.plotdata <- list(area_dep.fit$slma, area_dep.mod) %>% 
  pmap(forestData) %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  arrange(desc(variable), desc(age)) %>%
  left_join(., ref_tab, by = "cohort")


## ---- NDVI -------------------------------------------------------------------
ndvi_slma.plotdata <-list(ndvi.fit$slma, ndvi.mod) %>% 
  pmap(forestData) %>%
  bind_rows(.id = "age") %>%
  filter(variable == "ndvi") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(variable = "NDVI") %>%
  left_join(., ref_tab, by = "cohort")


## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_slma.plotdata <-list(preg_dia.fit$slma, preg_dia.mod) %>% 
  pmap(forestData) %>%
  bind_rows(.id = "age") %>%
  filter(variable == "preg_dia1") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(variable = "Pregnancy diabetes reported") %>%
  left_join(., ref_tab, by = "cohort")

################################################################################
# Plots  
################################################################################
slmaN <- function(x){ 
  
  n_study <- paste0("study", 1:dim(x$betamatrix.valid)[2])
  
  out <- n_study %>% map_int(function(y){x$output.summary[[y]][["Nvalid"]]})
  out <- c(out, sum(out))}

################################################################################
# Figure S3a & S3b: Maternal education SLMA
################################################################################
k_age_med <- mat_ed_slma_med.plotdata %>% 
  group_by(age) %>%
  summarise(n = n()) %>%
  pull(n) 

## ---- Medium vs high ---------------------------------------------------------
mat_ed_slma_med.plotdata <- mat_ed_slma.plotdata %>% 
  filter(variable == "Medium education (ref = high)")

png(
  file = here("figures", "mat_ed_slma_med_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_slma_med.plotdata  %>% pull(beta), 
  ci.lb = mat_ed_slma_med.plotdata %>% pull(ci_5),
  ci.ub = mat_ed_slma_med.plotdata %>% pull(ci_95), 
  slab = mat_ed_slma_med.plotdata %>% pull(cohort_neat), 
  ilab = mat_ed.fit$slma %>% map(slmaN) %>% unlist %>% as.numeric,
  ilab.x = -2,
  xlab = "Difference in childhood BMI by category of maternal education", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 4),
  ylim = c(0, 73),
  alim = c(-1.5, 2.5), 
  steps = 9, 
  digits = c(2, 2), 
  rows = c(68:56, 51:40, 36:24, 20:8, 4:1), 
  col = c(
    rep("black", 12), "#800000",
    rep("black", 11), "#800000", 
    rep("black", 12), "#800000", 
    rep("black", 12), "#800000",
    rep("black", 3), "#800000"))
  

text(-2, 72, "N subjects", cex = 0.6, font = 2)

text(-3.08, 69.5, "0-24 months", cex = 0.6, font = 2)
text(-3.05, 52.5, "24-48 months", cex = 0.6, font = 2)
text(-3.05, 37.5, "49-96 months", cex = 0.6, font = 2)
text(-3.02, 21.5, "97-168 months", cex = 0.6, font = 2)
text(-3.00, 5.5, "169-215 months", cex = 0.6, font = 2)

dev.off()


## ---- Low vs high ------------------------------------------------------------
mat_ed_slma_low.plotdata <- mat_ed_slma.plotdata %>% 
  filter(variable == "Low education (ref = high)")

png(
  file = here("figures", "mat_ed_slma_low_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_slma_low.plotdata  %>% pull(beta), 
  ci.lb = mat_ed_slma_low.plotdata %>% pull(ci_5),
  ci.ub = mat_ed_slma_low.plotdata %>% pull(ci_95), 
  slab = mat_ed_slma_low.plotdata %>% pull(cohort_neat), 
  ilab = mat_ed.fit$slma %>% map(slmaN) %>% unlist %>% as.numeric,
  ilab.x = -2,
  xlab = "Difference in childhood BMI by category of maternal education", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 4.5),
  ylim = c(0, 73),
  alim = c(-1, 3), 
  steps = 9, 
  digits = c(2, 2), 
  rows = c(68:56, 51:40, 36:24, 20:8, 4:1), 
  col = c(
    rep("black", 12), "#800000",
    rep("black", 11), "#800000", 
    rep("black", 12), "#800000", 
    rep("black", 12), "#800000",
    rep("black", 3), "#800000"))


text(-2, 72, "N subjects", cex = 0.6, font = 2)

text(-3.04, 69.5, "0-24 months", cex = 0.6, font = 2)
text(-3.01, 52.5, "24-48 months", cex = 0.6, font = 2)
text(-3.01, 37.5, "49-96 months", cex = 0.6, font = 2)
text(-2.98, 21.5, "97-168 months", cex = 0.6, font = 2)
text(-2.96, 5.5, "169-215 months", cex = 0.6, font = 2)

dev.off()


################################################################################
# Figures S4a & S4b: Area deprivation SLMA  
################################################################################

## ---- Medium vs high ---------------------------------------------------------
area_dep_slma_med.plotdata <- area_dep_slma.plotdata %>% 
  filter(variable == "Medium deprivation (ref = low)")

k_age_ad <- area_dep_slma_med.plotdata %>% 
  group_by(age) %>%
  summarise(n = n()) %>%
  pull(n) 

png(
  file = here("figures", "area_dep_slma_med_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_slma_med.plotdata  %>% pull(beta), 
  ci.lb = area_dep_slma_med.plotdata %>% pull(ci_5),
  ci.ub = area_dep_slma_med.plotdata %>% pull(ci_95), 
  slab = area_dep_slma_med.plotdata %>% pull(cohort_neat), 
  ilab = area_dep.fit$slma %>% map(slmaN) %>% unlist %>% as.numeric,
  ilab.x = -2,
  xlab = "Difference in childhood BMI by category of area deprivation", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 4),
  ylim = c(0, 34),
  alim = c(-1.5, 2.5), 
  steps = 9, 
  digits = c(2, 2), 
  rows = c(30:25, 22:17, 14:9, 6:1),
  col = rep(
    c(rep("black", 5), "#800000"), 4))

text(-2, 33, "N subjects", cex = 0.6, font = 2)


text(-3.08, 31, "0-24 months", cex = 0.6, font = 2)
text(-3.04, 23, "24-48 months", cex = 0.6, font = 2)
text(-3.04, 15, "49-96 months", cex = 0.6, font = 2)
text(-3.02, 7, "97-168 months", cex = 0.6, font = 2)

dev.off()

## ---- Low vs high ------------------------------------------------------------
area_dep_slma_high.plotdata <- area_dep_slma.plotdata %>% 
  filter(variable == "High deprivation (ref = low)")

png(
  file = here("figures", "area_dep_slma_high_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_slma_high.plotdata  %>% pull(beta), 
  ci.lb = area_dep_slma_high.plotdata %>% pull(ci_5),
  ci.ub = area_dep_slma_high.plotdata %>% pull(ci_95), 
  slab = area_dep_slma_high.plotdata %>% pull(cohort_neat), 
  ilab = area_dep.fit$slma %>% map(slmaN) %>% unlist %>% as.numeric,
  ilab.x = -2,
  xlab = "Difference in childhood BMI by category of area deprivation", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 4),
  ylim = c(0, 34),
  alim = c(-1.5, 2.5), 
  steps = 9, 
  digits = c(2, 2), 
  rows = c(30:25, 22:17, 14:9, 6:1),
  col = rep(
    c(rep("black", 5), "#800000"), 4))


text(-2, 33, "N subjects", cex = 0.6, font = 2)

text(-3.08, 31, "0-24 months", cex = 0.6, font = 2)
text(-3.04, 23, "24-48 months", cex = 0.6, font = 2)
text(-3.04, 15, "49-96 months", cex = 0.6, font = 2)
text(-3.02, 7, "97-168 months", cex = 0.6, font = 2)

dev.off()

################################################################################
# Figure S5: NDVI SLMA  
################################################################################
png(
  file = here("figures", "ndvi_slma_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = ndvi_slma.plotdata  %>% pull(beta), 
  ci.lb = ndvi_slma.plotdata %>% pull(ci_5),
  ci.ub = ndvi_slma.plotdata %>% pull(ci_95), 
  slab = ndvi_slma.plotdata %>% pull(cohort_neat), 
  xlab = "Difference in childhood BMI by unit change in NDVI", 
  ilab = ndvi.fit$slma %>% map(slmaN) %>% unlist %>% as.numeric,
  ilab.x = -7.5,
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-12, 10),
  ylim = c(0, 34),
  alim = c(-6, 6), 
  steps = 7, 
  digits = c(2, 2), 
  rows = c(30:25, 22:17, 14:9, 6:1),
  col = rep(
    c(rep("black", 5), "#800000"), 4))


text(-7.5, 33, "N subjects", cex = 0.6, font = 2)

text(-10.65, 31, "24-48 months", cex = 0.6, font = 2)
text(-10.65, 23, "49-96 months", cex = 0.6, font = 2)
text(-10.56, 15, "97-168 months", cex = 0.6, font = 2)
text(-10.52, 7, "169-215 months", cex = 0.6, font = 2)

dev.off()

################################################################################
# Figure S6: Pregnancy diabetes SLMA  
################################################################################
k_age_ad <- preg_dia_slma.plotdata %>% 
  group_by(age) %>%
  summarise(n = n())

png(
  file = here("figures", "preg_dia_slma_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = preg_dia_slma.plotdata  %>% pull(beta), 
  ci.lb = preg_dia_slma.plotdata %>% pull(ci_5),
  ci.ub = preg_dia_slma.plotdata %>% pull(ci_95), 
  slab = preg_dia_slma.plotdata %>% pull(cohort_neat), 
  ilab = preg_dia.fit$slma %>% map(slmaN) %>% unlist %>% as.numeric,
  ilab.x = -4,
  xlab = "Difference in childhood BMI where preganancy diabetes reported", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-6, 6),
  ylim = c(6, 64),
  alim = c(-3, 4), 
  steps = 8, 
  digits = c(2, 2), 
  rows = c(59:49, 45:35, 31:22, 18:8)  , 
  col = c(
    rep("black", 10), "#800000",
    rep("black", 10), "#800000", 
    rep("black", 9), "#800000", 
    rep("black", 10), "#800000"))


text(-4, 63, "N subjects", cex = 0.6, font = 2)

text(-5.31, 60.5, "0-24 months", cex = 0.6, font = 2)
text(-5.26, 46.5, "24-48 months", cex = 0.6, font = 2)
text(-5.26, 32.5, "49-96 months", cex = 0.6, font = 2)
text(-5.22, 19.5, "97-168 months", cex = 0.6, font = 2)

dev.off()


################################################################################
# Sex-stratified figures  
################################################################################
################################################################################
# Prepare data  
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed_m.pdata <- mat_ed_m.fit$ipd %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  mutate(sex = "Male")

mat_ed_f.pdata <- mat_ed_f.fit$ipd %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  mutate(sex = "Female")

mat_ed_sex.pdata <- list(mat_ed_m.pdata, mat_ed_f.pdata) %>%
  bind_rows %>%
  arrange(variable, desc(age), desc(sex)) 


## ---- Area deprivation -------------------------------------------------------
area_dep_m.pdata <- area_dep_m.fit$ipd %>% 
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  mutate(sex = "Male")

area_dep_f.pdata <- area_dep_f.fit$ipd %>% 
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  mutate(sex = "Female")

area_dep_sex.pdata <- list(area_dep_m.pdata, area_dep_f.pdata) %>%
  bind_rows %>%
  arrange(desc(age), variable, desc(sex))


## ---- NDVI -------------------------------------------------------------------
ndvi_m.plotdata <- ndvi_m.fit$ipd %>% 
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "ndvi") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(
    variable = "NDVI", 
    sex = "Male")     

ndvi_f.plotdata <- ndvi_f.fit$ipd %>% 
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "ndvi") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(
    variable = "NDVI", 
    sex = "Female")    

ndvi_sex.pdata <- list(ndvi_m.plotdata, ndvi_f.plotdata) %>%
  bind_rows %>%
  arrange(desc(age), variable, desc(sex))
  
## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_m.pdata <- preg_dia_m.fit$ipd %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "preg_dia1") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(
    variable = "Pregnancy diabetes reported", 
    sex = "Male")

preg_dia_f.pdata <- preg_dia_f.fit$ipd %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "preg_dia1") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(
    variable = "Pregnancy diabetes reported", 
    sex = "Female")

preg_dia_sex.pdata <- list(preg_dia_m.pdata, preg_dia_f.pdata) %>%
  bind_rows %>%
  arrange(desc(age), variable, desc(sex))

################################################################################
# Figure S7: Maternal education stratified by sex  
################################################################################
png(
  file = here("figures", "mat_ed_sex_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_sex.pdata %>% pull(est), 
  ci.lb = mat_ed_sex.pdata %>% pull(lowci),
  ci.ub = mat_ed_sex.pdata %>% pull(uppci), 
  slab = mat_ed_sex.pdata %>% pull(age),
  xlab = "Difference in childhood BMI by category of maternal education", 
  ilab =  cbind(
    c(
      rbind(
        rep(mat_ed_m.fit$ipd %>% map_int(function(x){x$Nvalid}), 2),
        rep(mat_ed_f.fit$ipd %>% map_int(function(x){x$Nvalid}), 2))
      ),
    c(
      rbind(
      rep(mat_ed_m.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}), 2),
      rep(mat_ed_f.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}), 2)))
  ),
  ilab.xpos = c(-2, -1.2),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(1, 27),
  alim = c(-1, 1.2), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(22:13, 10:1),
  col = rep(c("#F4A261", "#264653"), 10))

text(-2, 26, "N subjects", cex = 0.8, font = 2)
text(-1.2, 26, "N studies", cex = 0.8, font = 2)

text(-2.87, 23.5, "Medium education", cex = 0.8, font = 2)
text(-2.98, 11.5, "Low education", cex = 0.8, font = 2)

dev.off()

################################################################################
# Figure S8: Area deprivation stratified by sex  
################################################################################
png(
  file = here("figures", "area_dep_sex_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_sex.pdata %>% pull(est), 
  ci.lb = area_dep_sex.pdata %>% pull(lowci),
  ci.ub = area_dep_sex.pdata %>% pull(uppci), 
  slab = area_dep_sex.pdata %>% pull(age),
  xlab = "Difference in childhood BMI by category of maternal education", 
  ilab =  cbind(
    c(
      rbind(
        rep(area_dep_m.fit$ipd %>% map_int(function(x){x$Nvalid}), 2),
        rep(area_dep_f.fit$ipd %>% map_int(function(x){x$Nvalid}), 2))
    ),
    c(
      rbind(
        rep(area_dep_m.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}), 2),
        rep(area_dep_f.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}), 2)))
  ),
  ilab.xpos = c(-2, -1.2),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(1, 22),
  alim = c(-1, 1.5), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(18:11, 8:1),
  col = rep(c("#F4A261", "#264653"), 8))

text(-2, 21, "N subjects", cex = 0.8, font = 2)
text(-1.2, 21, "N studies", cex = 0.8, font = 2)

text(-2.87, 9.2, "Medium education", cex = 0.8, font = 2)
text(-2.98, 19.2, "Low education", cex = 0.8, font = 2)

dev.off()

################################################################################
# Figure S9: NDVI stratified by sex   
################################################################################
png(
  file = here("figures", "ndvi_ipd_sex_forest.png"), 
  width = word_full, 
  height = 10, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = ndvi_sex.pdata %>% pull(est), 
  ci.lb = ndvi_sex.pdata %>% pull(lowci),
  ci.ub = ndvi_sex.pdata %>% pull(uppci), 
  slab = ndvi_sex.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI by one unit change NDVI", 
  ilab =  cbind(
    c(
      rbind(
        area_dep_m.fit$ipd %>% map_int(function(x){x$Nvalid}),
        area_dep_f.fit$ipd %>% map_int(function(x){x$Nvalid}))
    ),
    c(
      rbind(
        area_dep_m.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}),
        area_dep_f.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})))
  ),
  ilab.xpos = c(-2.5, -1.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(0, 11),
  alim = c(-1.5, 1), 
  steps = 6, 
  digits = c(2, 2), 
  rows = 8:1,
  col = rep(c("#F4A261", "#264653"), 4))

text(-2.5, 10, "N subjects", cex = 0.8, font = 2)
text(-1.7, 10, "N studies", cex = 0.8, font = 2)


dev.off()


################################################################################
# Figure S10: Pregnancy diabetes  
################################################################################
png(
  file = here("figures", "preg_dia_sex_forest.png"), 
  width = word_full, 
  height = 12, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = preg_dia_sex.pdata %>% pull(est), 
  ci.lb = preg_dia_sex.pdata %>% pull(lowci),
  ci.ub = preg_dia_sex.pdata %>% pull(uppci), 
  slab = preg_dia_sex.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI where pregnancy diabetes reported", 
  ilab =  cbind(
    c(
      rbind(
        area_dep_m.fit$ipd %>% map_int(function(x){x$Nvalid}),
        area_dep_f.fit$ipd %>% map_int(function(x){x$Nvalid}))
    ),
    c(
      rbind(
        area_dep_m.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}),
        area_dep_f.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})))
  ),
  ilab.xpos = c(-2.5, -1.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(0, 11),
  alim = c(-1.5, 1), 
  steps = 6, 
  digits = c(2, 2), 
  rows = 8:1,
  col = rep(c("#F4A261", "#264653"), 4))

text(-2.5, 10, "N subjects", cex = 0.8, font = 2)
text(-1.7, 10, "N studies", cex = 0.8, font = 2)

dev.off()


################################################################################
# MoBa & DNBC
################################################################################
################################################################################
# Prepare data  
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed_r.pdata <- mat_ed_remove.fit$ipd %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  mutate(type = "Removed")

mat_ed_rem.pdata <- mat_ed_ipd.plotdata %>%
  mutate(type = "Full") %>%
  bind_rows(., mat_ed_r.pdata) %>%
  arrange(variable, desc(age), type)

## ---- Area deprivation -------------------------------------------------------
area_dep_r.pdata <- area_dep_remove.fit$ipd %>% 
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  mutate(type = "Removed")

area_dep_rem.pdata <- area_dep_ipd.plotdata %>%
  mutate(type = "Full") %>%
  bind_rows(., area_dep_r.pdata) %>%
  arrange(variable, desc(age), type)


## ---- NDVI -------------------------------------------------------------------
ndvi_r.pdata <- ndvi_remove.fit$ipd %>% 
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "ndvi") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(
    variable = "NDVI", 
    type = "Removed")     

ndvi_rem.pdata <- ndvi_ipd.plotdata %>%
  mutate(type = "Full") %>%
  bind_rows(., ndvi_r.pdata) %>%
  arrange(variable, desc(age), type)

## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_r.pdata <- preg_dia_remove.fit$ipd %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "preg_dia1") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(
    variable = "Pregnancy diabetes reported", 
    type = "Removed")    

preg_dia_rem.pdata <- preg_dia_ipd.plotdata %>%
  mutate(type = "Full") %>%
  bind_rows(., preg_dia_r.pdata) %>%
  arrange(variable, desc(age), type)


################################################################################
# Figure S9: Maternal education with and without MoBa & DNBC   
################################################################################
png(
  file = here("figures", "mat_ed_rem_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_rem.pdata %>% pull(est), 
  ci.lb = mat_ed_rem.pdata %>% pull(lowci),
  ci.ub = mat_ed_rem.pdata %>% pull(uppci), 
  slab = mat_ed_rem.pdata %>% pull(age),
  xlab = "Difference in childhood BMI by category of maternal education", 
  ilab =  cbind(
    c(
      rbind(
        rep(mat_ed_remove.fit$ipd %>% map_int(function(x){x$Nvalid}), 2),
        rep(mat_ed.fit$ipd %>% map_int(function(x){x$Nvalid}), 2))
    ),
    c(
      rbind(
        rep(mat_ed_remove.fit$ipd %>% map(function(x){length(x$disclosure.risk)}), 2),
        rep(mat_ed.fit$ipd %>% map(function(x){length(x$disclosure.risk)}), 2)))
  ),
  ilab.xpos = c(-2, -1.2),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(1, 27),
  alim = c(-1, 1.5), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(22:13, 10:1),
  col = rep(c("#264653", "#F4A261"), 10))

text(-2, 26, "N subjects", cex = 0.8, font = 2)
text(-1.2, 26, "N studies", cex = 0.8, font = 2)

text(-2.87, 23.5, "Medium education", cex = 0.8, font = 2)
text(-2.98, 11.5, "Low education", cex = 0.8, font = 2)

dev.off()

################################################################################
# Figure S10: Area deprivation with and without MoBa & DNBC   
################################################################################
png(
  file = here("figures", "area_dep_rem_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_rem.pdata %>% pull(est), 
  ci.lb = area_dep_rem.pdata %>% pull(lowci),
  ci.ub = area_dep_rem.pdata %>% pull(uppci), 
  slab = area_dep_rem.pdata %>% pull(age),
  xlab = "Difference in childhood BMI by category of maternal education", 
  ilab =  cbind(
    c(
      rbind(
        rep(area_dep_remove.fit$ipd %>% map_int(function(x){x$Nvalid}), 2),
        rep(area_dep.fit$ipd %>% map_int(function(x){x$Nvalid}), 2))
    ),
    c(
      rbind(
        rep(area_dep_remove.fit$ipd %>% map(function(x){length(x$disclosure.risk)}), 2),
        rep(area_dep.fit$ipd %>% map(function(x){length(x$disclosure.risk)}), 2)))
  ),
  ilab.xpos = c(-2, -1.2),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(1, 22),
  alim = c(-1, 1.2), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(18:11, 8:1),
  col = rep(c("#264653", "#F4A261"), 8))

text(-2, 21, "N subjects", cex = 0.8, font = 2)
text(-1.2, 21, "N studies", cex = 0.8, font = 2)

text(-2.87, 9.2, "Medium education", cex = 0.8, font = 2)
text(-2.98, 19.2, "Low education", cex = 0.8, font = 2)

dev.off()

################################################################################
# Figure S11: NDVI with and without MoBa & DNBC   
################################################################################
png(
  file = here("figures", "ndvi_rem_forest.png"), 
  width = word_full, 
  height = 10, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = ndvi_rem.pdata %>% pull(est), 
  ci.lb = ndvi_rem.pdata %>% pull(lowci),
  ci.ub = ndvi_rem.pdata %>% pull(uppci), 
  slab = ndvi_rem.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI by one unit change NDVI", 
  ilab =  cbind(
    c(
      rbind(
        area_dep_remove.fit$ipd %>% map_int(function(x){x$Nvalid}),
        area_dep.fit$ipd %>% map_int(function(x){x$Nvalid}))
    ),
    c(
      rbind(
        area_dep_remove.fit$ipd %>% map(function(x){length(x$disclosure.risk)}),
        area_dep.fit$ipd %>% map(function(x){length(x$disclosure.risk)})))
  ),
  ilab.xpos = c(-2.5, -1.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(0, 11),
  alim = c(-1.5, 1), 
  steps = 6, 
  digits = c(2, 2), 
  rows = 8:1,
  col = rep(c("#264653", "#F4A261"), 4))

text(-2.5, 10, "N subjects", cex = 0.8, font = 2)
text(-1.7, 10, "N studies", cex = 0.8, font = 2)

dev.off()

################################################################################
# Figure S12: Pregnanacy diabetes with and without MoBa & DNBC   
################################################################################
png(
  file = here("figures", "preg_dia_rem_forest.png"), 
  width = word_full, 
  height = 12, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = preg_dia_rem.pdata %>% pull(est), 
  ci.lb = preg_dia_rem.pdata %>% pull(lowci),
  ci.ub = preg_dia_rem.pdata %>% pull(uppci), 
  slab = preg_dia_rem.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI where pregnancy diabetes reported", 
  ilab =  cbind(
    c(
      rbind(
        area_dep_remove.fit$ipd %>% map_int(function(x){x$Nvalid}),
        area_dep.fit$ipd %>% map_int(function(x){x$Nvalid}))
    ),
    c(
      rbind(
        area_dep_remove.fit$ipd %>% map(function(x){length(x$disclosure.risk)}),
        area_dep.fit$ipd %>% map(function(x){length(x$disclosure.risk)})))
  ),
  ilab.xpos = c(-2.5, -1.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(0, 11),
  alim = c(-1.5, 1), 
  steps = 6, 
  digits = c(2, 2), 
  rows = 8:1,
  col = rep(c("#264653", "#F4A261"), 4))

text(-2.5, 10, "N subjects", cex = 0.8, font = 2)
text(-1.7, 10, "N studies", cex = 0.8, font = 2)

dev.off()



