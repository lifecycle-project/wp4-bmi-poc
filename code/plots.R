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

source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")

################################################################################
# Figure S1: missingness models
################################################################################

age_orig <- c("bmi_24", "bmi_48", "bmi_96", "bmi_168", "bmi_215")
age_new <- c("0-24 months", "25-48 months", "49-96 months", "97-168 months", 
             "169-215 months")

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

tmp <- mat_ed_ipd.plotdata %>% arrange(variable, desc(age))

forest(
  x = tmp %>% pull(beta), 
  ci.lb = tmp %>% pull(ci_5),
  ci.ub = tmp %>% pull(ci_95), 
  slab = tmp %>% pull(age), 
  xlab = "Difference in BMI by category of maternal education", 
  rows = c(4:8, 11:15), 
  cex = 1.2, 
  header = c("Age period", "Estimate [95% CI]"),
  ylim = c(4, 19), 
  xlim = c(-2, 4),
  alim = c(-1, 2)
)

text(x =  -1.45, y = 16, "Medium education (ref = high)", font = 2)
text(x =  -1.50, y = 9, "High education (ref = high)", font = 2)



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