# ---------------------------------------------------------------------------- #
# Manipulation Analysis
# Focus: Pre-school retention manipulation in 2006 (Fundeb base year)
#        Confounded by 8->9 year school reform
#        Placebo transfer comparison within terciles
#        RDD on enrollment at tercile boundaries (Section 5)
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(fixest)
library(rdrobust)
library(rddensity)
library(cobalt)
library(patchwork)
library(knitr)
library(kableExtra)

options(scipen = 999)

# ---- Paths ---- #
path_figures  <- "Z:/Tuffy/Paper - Educ/Resultados/v4/Figures/Manipulation/"
path_tables   <- "Z:/Tuffy/Paper - Educ/Resultados/v4/Tables/Manipulation/"


# ---------------------------------------------------------------------------- #
# 0. Data Prep ----
# ---------------------------------------------------------------------------- #

df_main <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf_flags.rds")

df_main <- df_main %>%
  mutate(cod_uf = codigo_ibge %/% 10000) %>%
  group_by(cod_uf) %>%
  mutate(
    dosage_tercile = ntile(dosage, 3),
    post_treat     = ifelse(ano > 2006, 1, 0)
  ) %>%
  ungroup() %>%
  mutate(
    tercile_label = factor(dosage_tercile,
                           levels = c(1, 2, 3),
                           labels = c("Low Dosage", "Medium Dosage", "High Dosage"))
  )

# ---------------------------------------------------------------------------- #
# 1. PRE-SCHOOL RETENTION ANALYSIS ----
#
# Logic: if municipalities inflated 2006 pre-school enrollment to gain
# higher Fundeb weights, we should see:
#   (a) an anomalous spike in mat_inf in 2006 relative to prior years
#   (b) concentrated in tercile 3 (net gainers)
#   (c) followed by a DROP in mat_inf and RISE in mat_fun in 2007-2008
#       as retained students finally transition — OR a rise in mat_fun9
#       consistent with the reform rather than manipulation
# ---------------------------------------------------------------------------- #

## 1.1 Build Pre-policy Growth History (2005 only, since only 2005-2006 pre-treatment) ----

# With only 2 pre-treatment years, we cannot compute a multi-year baseline.
# Instead, we use the SINGLE pre-treatment change (2005->2006) and compare
# it to the post-treatment change (2006->2007) — the reversion test.
# We also use cross-sectional variation: is the 2005->2006 inf growth
# correlated with dosage tercile?

df_transitions <- df_main %>%
  filter(ano %in% 2005:2009) %>%
  arrange(codigo_ibge, ano) %>%
  group_by(codigo_ibge) %>%
  mutate(
    # Level changes
    d_mat_inf   = mat_inf   - lag(mat_inf),
    d_mat_fun   = mat_fun   - lag(mat_fun),
    d_mat_fun8  = mat_fun8  - lag(mat_fun8),
    d_mat_fun9  = mat_fun9  - lag(mat_fun9),
    d_mat_ini8  = mat_ini8  - lag(mat_ini8),
    d_mat_ini9  = mat_ini9  - lag(mat_ini9),
    d_mat_total = mat_total - lag(mat_total),
    
    # Growth rates
    gr_inf   = d_mat_inf   / lag(mat_inf)   * 100,
    gr_fun   = d_mat_fun   / lag(mat_fun)   * 100,
    gr_fun8  = d_mat_fun8  / lag(mat_fun8)  * 100,
    gr_fun9  = d_mat_fun9  / lag(mat_fun9)  * 100,
    gr_ini8  = d_mat_ini8  / lag(mat_ini8)  * 100,
    gr_ini9  = d_mat_ini9  / lag(mat_ini9)  * 100,
    gr_total = d_mat_total / lag(mat_total) * 100,
    
    # Ratio: pre-school share of total enrollment
    inf_share  = mat_inf  / mat_total,
    fun8_share = mat_fun8 / mat_total,
    fun9_share = mat_fun9 / mat_total
  ) %>%
  ungroup()


## 1.2 Pre-school Share Over Time by Tercile ----
# If manipulation occurred, tercile 3 should show a spike in inf_share in 2006
# followed by a drop in 2007

p_inf_share <- df_transitions %>%
  filter(ano %in% 2005:2009) %>%
  group_by(ano, dosage_tercile) %>%
  summarise(
    mean_inf_share = mean(inf_share, na.rm = TRUE),
    se_inf_share   = sd(inf_share, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(tercile_label = factor(dosage_tercile,
                                levels = c(1,2,3),
                                labels = c("Low","Medium","High"))) %>%
  ggplot(aes(x = ano, y = mean_inf_share,
             color = tercile_label, group = tercile_label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.8) +
  geom_errorbar(aes(ymin = mean_inf_share - 1.96*se_inf_share,
                    ymax = mean_inf_share + 1.96*se_inf_share),
                width = 0.2, alpha = 0.5) +
  geom_vline(xintercept = 2006.5, linetype = "dashed", color = "grey40") +
  annotate("text", x = 2006.7, y = Inf,
           label = "Fundeb (2007)", hjust = 0, vjust = 1.5,
           size = 3.5, color = "grey40") +
  scale_color_manual(values = c("Low"    = "#0072B2",
                                "Medium" = "#D55E00",
                                "High"   = "#009E73")) +
  scale_x_continuous(breaks = 2005:2009) +
  labs(
    title    = "Pre-School Share of Total Enrollment by Dosage Tercile",
    #subtitle = "Spike in 2006 + drop in 2007 in tercile 3 would indicate retention manipulation",
    x = "Year", y = "Mean Pre-School Share", color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom")

p_inf_share


## 1.3 The Reform Decomposition ----
# The 8->9 year reform causes mat_fun8 to shrink and mat_fun9 to grow.
# Pure reform effect: mat_inf stable, mat_fun8 falls, mat_fun9 rises, mat_fun roughly stable
# Manipulation effect: mat_inf rises in 2006, then falls in 2007;
#                      mat_fun rises in 2007 (the retained cohort finally transitions)
#
# Plot all four enrollment categories together to visualize which story fits

p_reform <- df_transitions %>%
  filter(ano %in% 2005:2009) %>%
  group_by(ano, dosage_tercile) %>%
  summarise(
    across(c(gr_inf, gr_fun, gr_fun8, gr_fun9),
           ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(gr_inf, gr_fun, gr_fun8, gr_fun9),
               names_to = "category",
               values_to = "mean_growth") %>%
  mutate(
    category_label = factor(category,
                            levels = c("gr_inf","gr_fun","gr_fun8","gr_fun9"),
                            labels = c("Pre-School","Middle School (Total)",
                                       "Middle School (8yr)","Middle School (9yr)")),
    tercile_label = factor(dosage_tercile,
                           levels = c(1,2,3),
                           labels = c("Low Dosage","Medium Dosage","High Dosage"))
  ) %>%
  ggplot(aes(x = ano, y = mean_growth,
             color = category_label, group = category_label)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2006.5, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.5) +
  facet_wrap(~tercile_label) +
  scale_color_manual(values = c(
    "Pre-School"             = "#0072B2",
    "Middle School (Total)"  = "#D55E00",
    "Middle School (8yr)"    = "#CC79A7",
    "Middle School (9yr)"    = "#009E73"
  )) +
  scale_x_continuous(breaks = 2005:2009) +
  labs(
    title    = "Enrollment Growth by Category and Dosage Tercile",
    subtitle = paste0("Reform prediction: fun8 falls, fun9 rises, inf stable\n",
                      "Manipulation prediction: inf rises 2005→2006, falls 2006→2007"),
    x = "Year", y = "Mean Growth Rate (%)", color = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

p_reform


## 1.4 Reversion Test: inf growth 2005->2006 vs. 2006->2007 ----
# For each municipality: did those with large 2006 inf growth
# see a corresponding DROP in 2007? (manipulation)
# Or did growth continue/stabilize? (genuine reform/expansion)

df_reversion <- df_transitions %>%
  filter(ano %in% c(2006, 2007)) %>%
  select(codigo_ibge, dosage_tercile, tercile_label, ano, gr_inf, gr_fun) %>%
  pivot_wider(names_from = ano,
              values_from = c(gr_inf, gr_fun),
              names_sep = "_") %>%
  mutate(
    # Reversion = large positive 2006 followed by negative 2007
    inf_reversed = gr_inf_2006 > 5 & gr_inf_2007 < 0
  )

# Reversion rate by tercile
reversion_tab <- df_reversion %>%
  group_by(dosage_tercile, tercile_label) %>%
  summarise(
    n                   = n(),
    mean_gr_inf_2006    = mean(gr_inf_2006, na.rm = TRUE),
    mean_gr_inf_2007    = mean(gr_inf_2007, na.rm = TRUE),
    pct_large_2006      = mean(gr_inf_2006 > 5,  na.rm = TRUE),
    pct_reversed        = mean(inf_reversed, na.rm = TRUE),
    .groups = "drop"
  )

print(reversion_tab)


# Scatter: 2006 growth vs. 2007 growth in inf, by tercile
# Manipulation: points in Q4 (top-right in 2006, bottom-right in 2007)
p_reversion_scatter <- df_reversion %>%
  filter(!is.na(gr_inf_2006), !is.na(gr_inf_2007)) %>%
  ggplot(aes(x = gr_inf_2006, y = gr_inf_2007, color = tercile_label)) +
  geom_point(alpha = 0.3, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
           label = "Q4: Spike then reversal\n(manipulation zone)",
           size = 3, color = "grey30") +
  facet_wrap(~tercile_label) +
  coord_cartesian(xlim = c(-30, 30), ylim = c(-30, 30)) +
  scale_color_manual(values = c("Low Dosage"    = "#0072B2",
                                "Medium Dosage" = "#D55E00",
                                "High Dosage"   = "#009E73")) +
  labs(
    title    = "Pre-School Growth: 2006 vs. 2007 by Dosage Tercile",
    subtitle = "Points in lower-right quadrant (spike then reversal) suggest manipulation",
    x = "Pre-School Growth 2005→2006 (%)",
    y = "Pre-School Growth 2006→2007 (%)",
    color = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

p_reversion_scatter


## 1.5 Regression: Does 2006 inf growth predict 2007 inf DECLINE? ----
# Controlling for the reform (proxied by the simultaneous rise in fun9)
# A negative beta on gr_inf_2006 -> evidence of reversion (consistent with manipulation)
# A positive beta -> growth was real

df_reg_reversion <- df_reversion %>%
  left_join(
    df_transitions %>%
      filter(ano == 2007) %>%
      select(codigo_ibge, gr_fun9_2007 = gr_fun9, gr_fun8_2007 = gr_fun8),
    by = "codigo_ibge"
  ) %>%
  left_join(
    df_main %>%
      filter(ano == 2006) %>%
      select(codigo_ibge, cod_uf, PIBpc),
    by = "codigo_ibge"
  )

# OLS: does 2006 inf growth predict 2007 inf growth?
# Interacted with tercile to test if pattern differs by dosage group
lm_reversion <- feols(
  gr_inf_2007 ~ gr_inf_2006 * factor(dosage_tercile) +
    gr_fun9_2007 + gr_fun8_2007 + PIBpc  # control for reform and GDP
  | cod_uf,                               # UF fixed effects
  data = df_reg_reversion,
  cluster = ~cod_uf
)

etable(lm_reversion,
       title = "Reversion Test: Does 2006 Pre-School Growth Predict 2007 Decline?")

# Interpretation guide (printed for reference):
# gr_inf_2006 < 0 AND interaction with tercile 3 more negative
#   -> tercile 3 shows stronger reversion = manipulation consistent
# gr_inf_2006 > 0 (persistence) with no tercile difference
#   -> growth was genuine, no differential manipulation


# ---------------------------------------------------------------------------- #
# 2. PLACEBO TRANSFER COMPARISON ----
#
# You have: receita_real (actual Fundeb transfer)
#           receita_simulada (placebo: what Fundef would have given)
# aluno_dosage = (receita_real - receita_simulada) / mat_total[2006]
#
# Here we:
#   (a) Plot the distribution of actual vs. placebo transfers by tercile
#   (b) Test whether pre-2007 trends in spending/enrollment are parallel
#       across terciles (parallel trends using the placebo as counterfactual)
#   (c) Density test around the zero threshold
# ---------------------------------------------------------------------------- #

## 2.1 Distribution of Actual vs. Placebo Transfer by Tercile ----

df_transfer <- df_main %>%
  filter(ano == 2007) %>%  # first year of actual Fundeb
  mutate(
    gain        = receita_real - receita_simulada,
    gain_pa     = gain / mat_total,
    pct_change  = (receita_real - receita_simulada) / receita_simulada * 100
  )

# Box plot: distribution of gains by tercile
p_transfer_dist <- df_transfer %>%
  ggplot(aes(x = tercile_label, y = gain_pa, fill = tercile_label)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.8, width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("Low Dosage"    = "#0072B2",
                               "Medium Dosage" = "#D55E00",
                               "High Dosage"   = "#009E73")) +
  labs(
    title    = "Distribution of Fundeb Gain over Fundef Placebo (2007)",
    subtitle = "Gain = (Actual Fundeb - Simulated Fundef) / Enrollment 2006",
    x = NULL, y = "Gain per Student (R$)", fill = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")

p_transfer_dist


## 2.2 rddensity: Bunching at Zero ----

df_2006 <- df_main %>% filter(ano == 2006)

rdd_test <- rddensity(X = df_2006$aluno_dosage, c = 0)
summary(rdd_test)

# Visual — saved separately since rdplotdensity returns its own ggplot
rdd_plot <- rdplotdensity(
  rdd_test,
  df_2006$aluno_dosage,
  title  = "Density of Aluno Dosage around Zero Threshold (2006)",
  xlabel = "Aluno Dosage (R$/student)",
  ylabel = "Density"
)

# Repeat for pre-school-specific dosage if available
# (if you can compute inf_dosage = weight_inf * transfer / mat_inf)


## 2.3 Placebo Parallel Trends: Pre-2007 Spending by Tercile ----
# With only 2005 and 2006, this is a single pre-treatment gap.
# We test whether terciles were converging or diverging BEFORE treatment.

df_pre_trends <- df_main %>%
  filter(ano %in% c(2005, 2006)) %>%
  group_by(ano, dosage_tercile, tercile_label) %>%
  summarise(
    mean_spend_pa = mean(real_des_edu_pa, na.rm = TRUE),
    mean_inf_pa   = mean(real_des_inf_pa, na.rm = TRUE),
    mean_enroll   = mean(mat_total,       na.rm = TRUE),
    mean_inf      = mean(mat_inf,         na.rm = TRUE),
    se_spend_pa   = sd(real_des_edu_pa,   na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p_pre_trends <- df_pre_trends %>%
  ggplot(aes(x = ano, y = mean_spend_pa,
             color = tercile_label, group = tercile_label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_spend_pa - 1.96*se_spend_pa,
                    ymax = mean_spend_pa + 1.96*se_spend_pa),
                width = 0.05, alpha = 0.6) +
  scale_color_manual(values = c("Low Dosage"    = "#0072B2",
                                "Medium Dosage" = "#D55E00",
                                "High Dosage"   = "#009E73")) +
  scale_x_continuous(breaks = c(2005, 2006)) +
  labs(
    title    = "Pre-Treatment Spending per Student by Dosage Tercile",
    #subtitle = "Parallel slopes in 2005-2006 support parallel trends assumption",
    x = "Year", y = "Mean Spending per Student (R$)", color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom")

p_pre_trends


# ---------------------------------------------------------------------------- #
# 3. BALANCE / LOVE PLOT WITHIN TERCILES ----
#
# Comparing tercile 1 vs. 3 (with 2 as the "control" implicit in your design)
# on pre-treatment characteristics that should NOT differ if dosage assignment
# is as-good-as-random within UFs
# ---------------------------------------------------------------------------- #

## 3.1 Balance Table ----

balance_vars <- c(
  "mat_total",       # total enrollment
  "mat_inf",         # pre-school enrollment
  "inf_share",       # pre-school share of total — key manipulation test
  "mat_fun",         # middle school enrollment
  "mat_med",         # high school enrollment
  "real_des_edu_pa", # spending per student
  "real_des_inf_pa", # pre-school spending per student
  "real_des_fun_pa", # middle school spending per student
  "PIBpc"            # GDP per capita
)

balance_labels <- c(
  "Total Enrollment",
  "Pre-School Enrollment",
  "Pre-School Share of Total Enrollment",   # share — not affected by scale differences
  "Middle School Enrollment",
  "High School Enrollment",
  "Educ. Spending per Student",
  "Pre-School Spending per Student",
  "Middle School Spending per Student",
  "GDP per Capita"
)

df_balance <- df_main %>%
  filter(ano == 2006) %>%   # baseline year
  mutate(inf_share = mat_inf / mat_total) %>%
  select(codigo_ibge, dosage_tercile, tercile_label, all_of(balance_vars))

# Summary table: mean and SD by tercile
balance_tab <- df_balance %>%
  group_by(dosage_tercile) %>%
  summarise(
    across(all_of(balance_vars),
           list(mean = ~round(mean(.x, na.rm=TRUE), 2),
                sd   = ~round(sd(.x,   na.rm=TRUE), 2))),
    n = n(),
    .groups = "drop"
  )

# T-test: tercile 1 vs. 3 for each variable
ttest_results <- map2_dfr(balance_vars, balance_labels, function(var, label) {
  d1 <- df_balance %>% filter(dosage_tercile == 1) %>% pull(!!sym(var))
  d3 <- df_balance %>% filter(dosage_tercile == 3) %>% pull(!!sym(var))
  
  tt <- t.test(d1, d3)
  
  tibble(
    Variable   = label,
    Mean_T1    = round(mean(d1, na.rm=TRUE), 2),
    SD_T1      = round(sd(d1,   na.rm=TRUE), 2),
    Mean_T3    = round(mean(d3, na.rm=TRUE), 2),
    SD_T3      = round(sd(d3,   na.rm=TRUE), 2),
    Diff       = round(tt$estimate[1] - tt$estimate[2], 2),
    P_value    = round(tt$p.value, 3),
    Sig        = case_when(
      tt$p.value < 0.01 ~ "***",
      tt$p.value < 0.05 ~ "**",
      tt$p.value < 0.10 ~ "*",
      TRUE              ~ ""
    )
  )
})

print(ttest_results)

# Save as LaTeX
tex_table <- ttest_results %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Balance Table: Tercile 1 vs. Tercile 3 (Baseline 2006)",
    label = "tab:balance",
    align = c("l","r","r","r","r","r","r","c")
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size = 9
  ) %>%
  add_footnote(
    "Pre-School Share = mat\\_inf / mat\\_total. Shares compare composition, not scale.",
    notation = "none"
  )

writeLines(as.character(tex_table),
           paste0(path_tables, "balance_terciles.tex"))


## 3.2 Love Plot (cobalt) ----
# Standardized mean differences between tercile 1 and 3,
# treating tercile 3 as "treated" and tercile 1 as "control"
# (tercile 2 excluded since it's your comparison group in regressions)

df_loveplot <- df_balance %>%
  filter(dosage_tercile != 2) %>%
  mutate(treat_binary = ifelse(dosage_tercile == 3, 1, 0))

love_data <- df_loveplot %>%
  select(treat_binary, all_of(balance_vars)) %>%
  as.data.frame()

# Compute standardized mean differences manually for clean ggplot version
smd_tab <- map2_dfr(balance_vars, balance_labels, function(var, label) {
  d1   <- love_data[[var]][love_data$treat_binary == 0]
  d3   <- love_data[[var]][love_data$treat_binary == 1]
  pool_sd <- sqrt((var(d1, na.rm=T) + var(d3, na.rm=T)) / 2)
  
  tibble(
    Variable = label,
    SMD      = (mean(d3, na.rm=T) - mean(d1, na.rm=T)) / pool_sd
  )
})

# Love plot
p_love <- smd_tab %>%
  mutate(Variable = fct_reorder(Variable, abs(SMD))) %>%
  ggplot(aes(x = SMD, y = Variable)) +
  geom_vline(xintercept =  0,    color = "grey40",  linetype = "solid",  linewidth = 0.5) +
  geom_vline(xintercept =  0.1,  color = "#D62728", linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = -0.1,  color = "#D62728", linetype = "dashed", linewidth = 0.7) +
  geom_point(aes(color = abs(SMD) > 0.1), size = 3.5) +
  scale_color_manual(values = c("FALSE" = "#0072B2", "TRUE" = "#D62728"),
                     labels = c("|SMD| ≤ 0.1 (Balanced)",
                                "|SMD| > 0.1 (Imbalanced)"),
                     name = NULL) +
  labs(
    title    = "Love Plot: Standardized Mean Differences (Tercile 3 vs. Tercile 1)",
    subtitle = "Baseline characteristics in 2006 | Red dashed lines at ±0.1 threshold",
    x = "Standardized Mean Difference",
    y = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom")

p_love


## 3.3 Love Plot: Pre-School Enrollment Specifically ----
# Break down the pre-school comparison more finely:
# Is the imbalance in mat_inf already present in 2005,
# or does it emerge only in 2006 (consistent with manipulation)?

smd_by_year <- map_dfr(c(2005, 2006), function(yr) {
  df_yr <- df_main %>%
    filter(ano == yr, dosage_tercile != 2) %>%
    mutate(
      inf_share    = mat_inf / mat_total,   # compute share for each year
      treat_binary = ifelse(dosage_tercile == 3, 1, 0)
    )
  
  map2_dfr(balance_vars, balance_labels, function(var, label) {
    d1      <- df_yr[[var]][df_yr$treat_binary == 0]
    d3      <- df_yr[[var]][df_yr$treat_binary == 1]
    pool_sd <- sqrt((var(d1, na.rm=T) + var(d3, na.rm=T)) / 2)
    
    tibble(
      Year     = yr,
      Variable = label,
      SMD      = (mean(d3, na.rm=T) - mean(d1, na.rm=T)) / pool_sd
    )
  })
})

# Does the SMD on mat_inf GROW from 2005 to 2006?
# If yes -> imbalance emerged in the manipulation year
p_smd_years <- smd_by_year %>%
  mutate(Variable = fct_reorder(Variable, abs(SMD)),
         Year_f   = factor(Year)) %>%
  ggplot(aes(x = SMD, y = Variable, color = Year_f, shape = Year_f)) +
  geom_vline(xintercept =  0.1, color = "#D62728", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = -0.1, color = "#D62728", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept =  0,   color = "grey40",  linetype = "solid",  linewidth = 0.4) +
  geom_point(size = 3.5, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("2005" = "#0072B2", "2006" = "#D55E00"),
                     name = "Year") +
  scale_shape_manual(values = c("2005" = 16, "2006" = 17), name = "Year") +
  labs(
    title    = "SMD by Year: Did Imbalance Emerge in 2006?",
    x = "Standardized Mean Difference (Tercile 3 vs. Tercile 1)",
    y = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom")

p_smd_years


# ---------------------------------------------------------------------------- #
# 4. COVARIATE SMOOTHNESS AT THE ZERO THRESHOLD ----
# Tests whether manipulable (mat_inf) and non-manipulable (PIBpc) variables
# jump at the aluno_dosage = 0 cutoff in 2006
# ---------------------------------------------------------------------------- #

extract_rdrobust <- function(cov, data, x_var = "aluno_dosage", cutoff = 0) {
  fit <- rdrobust(y = data[[cov]], x = data[[x_var]], c = cutoff)
  tibble(
    covariate  = cov,
    estimate   = fit$coef[1, 1],
    se_robust  = fit$se[3, 1],
    p_robust   = fit$pv[3, 1],
    ci_low     = fit$ci[3, 1],
    ci_high    = fit$ci[3, 2],
    n_left     = fit$N[1],
    n_right    = fit$N[2]
  )
}

# Non-manipulable: PIBpc
# Potentially manipulable: mat_inf (pre-school), mat_fun (middle school)
# mat_total is the denominator of aluno_dosage so test separately
rd_covariates <- c("PIBpc", "mat_inf", "mat_fun", "real_des_edu_pa")
rd_labels     <- c("GDP per Capita", "Pre-School Enroll.", "Middle School Enroll.", "Spending per Student")

rd_smooth <- map2_dfr(rd_covariates, rd_labels, function(cov, label) {
  tryCatch(
    extract_rdrobust(cov, df_2006) %>% mutate(label = label),
    error = function(e) tibble(covariate = cov, label = label,
                               estimate = NA, se_robust = NA,
                               p_robust = NA, ci_low = NA, ci_high = NA,
                               n_left = NA, n_right = NA)
  )
})

print(rd_smooth)

# Plot the RD estimates with confidence intervals
p_rd_smooth <- rd_smooth %>%
  filter(!is.na(estimate)) %>%
  mutate(
    label = fct_reorder(label, estimate),
    sig   = p_robust < 0.1
  ) %>%
  ggplot(aes(x = estimate, y = label, color = sig)) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                 height = 0.2, linewidth = 0.8) +
  geom_point(size = 3.5) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "#0072B2", "TRUE" = "#D62728"),
                     labels = c("p > 0.10 (Smooth)", "p ≤ 0.10 (Jump)"),
                     name = NULL) +
  labs(
    title    = "Covariate Smoothness at the Zero Dosage Threshold (2006)",
    #subtitle = "Red = significant jump; non-manipulable vars (PIBpc) should be smooth",
    x = "RD Estimate (Robust)", y = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom")

p_rd_smooth


# ---------------------------------------------------------------------------- #
# 5. RDD ON PRE-SCHOOL ENROLLMENT ----
#
# Motivation: the manipulation channel runs through pre-school enrollment
# (mat_inf). If municipalities retained students in pre-school in 2006 to
# inflate the higher-weight Fundeb category, we should see a DISCONTINUOUS
# JUMP in mat_inf (and in inf_share = mat_inf/mat_total) at the tercile
# boundaries defined by aluno_dosage.
#
# Running variable: aluno_dosage (continuous, within-UF)
# Cutoffs: the empirical T1/T2 and T2/T3 boundary values within each UF.
#   Because terciles are defined within UF, we demean aluno_dosage by its
#   within-UF tercile boundary so that a single cutoff at zero applies pool-
#   wide. We do this separately for each boundary.
#
# Outcomes tested:
#   mat_inf   — pre-school level (direct manipulation target)
#   inf_share — pre-school share of total (composition, not scale)
#   mat_fun8  — middle school 8yr (shrinks under reform; should be smooth)
#   mat_fun9  — middle school 9yr (grows under reform; should be smooth)
# ---------------------------------------------------------------------------- #

df_2006 <- df_main %>% filter(ano == 2006)

## 5.1 Compute within-UF boundary values and re-center aluno_dosage ----
# For each UF, find the dosage values at the T1/T2 and T2/T3 cutoffs.
# Re-centering makes the cutoff sit at 0 for both boundaries pooled.

df_rdd_base <- df_2006 %>%
  mutate(inf_share = mat_inf / mat_total) %>%
  group_by(cod_uf) %>%
  mutate(
    # Exact boundary: the mean of the max of T1 and min of T2, etc.
    cut_12 = (max(aluno_dosage[dosage_tercile == 1], na.rm = TRUE) +
                min(aluno_dosage[dosage_tercile == 2], na.rm = TRUE)) / 2,
    cut_23 = (max(aluno_dosage[dosage_tercile == 2], na.rm = TRUE) +
                min(aluno_dosage[dosage_tercile == 3], na.rm = TRUE)) / 2,
    
    # Re-centered running variables: distance from each boundary
    rv_12  = aluno_dosage - cut_12,   # >0 means in T2 or above
    rv_23  = aluno_dosage - cut_23    # >0 means in T3
  ) %>%
  ungroup()


## 5.2 Helper: rdrobust on a re-centered running variable ----

run_rdd_inf <- function(outcome_var, rv_var, data, label_str) {
  y    <- data[[outcome_var]]
  x    <- data[[rv_var]]
  keep <- !is.na(y) & !is.na(x)
  y <- y[keep]; x <- x[keep]
  
  tryCatch({
    fit <- rdrobust(y = y, x = x, c = 0, bwselect = "mserd")
    tibble(
      outcome      = outcome_var,
      boundary     = rv_var,
      label        = label_str,
      estimate     = fit$coef[1, 1],
      se_robust    = fit$se[3, 1],
      p_robust     = fit$pv[3, 1],
      ci_low       = fit$ci[3, 1],
      ci_high      = fit$ci[3, 2],
      bw_left      = fit$bws[1, 1],
      bw_right     = fit$bws[1, 2],
      n_left       = fit$N_h[1],
      n_right      = fit$N_h[2]
    )
  }, error = function(e) {
    tibble(outcome = outcome_var, boundary = rv_var, label = label_str,
           estimate = NA_real_, se_robust = NA_real_, p_robust = NA_real_,
           ci_low = NA_real_, ci_high = NA_real_,
           bw_left = NA_real_, bw_right = NA_real_,
           n_left = NA_integer_, n_right = NA_integer_)
  })
}


## 5.3 Run RDD for each outcome × boundary combination ----

rdd_specs <- list(
  # outcome var   rv var    readable label
  list("mat_inf",   "rv_12", "Pre-School Level — T1/T2"),
  list("mat_inf",   "rv_23", "Pre-School Level — T2/T3"),
  list("inf_share", "rv_12", "Pre-School Share — T1/T2"),
  list("inf_share", "rv_23", "Pre-School Share — T2/T3"),
  list("mat_fun8",  "rv_12", "Middle School 8yr — T1/T2"),
  list("mat_fun8",  "rv_23", "Middle School 8yr — T2/T3"),
  list("mat_fun9",  "rv_12", "Middle School 9yr — T1/T2"),
  list("mat_fun9",  "rv_23", "Middle School 9yr — T2/T3")
)

rdd_results <- map_dfr(rdd_specs, function(s) {
  run_rdd_inf(s[[1]], s[[2]], df_rdd_base, s[[3]])
}) %>%
  mutate(
    cutoff_label = ifelse(boundary == "rv_12", "T1/T2 Boundary", "T2/T3 Boundary"),
    outcome_label = case_when(
      outcome == "mat_inf"   ~ "Pre-School Level",
      outcome == "inf_share" ~ "Pre-School Share",
      outcome == "mat_fun8"  ~ "Middle School 8yr",
      outcome == "mat_fun9"  ~ "Middle School 9yr"
    ),
    sig_flag = !is.na(p_robust) & p_robust < 0.10,
    sig = case_when(
      p_robust < 0.01 ~ "***",
      p_robust < 0.05 ~ "**",
      p_robust < 0.10 ~ "*",
      TRUE            ~ ""
    )
  )

print(rdd_results %>%
        select(label, estimate, se_robust, p_robust, ci_low, ci_high, n_left, n_right))


## 5.4 Coefficient plot: all outcomes × boundaries ----
# Key reading guide:
#   mat_inf or inf_share significant at T2/T3 -> pre-school manipulation concern
#   mat_fun8 / mat_fun9 significant -> reform effect bleeding through, not manipulation
#   All smooth (p > 0.10) -> no evidence of sorting at the tercile boundary

p_rdd_inf <- rdd_results %>%
  filter(!is.na(estimate)) %>%
  mutate(outcome_label = fct_rev(factor(outcome_label,
                                        levels = c("Pre-School Level",
                                                   "Pre-School Share",
                                                   "Middle School 8yr",
                                                   "Middle School 9yr")))) %>%
  ggplot(aes(x = estimate, y = outcome_label,
             color = sig_flag, shape = cutoff_label)) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                 height = 0.2, linewidth = 0.7,
                 position = position_dodge(width = 0.55)) +
  geom_point(size = 3.8,
             position = position_dodge(width = 0.55)) +
  scale_color_manual(
    values = c("FALSE" = "#0072B2", "TRUE"  = "#D62728"),
    labels = c("p \u2265 0.10 (Smooth)", "p < 0.10 (Jump)"),
    name   = NULL
  ) +
  scale_shape_manual(
    values = c("T1/T2 Boundary" = 16, "T2/T3 Boundary" = 17),
    name   = "Boundary"
  ) +
  labs(
    title    = "RDD: Enrollment Discontinuity at Tercile Boundaries (2006)",
    # subtitle = paste0(
    #   "Running variable: aluno\\_dosage re-centred at within-UF boundary\n",
    #   "Red = p < 0.10 | inf\\_share jump at T2/T3 = main manipulation signal"
    # ),
    x = "RD Estimate",
    y = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position  = "bottom",
    legend.box       = "vertical"
  )

p_rdd_inf


## 5.5 Visual RD plots for the two key outcomes at T2/T3 ----
# rdplot shows binned means on each side — a visible jump vs. smooth fit

make_rdplot_rv <- function(outcome_var, rv_var, title_str, df) {
  y    <- df[[outcome_var]]
  x    <- df[[rv_var]]
  keep <- !is.na(y) & !is.na(x)
  rdplot(
    y       = y[keep],
    x       = x[keep],
    c       = 0,
    title   = title_str,
    x.label = paste0("Aluno Dosage (re-centred at ", rv_var, " = 0)"),
    y.label = outcome_var
  )
}

# Pre-school level at T2/T3 — main manipulation concern
make_rdplot_rv("mat_inf",   "rv_23",
               "Pre-School Enrollment at T2/T3 Boundary (2006)", df_rdd_base)

# Pre-school share at T2/T3 — composition check
make_rdplot_rv("inf_share", "rv_23",
               "Pre-School Share at T2/T3 Boundary (2006)", df_rdd_base)

# Middle school 9yr at T2/T3 — reform check (should be smooth if not reform-driven)
make_rdplot_rv("mat_fun9",  "rv_23",
               "Middle School 9yr at T2/T3 Boundary (2006)", df_rdd_base)


## 5.6 LaTeX Table: RDD Results ----

tex_table <- rdd_results %>%
  filter(!is.na(estimate)) %>%
  select(label, estimate, se_robust, p_robust, ci_low, ci_high,
         n_left, n_right, sig) %>%
  mutate(
    across(c(estimate, se_robust, ci_low, ci_high), ~ round(.x, 3)),
    p_robust = round(p_robust, 3)
  ) %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    caption  = "RDD: Pre-School Enrollment Discontinuity at Tercile Boundaries (2006)",
    label    = "tab:rdd_preschool",
    col.names = c("Outcome", "Estimate", "SE (Robust)", "p-value",
                  "CI Low", "CI High", "N Left", "N Right", "Sig."),
    align    = c("l","r","r","r","r","r","r","r","c")
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 9
  ) %>%
  add_footnote(
    paste0(
      "Running variable is aluno\\_dosage re-centred at the within-UF T1/T2 or T2/T3 boundary. ",
      "inf\\_share = mat\\_inf / mat\\_total. ",
      "A significant jump in inf\\_share at T2/T3 indicates compositional manipulation."
    ),
    notation = "none"
  )

writeLines(as.character(tex_table),
           paste0(path_tables, "rdd_preschool_boundaries.tex"))


# ---------------------------------------------------------------------------- #
# 7. COMBINED FIGURE PANELS ----
# ---------------------------------------------------------------------------- #

# Panel A: Pre-school share over time + reform decomposition
panel_A <- p_inf_share / p_reform +
  plot_layout(heights = c(1, 1.5)) +
  plot_annotation(
    title = "A. Pre-School Retention: Time Series Evidence",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(plot = panel_A,
       filename = file.path(path_figures, "panel_A_retention.pdf"),
       device = "pdf", height = 12, width = 14)

# Panel B: Reversion scatter
ggsave(plot = p_reversion_scatter,
       filename = file.path(path_figures, "panel_B_reversion.pdf"),
       device = "pdf", height = 6, width = 12)

# Panel C: Love plots
panel_C <- p_love / p_smd_years +
  plot_annotation(
    title = "C. Balance Between Tercile 1 and 3",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(plot = panel_C,
       filename = file.path(path_figures, "panel_C_loveplot.pdf"),
       device = "pdf", height = 10, width = 10)

# Panel D: Transfer distribution + pre-trends
panel_D <- p_transfer_dist | p_pre_trends +
  plot_annotation(
    title = "D. Placebo Transfer Comparison",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(plot = panel_D,
       filename = file.path(path_figures, "panel_D_placebo.pdf"),
       device = "pdf", height = 6, width = 14)


message("All manipulation analysis outputs saved.")
