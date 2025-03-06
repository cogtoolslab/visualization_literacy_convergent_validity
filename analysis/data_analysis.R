
#'
#' Data analysis script for "Evaluating convergence between two data visualization literacy assessments"
#'


# INIT ----
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(boot) # for inv.logit calls
library(emmeans) # for emmeans calls
library(ggdist) # for half-eye distribution plots
library(lme4)
library(psych) # needed for factor analysis (`fa`)
library(RColorBrewer) # colors in heatmap https://r-graph-gallery.com/215-the-heatmap-function.html
library(tidyverse)


# GLOBALS ----
DATA_DIR = '../data'
FIGURES_DIR = '../results/figures'

default_plot_theme = theme(
  # text
  plot.title = element_text(size = 24, family = 'Avenir', color = 'black', margin = margin(b = 0.5, unit = 'line')),
  axis.title.y = element_text(size = 24, family = 'Avenir', color = 'black', margin = margin(r = 0.5, unit = 'line')),
  axis.title.x = element_text(size = 24, family = 'Avenir', color = 'black', margin = margin(t = 0.5, unit = 'line')),
  axis.text.y = element_text(size = 18, family = 'Avenir', color = 'black'),
  axis.text.x = element_text(size = 18, family = 'Avenir', color = 'black'),
  legend.title = element_text(size = 20, family = 'Avenir', color = 'black'),
  legend.text = element_text(size = 14, family = 'Avenir', color = 'black'),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.line = element_line(color = 'black'),
  axis.ticks = element_line(color = 'black')
)

TEST_COLORS = c('GGR' = '#ef8a62', 'VLAT' = '#67a9cf')
SAMPLE_COLORS = c('prolific' = '#617e49', 'sona' = '#aec59b')



# DATA PROCESSING ----

data = read_csv(paste(DATA_DIR, 'data_processed.csv', sep = '/'))

data$math_sum = factor(data$math_sum, levels = c('0', '1', '2', '3'))
data$recruitmentPlatform = factor(data$recruitmentPlatform,
                                  levels = c('prolific', 'sona'))


# DEMOGRAPHICS ----

# Recruiting platform
data %>%
  group_by(recruitmentPlatform) %>%
  summarize(n_distinct(gameID))

# Math experience
data %>%
  group_by(math_sum, recruitmentPlatform) %>%
  summarize(n_distinct(gameID))


# ANALYSIS: Comparing performance across groups (overall) ----

# Overall performance

accuracy_baseline = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data
)
summary(accuracy_baseline) # mean estimate
confint(accuracy_baseline) # 95% CI
# Deriving probabilities from these summaries:
inv.logit(1.13435)

subj_summary = data %>%
  group_by(gameID, recruitmentPlatform, math_sum) %>% # need recruitmentPlatform and math_sum later
  summarize(
    subj_accuracy = mean(accuracy)
  )
summary(subj_summary$subj_accuracy) # mean, min, max
sd(subj_summary$subj_accuracy) # SD




# Overall performance across groups
accuracy_baseline_prolific = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(recruitmentPlatform == 'prolific')
)

accuracy_baseline_sona = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(recruitmentPlatform == 'sona')
)

summary(accuracy_baseline_prolific)
confint(accuracy_baseline_prolific)
inv.logit(1.3677741)
summary(accuracy_baseline_sona)
confint(accuracy_baseline_sona)
inv.logit(1.096523)


# Correlation between groups
item_summary = data %>%
  group_by(gcbID, recruitmentPlatform) %>%
  summarize(
    mean_accuracy = mean(accuracy),
    sd_accuracy = sd(accuracy),
    se_accuracy = sd(accuracy) / sqrt(n())
  ) %>% ungroup()

cor.test(
  item_summary$mean_accuracy[item_summary$recruitmentPlatform == 'prolific'],
  item_summary$mean_accuracy[item_summary$recruitmentPlatform == 'sona'],
)



# > FIGURE: Comparing performance across groups (overall) ----
fig_perf_sample = data %>%
  group_by(gcbID, origStudy, recruitmentPlatform) %>%
  summarize(accuracy = mean(accuracy)) %>%
  ungroup() %>%
  pivot_wider(names_from = recruitmentPlatform, values_from = accuracy) %>%
  ggplot(aes(x = prolific, y = sona, color = origStudy)) +
  geom_point(alpha = 0.75, size = 3) +
  geom_abline(linetype = 'dashed', linewidth = 0.75) +
  scale_x_continuous(
    name = 'proportion correct in U.S. representative sample',
    breaks = seq(0, 1, by = 0.25),
    labels = c('0.00', '0.25', '0.50', '0.75', '1.00'),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    name = 'proportion correct in U.S. university sample',
    breaks = seq(0, 1, by = 0.25),
    labels = c('0.00', '0.25', '0.50', '0.75', '1.00'),
    limits = c(0, 1)
  ) +
  scale_color_manual(
    values = TEST_COLORS
  ) +
  default_plot_theme +
  theme(
    legend.position = 'none'
  )

fig_perf_sample
ggsave(paste(FIGURES_DIR, 'Proportion_GGR_VLAT_RAW.pdf', sep = '/'),
       fig_perf_sample,
       width = 9, height = 8,
       device = cairo_pdf)


# ANALYSIS: Comparing performance across groups by math experience ----

# Is math experience a significant predictor of overall performance?
accuracy_baseline = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data
)
accuracy_math_exp = glmer(
  accuracy ~ 1 + math_sum + (1 | gameID),
  family = 'binomial',
  data = data
)
summary(accuracy_math_exp)
anova(accuracy_baseline, accuracy_math_exp, test = 'LRT')
emmeans(accuracy_math_exp, 'math_sum')
inv.logit(0.971) # NB: use inv.logit to get predicted performance for each level of math_sum


# Does this effect differ across sample populations?
accuracy_baseline_math_exp_sample = glmer(
  accuracy ~ 1 + math_sum + recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  data = data
)
accuracy_interaction_math_exp_sample = glmer(
  accuracy ~ 1 + math_sum * recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  data = data
)
summary(accuracy_interaction_math_exp_sample)
anova(accuracy_baseline_math_exp_sample, accuracy_interaction_math_exp_sample, test = 'LRT')



# > FIGURE: Comparing performance across groups by math experience ----

fig_math_exp = data %>%
  group_by(math_sum, recruitmentPlatform) %>%
  summarize(
    mean_accuracy = mean(accuracy),
    se_accuracy = sd(accuracy) / sqrt(n())
  ) %>%
  ggplot(aes(x = reorder(math_sum, mean_accuracy),
             y = mean_accuracy,
             fill = recruitmentPlatform,
             dist = 'norm',
             arg1 = mean_accuracy,
             arg2 = se_accuracy )) +
  stat_dist_halfeye(show_interval = TRUE,
                    position = position_dodge(width = 0),
                    width = 0.95,
                    size = 0.5,
                    alpha = 0.7) +
  scale_x_discrete(
    name = 'number of math courses'
  ) +
  scale_y_continuous(
    name = 'proportion correct',
    breaks = seq(0, 1, by = 0.25),
    labels = c('0.00', '0.25', '0.50', '0.75', '1.00'),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = SAMPLE_COLORS
  ) +
  default_plot_theme +
  theme(
    legend.position = 'none'
  )

fig_math_exp
ggsave(paste(FIGURES_DIR, 'Proportion_MathExperience_BySample_RAW.pdf', sep = '/'),
       fig_math_exp,
       width = 6, height = 7,
       device = cairo_pdf)


# ANALYSIS: Comparing performance across assessments ----

# Summary of performance on each assessment

accuracy_baseline_ggr = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'GGR')
)
accuracy_baseline_vlat = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'VLAT')
)

summary(accuracy_baseline_ggr)
confint(accuracy_baseline_ggr)
inv.logit(1.4694818)
summary(accuracy_baseline_vlat)
confint(accuracy_baseline_vlat)
inv.logit(1.1229898)

test_summary_subj = data %>%
  group_by(gameID, origStudy) %>%
  summarize(
    subj_test_accuracy = mean(accuracy)
  ) %>% ungroup()
sd(test_summary_subj$subj_test_accuracy[test_summary_subj$origStudy == 'GGR']) # sd GGR
sd(test_summary_subj$subj_test_accuracy[test_summary_subj$origStudy == 'VLAT']) # sd VLAT

# Correlation between assessments
cor.test(
  test_summary_subj$subj_test_accuracy[test_summary_subj$origStudy == 'GGR'],
  test_summary_subj$subj_test_accuracy[test_summary_subj$origStudy == 'VLAT']
)


# > FIGURE: Comparing performance across assessments ----

fig_corr_test = data %>%
  select(gameID, accuracy, origStudy) %>%
  group_by(gameID, origStudy) %>%
  summarize(accuracy = mean(accuracy)) %>%
  ungroup() %>%
  pivot_wider(names_from = origStudy, values_from = accuracy) %>%
  ggplot(aes(x = VLAT, y = GGR)) +
  geom_point(alpha = 0.1, size = 3) +
  geom_abline(linetype = 'dashed', linewidth = 0.75) +
  scale_x_continuous(
    name = 'proportion correct on VLAT',
    breaks = seq(0, 1, by = 0.25),
    labels = c('0.00', '0.25', '0.50', '0.75', '1.00'),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    name = 'proportion correct on GGR',
    breaks = seq(0, 1, by = 0.25),
    labels = c('0.00', '0.25', '0.50', '0.75', '1.00'),
    limits = c(0, 1)
  ) +
  default_plot_theme

fig_corr_test
ggsave(paste(FIGURES_DIR, 'Proportion_University_US_RAW.pdf', sep = '/'),
       fig_corr_test,
       width = 6,
       height = 6,
       device = cairo_pdf
       )


# ANALYSIS: Comparing performance across graph type ----

# Does performance vary by graph type on the combined assessment?
accuracy_baseline = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data
)
accuracy_graph_type = glmer(
  accuracy ~ graphType + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'optimx', optCtrl = list(method = 'nlminb')),
  data = data
)
summary(accuracy_graph_type)
anova(accuracy_graph_type, accuracy_baseline, test = 'LRT')

# Does performance vary by graph type on each assessment individually?
  # GGR
accuracy_baseline_ggr = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'GGR'),
)
accuracy_graph_type_ggr = glmer(
  accuracy ~ graphType + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'GGR'),
)
summary(accuracy_graph_type_ggr)
anova(accuracy_baseline_ggr, accuracy_graph_type_ggr, test = 'LRT')

  # VLAT
accuracy_baseline_vlat = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'VLAT'),
)
accuracy_graph_type_vlat = glmer(
  accuracy ~ graphType + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'optimx', optCtrl = list(method = 'nlminb')),
  data = data %>% filter(origStudy == 'VLAT'),
)
summary(accuracy_graph_type_vlat)
anova(accuracy_baseline_vlat, accuracy_graph_type_vlat, test = 'LRT')


# Does effect of performance on graph type vary across samples (prolific, sona)?
  # Combined assessment
accuracy_graph_type_sample_baseline = glmer(
  accuracy ~ graphType + recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)),
  data = data
)
accuracy_graph_type_sample = glmer(
  accuracy ~ graphType * recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)),
  data = data
)
summary(accuracy_graph_type_sample)
anova(accuracy_graph_type_sample, accuracy_graph_type_sample_baseline, test = 'LRT')
emmeans(accuracy_graph_type_sample, pairwise ~ recruitmentPlatform) # larger effect for prolific sample?

  # GGR
accuracy_graph_type_sample_ggr_baseline = glmer(
  accuracy ~ graphType + recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'GGR'),
)
accuracy_graph_type_sample_ggr = glmer(
  accuracy ~ graphType * recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'GGR'),
)
summary(accuracy_graph_type_sample_ggr)
anova(accuracy_graph_type_sample_ggr, accuracy_graph_type_sample_ggr_baseline, test = 'LRT')
emmeans(accuracy_graph_type_sample_ggr, pairwise ~ recruitmentPlatform) # larger effect for prolific sample?

  # VLAT
accuracy_graph_type_sample_vlat_baseline = glmer(
  accuracy ~ graphType + recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)),
  data = data %>% filter(origStudy == 'VLAT'),
)
accuracy_graph_type_sample_vlat = glmer(
  accuracy ~ graphType * recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)),
  data = data %>% filter(origStudy == 'VLAT'),
)
summary(accuracy_graph_type_sample_vlat)
anova(accuracy_graph_type_sample_vlat, accuracy_graph_type_sample_vlat_baseline, test = 'LRT')
emmeans(accuracy_graph_type_sample_vlat, pairwise ~ recruitmentPlatform) # larger effect for prolific sample?




# > FIGURE: Comparing performance across graph type ----

fig_graph_type = data %>%
  mutate(
    study_graph = factor(
      paste(origStudy, graphType, sep = '+'),
      levels = c(
        'GGR+bar_chart',
        'GGR+line_chart',
        'GGR+pie_chart',
        'GGR+icon_array',

        'VLAT+bar_chart',
        'VLAT+line_chart',
        'VLAT+pie_chart',

        'VLAT+stacked_bar_chart',
        'VLAT+normalized_stacked_bar_chart',
        'VLAT+histogram',

        'VLAT+scatterplot',
        'VLAT+bubble_chart',

        'VLAT+area_chart',
        'VLAT+stacked_area_chart',

        'VLAT+treemap',
        'VLAT+choropleth'
      )
    )
  ) %>%
  group_by(origStudy, graphType, study_graph, filename) %>%
  summarize(
    mean_accuracy = mean(accuracy),
    se_accuracy = sd(accuracy) / sqrt(n())
  ) %>%
  ggplot(aes(x = study_graph,
             y = mean_accuracy,
             dist = 'norm',
             arg1 = mean_accuracy,
             arg2 = se_accuracy,
             fill = origStudy)
  ) +
  stat_dist_halfeye(
    show_interval = TRUE,
    position = position_dodge(width = 0),
    width = 0.95,
    size = 0.5,
    alpha = 1
  ) +
  scale_x_discrete(
    name = element_blank(),
    labels = c(
      'GGR+bar_chart' = 'bar',
      'GGR+line_chart' = 'line',
      'GGR+pie_chart' = 'pie',
      'GGR+icon_array' = 'icon array',

      'VLAT+bar_chart' = 'bar',
      'VLAT+line_chart' = 'line',
      'VLAT+pie_chart' = 'pie',

      'VLAT+stacked_bar_chart' = 'stacked \nbar',
      'VLAT+normalized_stacked_bar_chart' = '100% \nstacked \nbar',
      'VLAT+histogram' = 'histogram',

      'VLAT+area_chart' = 'area',
      'VLAT+stacked_area_chart' = 'stacked \narea',

      'VLAT+scatterplot' = 'scatter',
      'VLAT+bubble_chart' = 'bubble',

      'VLAT+choropleth' = 'map',
      'VLAT+treemap' = 'treemap'
    )
  ) +
  scale_y_continuous(
    name = 'proportion correct',
    breaks = seq(0, 1, by = 0.25),
    labels = c('0.00', '0.25', '0.50', '0.75', '1.00'),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = TEST_COLORS
  ) +
  default_plot_theme +
  theme(
    legend.position = 'none'
  )


fig_graph_type
ggsave(paste(FIGURES_DIR, 'Propotion_GraphType_ByTest_RAW.pdf', sep = '/'),
       fig_graph_type,
       width = 20,
       height = 8,
       device = cairo_pdf
)



# ANALYSIS: Comparing performance across question type ----

# Does performance vary by question type on the combined assessment?
accuracy_baseline = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data
)
accuracy_question_type = glmer(
  accuracy ~ 1 + origStudy_taskCategorization + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'optimx', optCtrl = list(method = 'nlminb')),
  data = data
)
summary(accuracy_question_type)
anova(accuracy_baseline, accuracy_question_type, test = 'LRT')


# Does performance vary by question type on each assessment individually?
  # GGR
accuracy_baseline_ggr = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'GGR'),
)
accuracy_question_type_ggr = glmer(
  accuracy ~ 1 + origStudy_taskCategorization + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'GGR'),
)
summary(accuracy_question_type_ggr)
anova(accuracy_baseline_ggr, accuracy_question_type_ggr, test = 'LRT')



  # VLAT
accuracy_baseline_vlat = glmer(
  accuracy ~ 1 + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'VLAT'),
)
accuracy_question_type_vlat = glmer(
  accuracy ~ 1 + origStudy_taskCategorization + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'Nelder_Mead'),
  data = data %>% filter(origStudy == 'VLAT'),
)
summary(accuracy_question_type_vlat)
anova(accuracy_baseline_vlat, accuracy_question_type_vlat, test = 'LRT')


# Does effect of performance on question type vary across samples (prolific, sona)?
  # Combined assessment
accuracy_question_type_sample_baseline = glmer(
  accuracy ~ origStudy_taskCategorization + recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)),
  data = data
)
accuracy_question_type_sample = glmer(
  accuracy ~ origStudy_taskCategorization * recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)),
  data = data
)
summary(accuracy_question_type_sample)
anova(accuracy_question_type_sample, accuracy_question_type_sample_baseline, test = 'LRT')
emmeans(accuracy_question_type_sample, pairwise ~ recruitmentPlatform) # larger effect for prolific sample?

  # GGR
accuracy_question_type_sample_ggr_baseline = glmer(
  accuracy ~ origStudy_taskCategorization + recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'GGR'),
)
accuracy_question_type_sample_ggr = glmer(
  accuracy ~ origStudy_taskCategorization * recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  data = data %>% filter(origStudy == 'GGR'),
)
summary(accuracy_question_type_sample_ggr)
anova(accuracy_question_type_sample_ggr, accuracy_question_type_sample_ggr_baseline, test = 'LRT')
emmeans(accuracy_question_type_sample_ggr, pairwise ~ recruitmentPlatform) # larger effect for prolific sample?

  # VLAT
accuracy_question_type_sample_vlat_baseline = glmer(
  accuracy ~ origStudy_taskCategorization + recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)),
  data = data %>% filter(origStudy == 'VLAT'),
)
accuracy_question_type_sample_vlat = glmer(
  accuracy ~ origStudy_taskCategorization * recruitmentPlatform + (1 | gameID),
  family = 'binomial',
  control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)),
  data = data %>% filter(origStudy == 'VLAT'),
)
summary(accuracy_question_type_sample_vlat)
anova(accuracy_question_type_sample_vlat, accuracy_question_type_sample_vlat_baseline, test = 'LRT')
emmeans(accuracy_question_type_sample_vlat, pairwise ~ recruitmentPlatform) # larger effect for prolific sample?


# > FIGURE: Comparing performance across question type ----

fig_question_type = data %>%
  group_by(origStudy, origStudy_taskCategorization) %>%
  summarize(
    mean_accuracy = mean(accuracy),
    se_accuracy = sd(accuracy) / sqrt(n())
  ) %>%
  mutate(
    question_type_fac = factor(
      origStudy_taskCategorization,
      levels = c('level_1', 'level_2', 'level_3',
                 'characterize_distribution', 'find_correlations_trends',
                 'find_clusters',  'find_extremum',
                 'retrieve_value', 'make_comparisons',
                 'determine_range', 'find_anomolies'))
  ) %>%
  ggplot(aes(x = question_type_fac,
             y = mean_accuracy,
             dist = 'norm',
             arg1 = mean_accuracy,
             arg2 = se_accuracy,
             fill = origStudy)
  ) +
  stat_dist_halfeye(
    show_interval = TRUE,
    position = position_dodge(width = 0),
    width = 0.95,
    size = 0.5,
    alpha = 1
  ) +
  scale_x_discrete(
    name = element_blank(),
    labels = c(
      'level_1' = 'level 1 \nread \nthe data',
      'level_2' = 'level 2 \nread between \nthe data',
      'level_3' = 'level 3 \nread beyond \nthe data',
      'characterize_distribution' = 'characterize \ndistribution',
      'find_correlations_trends' = 'find \ncorrelations / \ntrends',
      'find_clusters' = 'find \nclusters',
      'find_extremum' = 'find \nextremum',
      'retrieve_value' = 'retrieve \nvalue',
      'make_comparisons' = 'make \ncomparisons',
      'determine_range' = 'determine \nrange',
      'find_anomolies' = 'find \nanomalies'
    )
  ) +
  scale_y_continuous(
    name = 'proportion correct',
    breaks = seq(0, 1, by = 0.25),
    labels = c('0.00', '0.25', '0.50', '0.75', '1.00'),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = TEST_COLORS
  ) +
  default_plot_theme +
  theme(
    legend.position = 'none'
  )

fig_question_type
ggsave(paste(FIGURES_DIR, 'Proportion_QuestionType_ByTest_RAW.pdf', sep = '/'),
       fig_question_type,
       width = 20,
       height = 10,
       device = cairo_pdf
)



# ANALYSIS: Comparing predictive models of performance ----

# > FUNCTIONS ----

get_normalized_data = function(data_mat) {
  # Calculate factor scores based on modified loadings
  # NB: same process as used for verification of scores above
  mus = colMeans(data_mat)
  sigmas = apply(data_mat, 2, sd) # calculate `sd` over all columns ('2' means apply over columns)
  dat_scaled = scale(data_mat, mus, sigmas)
  return(dat_scaled)
}

init_loading_matrix = function(factor_col, L) {
  factors = unique(L[[factor_col]])
  m_factors = length(factors)
  colnames = sapply(seq(m_factors), function(x){paste0('MR', x)})

  # Create new top-down loading matrix
  loading_matrix = matrix(
    nrow = nrow(L),
    ncol = m_factors,
    dimnames = list(L$Item, colnames)
  )

  for(i in seq(m_factors)) {
    loading_matrix[,i] = ifelse(L[[factor_col]] == factors[i], 1, 0)
  }
  # Sanity check
  stopifnot(unique(rowSums(loading_matrix)) == 1) # all rows should sum to 1
  return(loading_matrix)
}

factor_analysis = function(data_mat, factor_type) {
  # Calculate factor scores based on modified loadings
  # NB: same process as used for verification of scores above
  mus = colMeans(data_mat)
  sigmas = apply(data_mat, 2, sd) # calculate `sd` over all columns (2 is for columns)
  dat_scaled = scale(data_mat, mus, sigmas)

  lambda_inv = solve(cor(data_mat)) %*% factor_type
  scores_manual = dat_scaled  %*% lambda_inv
  preds = t(factor_type %*% t(scores_manual))
  return(preds)
}


# > INIT ----

# Convert long form data to wide format accuracy matrix (n participant rows by q question columns)
accuracy_matrix = data %>%
  pivot_wider(
    id_cols = c('gameID'),
    names_from = c('gcbID'),
    values_from = c('accuracy'),
    names_sort = TRUE # sort columns so we know they're always in a reliable order
  ) %>%
  select(!gameID) # can add this back in for debugging


test_item_lookup = data %>%
  group_by(gcbID) %>%
  summarize(test = unique(origStudy),
            graph_type = unique(graphType),
            q_category = unique(origStudy_taskCategorization)) %>%
  ungroup() %>%
  rename('Item' = gcbID)


# > Number of factors ----

# Shuffle participant rows for train / test folds
rows = seq(1:nrow(accuracy_matrix))
rows_shuffled = sample(rows)
kfolds = 5
max_factors = 6
fold_size = ceiling(nrow(accuracy_matrix) / kfolds)
bic_df = data.frame()
# For each iteration through the k folds, set aside one fold and use the remaining ones as training
for (k in seq(1:kfolds)) {
  start_index = ((k - 1) * fold_size) + 1
  end_index = min((k * fold_size), nrow(accuracy_matrix))
  test = accuracy_matrix[rows_shuffled[start_index:end_index],]
  train = accuracy_matrix[-rows_shuffled[start_index:end_index],]
  # Fit factor analysis to the training folds with increasing number of factors and record BIC
  for(i in seq(1:max_factors)) {
    fa_i = fa(r = train, nfactors = i, rotate = 'none', fm = 'ml', scores = 'regression')
    bic_df = bind_rows(bic_df, data.frame(fold = k, factors = i, bic = fa_i[['BIC']]))
  }
}
bic_df



# > FIGURE: Number of factors ----

fig_num_factors = bic_df %>%
  ggplot(aes(x = factors, y = bic, color = as.factor(fold))) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    name = 'number of factors',
    breaks = seq(1:6)
  ) +
  scale_y_continuous(
    name = 'BIC'
  ) +
  default_plot_theme +
  theme(
    legend.position = 'none'
  )

fig_num_factors
ggsave(paste(FIGURES_DIR, 'BIC_NumFactors_RAW.pdf', sep = '/'),
       fig_num_factors,
       width = 6,
       height = 8,
       device = cairo_pdf
)


# > Factor analysis MSE comparison ----

# Run factor analysis fits for test, question type, graph type
bootstrap_iters = 10000
accuracy_matrix_scaled = get_normalized_data(accuracy_matrix)
model_comparison = data.frame()
model_vars = c('test', 'q_category', 'graph_type')

for(i in seq(1:length(model_vars))) {
  model_var = model_vars[i]
  n_factors = n_distinct(test_item_lookup[model_var])
  print(paste('Running model', i, 'with factor variable:', model_var, '<', n_factors, 'factors >'))
  loading_matrix = init_loading_matrix(model_var, test_item_lookup)
  model_predictions = factor_analysis(accuracy_matrix, loading_matrix)
  model_error = rowMeans((accuracy_matrix_scaled - model_predictions)^2)
  model_error_bootstrap = replicate(n = bootstrap_iters,
                                    expr = mean(sample_n(as_tibble(model_error),
                                                         length(model_error),
                                                         replace = TRUE)$value)
                                    )

  model_comparison = rbind(
    model_comparison,
    data.frame(
      'model' = model_var,
      'factors' = n_factors,
      'n' = length(model_error),
      'MSE' = mean(model_error),
      'lb' = quantile(model_error_bootstrap, c(0.025)),
      'ub' = quantile(model_error_bootstrap, c(0.975))
    )
  )
}
model_comparison


# Run factor analysis for bottom up model

# Shuffle participant rows for train / test folds
rows = seq(1:nrow(accuracy_matrix))
rows_shuffled = sample(rows)
kfolds = 5
fold_size = ceiling(nrow(accuracy_matrix) / kfolds)
n_factors = 4
error_df = data.frame()
# Run k-fold factor analysis (fit to train, evaluate error on test)
for (k in seq(1:kfolds)) {
  print(paste('Running factor analysis for fold:', k))
  start_index = ((k - 1) * fold_size) + 1
  end_index = min((k * fold_size), nrow(accuracy_matrix))
  test = accuracy_matrix[rows_shuffled[start_index:end_index],]
  train = accuracy_matrix[-rows_shuffled[start_index:end_index],]
  # Run factor analysis on training folds
  fa_train = fa(
    r = train,
    rotate = 'none',
    fm = 'ml',
    scores = 'regression',
    nfactors = n_factors
  )
  # Calculate error on test folds
  preds_test = factor_analysis(test, fa_train$loadings)
  error_test = (get_normalized_data(test) - preds_test)^2
  error_df = rbind(error_df, error_test)
}

subject_error = rowMeans(error_df)
subject_error_bootstrap = replicate(n = bootstrap_iters,
                                    expr = mean(sample_n(as_tibble(subject_error),
                                                         length(subject_error),
                                                         replace = TRUE)$value)
                                    )

model_comparison = rbind(
  model_comparison,
  data.frame(
    'model' = 'latent_factor',
    'factors' = n_factors,
    'n' = length(subject_error),
    'MSE' = mean(subject_error),
    'lb' = quantile(subject_error_bootstrap, c(0.025)),
    'ub' = quantile(subject_error_bootstrap, c(0.975))
  )
)
model_comparison



# > FIGURE: Factor analysis MSE comparison ----

model_comparison$model = factor(
  model_comparison$model,
  levels = c('test', 'q_category', 'graph_type', 'latent_factor')
)

fig_mse_comparison = model_comparison %>%
  ggplot(aes(x = model, y = MSE, fill = model)) +
  geom_bar(stat = 'identity', width = 0.8) +
  geom_errorbar(aes(ymin = lb, ymax = ub),
                width = 0, linewidth = 1) +
  scale_x_discrete(
    name = element_blank(),
    labels = c('test' = 'test \ntype \nm=2',
               'q_category' = 'question \ntype \nm=11',
               'graph_type' = 'graph \ntype \nm=13',
               'latent_factor' = 'latent \nfactor \nm=4')
  ) +
  scale_y_continuous(
    name = 'mean squared error',
    breaks = c(0, 5, 10, 15),
    labels = c(0, 5, 10, 15),
    limits = c(0, 15)
  ) +
  scale_fill_manual(
    name = element_blank(),
    values = c(
      'test' = '#e8e1ed',
      'q_category' = '#d1c3dc',
      'graph_type' = '#baa6cb',
      'latent_factor' = '#a388ba'
    )
  ) +
  default_plot_theme +
  theme(
    legend.position = 'None',
  )

fig_mse_comparison
ggsave(paste(FIGURES_DIR, 'Modelfit_Factors_RAW.pdf', sep = '/'),
       fig_mse_comparison,
       width = 6,
       height = 6,
       device = cairo_pdf
)



# > FIGURE: Factor analysis loadings ----


hierarchicalCategoriesLevels = c(
  'GGR, bar chart, level 1, item 1',
  'GGR, bar chart, level 2, item 2',
  'GGR, bar chart, level 3, item 10',
  'GGR, bar chart, level 3, item 13',

  'GGR, line chart, level 1, item 5',
  'GGR, line chart, level 2, item 6',
  'GGR, line chart, level 3, item 7',
  'GGR, line chart, level 3, item 11',
  'GGR, line chart, level 3, item 12',

  'GGR, pie chart, level 1, item 3',
  'GGR, pie chart, level 2, item 4',

  'GGR, icon array, level 1, item 8',
  'GGR, icon array, level 2, item 9',

  'VLAT, bar chart, determine range, item 21',
  'VLAT, bar chart, find extremum, item 20',
  'VLAT, bar chart, make comparisons, item 22',
  'VLAT, bar chart, retrieve value, item 19',

  'VLAT, stacked bar chart, find extremum, item 25',
  'VLAT, stacked bar chart, make comparisons, item 27',
  'VLAT, stacked bar chart, make comparisons, item 28',
  'VLAT, stacked bar chart, retrieve value, item 23',
  'VLAT, stacked bar chart, retrieve value, item 24',

  'VLAT, normalized stacked bar chart, find extremum, item 30',
  'VLAT, normalized stacked bar chart, make comparisons, item 31',
  'VLAT, normalized stacked bar chart, retrieve value, item 29',

  'VLAT, line chart, determine range, item 16',
  'VLAT, line chart, find extremum, item 15',
  'VLAT, line chart, find correlations trends, item 17',
  'VLAT, line chart, make comparisons, item 18',
  'VLAT, line chart, retrieve value, item 14',

  'VLAT, area chart, determine range, item 49',
  'VLAT, area chart, find extremum, item 48',
  'VLAT, area chart, find correlations trends, item 50',
  'VLAT, area chart, retrieve value, item 47',

  'VLAT, stacked area chart, find extremum, item 54',
  'VLAT, stacked area chart, find correlations trends, item 56',
  'VLAT, stacked area chart, make comparisons, item 57',
  'VLAT, stacked area chart, make comparisons, item 58',
  'VLAT, stacked area chart, retrieve value, item 52',
  'VLAT, stacked area chart, retrieve value, item 53',

  'VLAT, scatterplot, determine range, item 41',
  'VLAT, scatterplot, find extremum, item 40',
  'VLAT, scatterplot, find anomolies, item 43',
  'VLAT, scatterplot, find clusters, item 44',
  'VLAT, scatterplot, find correlations trends, item 45',
  'VLAT, scatterplot, make comparisons, item 46',
  'VLAT, scatterplot, retrieve value, item 39',

  'VLAT, bubble chart, determine range, item 61',
  'VLAT, bubble chart, find extremum, item 60',
  'VLAT, bubble chart, find anomolies, item 63',
  'VLAT, bubble chart, find clusters, item 64',
  'VLAT, bubble chart, find correlations trends, item 65',
  'VLAT, bubble chart, make comparisons, item 66',
  'VLAT, bubble chart, retrieve value, item 59',

  'VLAT, histogram, find extremum, item 36',
  'VLAT, histogram, retrieve value, item 35',
  'VLAT, histogram, characterize distribution, item 38',

  'VLAT, pie chart, find extremum, item 33',
  'VLAT, pie chart, make comparisons, item 34',
  'VLAT, pie chart, retrieve value, item 32',

  'VLAT, choropleth, find extremum, item 68',
  'VLAT, choropleth, make comparisons, item 69',
  'VLAT, choropleth, retrieve value, item 67',

  'VLAT, treemap, find extremum, item 71',
  'VLAT, treemap, find correlations trends, item 73',
  'VLAT, treemap, make comparisons, item 72'
)

# Run latent factor model fit with rotation
fa_rotated = fa(
  r = accuracy_matrix,
  # oblimin roation makes the loading more orthogonal to one another
  rotate = 'oblimin', # see options: https://www.rdocumentation.org/packages/psych/versions/2.2.9/topics/fa
  fm = 'ml',
  scores = 'regression',
  nfactors = 4
)
L_rotated = fa_rotated$loadings



L_rotated_df = data.frame(
    Item = rownames(L_rotated),
    factor1 = L_rotated[,1],
    factor2 = L_rotated[,2],
    factor3 = L_rotated[,3],
    factor4 = L_rotated[,4]
  ) %>%
  inner_join(
    test_item_lookup,
    by = c('Item')
  ) %>%
  mutate(
    HierarchicalCategories = factor(paste(test,
                                          gsub('_', ' ', graph_type, fixed = TRUE),
                                          gsub('_', ' ', q_category, fixed = TRUE),
                                          gsub('_', ' ', Item, fixed = TRUE),
                                          sep = ', '
                                          ),
                                    levels = rev(hierarchicalCategoriesLevels))
  )


L_rotated_long = L_rotated_df %>%
  pivot_longer(
    cols = c('factor1', 'factor2', 'factor3', 'factor4'),
    names_to = 'Factors',
    values_to = 'value')


colMain = colorRampPalette(brewer.pal(8, 'Greys'))(length(unique(L_rotated_long$HierarchicalCategories)))

fig_factor_loadings = L_rotated_long %>%
  ggplot(aes(x = Factors, y = HierarchicalCategories, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = colMain) +
  # geom_text(aes(label = round(value, 2)), size = 1.75) +
  default_plot_theme +
  scale_x_discrete(
    name = element_blank()
  ) +
  scale_y_discrete(
    name = element_blank(),
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 16, angle = -90),
  )

fig_factor_loadings
ggsave(paste(FIGURES_DIR, 'Factors_HierarchicalCategories_RAW_full.pdf', sep = '/'),
       fig_factor_loadings,
       width = 6,
       height = 8,
       device = cairo_pdf
)





# > REVISED FIGURE: Factor analysis loadings with numbers ----

fig_factor_loadings = L_rotated_long %>%
  ggplot(aes(x = Factors, y = HierarchicalCategories, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = 'white',
    high = 'darkgrey',
    # colors = colMain
  ) +
  geom_text(
    aes(label = round(value, 2)),
    size = 2,
    fontface = 'bold'
  ) +
  default_plot_theme +
  scale_x_discrete(
    name = element_blank()
  ) +
  scale_y_discrete(
    name = element_blank(),
    # expand = expansion(mult = c(0.1, 0.1))
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 12, angle = -45),
    legend.position = 'none'
  )

fig_factor_loadings
ggsave(paste(FIGURES_DIR, 'FactorLoadingsSupplement.pdf', sep = '/'),
       fig_factor_loadings,
       width = 6,
       height = 10,
       device = cairo_pdf
)

