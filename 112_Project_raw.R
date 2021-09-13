library(tidyverse)
library(readr)
library(ggplot2)
library(pander)
library(GGally)
library(nlme)
library(mgcv)
library(lme4)
library(ggthemes)
aids <- read.csv("aids.csv")

########## EXPLORATORY DATA ANALYSIS ##########

# Convert Treatment variable into factor

aids <- aids %>% 
  mutate(treatment = as.factor(treatment),
         gender = as.factor(gender))
aids <- aids %>% 
  mutate(week_sq = week^2) %>% 
  relocate(week_sq, .after = week)

# Plots

aids %>% 
  group_by(id) %>% 
  summarize(mean_age = mean(age)) %>% 
  ggplot(aes(x = mean_age)) +
  geom_histogram(bins = 77*11, fill = "purple") +
  theme_clean() +
  scale_x_continuous(breaks = seq(0,75,5)) +
  scale_y_continuous(breaks = seq(0,13,1))+
  labs(x = "Age", y = "Frequency/Count", title = "Histogram of Subjects' Age")

newdf <- aids %>% 
  mutate(week0 = ifelse(week == 0, log_cd4, NA)) 

newdf %>% 
  ggplot(aes(x = log_cd4, y = treatment, fill = treatment)) +
  geom_boxplot() +
  theme_clean() +
  labs(x = "Log CD4 at week 0", y = "Treatment", title = "Log CD4 at week 0 for each treatment")

aids %>% 
  ggplot(aes(x = factor(treatment), fill = factor(gender))) +
  geom_bar(position = "dodge2") +
  labs(x = "Treatment", fill = "Gender")

aids %>% 
  ggplot(aes(x = week, y = log_cd4, color = factor(treatment))) +
  geom_point() +
  theme_clean()+
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Week", y = "Log(CD4)", title = "Scatterplot of Log CD4 Over Time")

ggpairs(select(aids, -id))


########## MODEL #########################################################

model_linear <- lme(log_cd4 ~ age + gender + week + age:treatment + gender:treatment + week:treatment,
                        data = aids,
                        random = ~ 1 | id,
                        method = "ML")

# We can test if age, gender, age:treatment, gender:treatment, or week:treatment are individually significant

model_at <- lme(log_cd4 ~ gender + week + gender:treatment + week:treatment + age:treatment,
                         data = aids,
                         random = ~ 1 | id,
                         method = "ML")

model_gt <- lme(log_cd4 ~ age + week + age:treatment + gender:treatment + week:treatment,
                    data = aids,
                    random = ~ 1 | id,
                    method = "ML")

model_w <- lme(log_cd4 ~ week + age:treatment + gender:treatment + week:treatment,
                            data = aids,
                            random = ~ 1 | id,
                            method = "ML")

anova(model_linear, model_at, model_gt, model_w)
# Although log-likelihood says model_at is best and disagrees with AIC, we will continue with model_linear as we add more covariates
# Summary: We will use the full model of log_cd4 ~ age + gender + week + age:treatment + gender:treatment + week:treatment

summary(model_linear_full)

########## Nonlinear Relationship of log(CD4) over Time #####################

aids %>% 
  ggplot(aes(x = week, y = log_cd4, color = factor(treatment))) +
  theme_clean()+
  scale_color_manual(values=c("pink","orchid","purple", "deeppink4"))+
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Week", y = "Log(CD4)", title = "Scatterplot of Log CD4 Over Time")

# We can see from this graph, that a possible knot term or quadratic could be implemented at week = 10.

# After much experiment, these are 3 models that include a knot term or a quadratic term

aids <- aids %>% 
  mutate(knot_term = if_else(week - 10 > 10, week - 10, 0))

aids <- aids %>% 
  mutate(log_week = ifelse(week == 0, 0, log(week)),
         week_sqr = week^2,
         log_week_sqr = log_week^2,
         knot_term = if_else(week - 10 > 10, week - 10, 0))

# Five Models
#model_linear
#model_piecewise
#model_picewise2 (interaction)
#model_quad
#model_quad2 (interaction)

model_piecewise <- lme(log_cd4 ~ age + gender + week + knot_term + age:treatment + gender:treatment + week:treatment,
                data = aids,
                random = ~ 1 | id,
                method = "ML")

model_piecewise2 <- lme(log_cd4 ~ age + gender + week + knot_term + age:treatment + gender:treatment + week:treatment + knot_term:treatment,
                        data = aids,
                        random = ~ 1 | id,
                        method = "ML")

model_quad <- lme(log_cd4 ~ age + gender + week + week_sq + age:treatment + gender:treatment + week:treatment,
                       data = aids,
                       random = ~ 1 | id,
                       method = "ML")

model_quad2 <- lme(log_cd4 ~ age + gender + week + week_sq + age:treatment + gender:treatment + week:treatment + week_sq:treatment,
                  data = aids,
                  random = ~ 1 | id,
                  method = "ML")

# Now we will use anova() to compare these models:
anova(model_linear, model_piecewise, model_piecewise2, model_quad, model_quad2)

# We see that 
# 1) logLik_quad2 > logLik_quad > logLik_linear
# 2) logLik_piecewise2 > logLik_piecewise > logLik_linear
# 3) AIC_quad2 < AIC_piecewise2

# model_quad2 is preferred and the model we should use
summary(model_quad2)

########## Random Effects #########################################################
model_quad_a <- lme(log_cd4 ~ age + gender + week + week_sq + age:treatment + gender:treatment + week:treatment + week_sq:treatment,
                  data = aids,
                  random = ~ 1 + age | id,
                  method = "ML")

model_quad_g <- lme(log_cd4 ~ age + gender + week + week_sq + age:treatment + gender:treatment + week:treatment + week_sq:treatment,
                  data = aids,
                  random = ~ 1 + gender | id,
                  method = "ML")

model_quad_w <- lme(log_cd4 ~ age + gender + week + week_sq + age:treatment + gender:treatment + week:treatment + week_sq:treatment,
                  data = aids,
                  random = ~ 1 + week | id,
                  method = "ML")

model_quad_w2 <- lme(log_cd4 ~ age + gender + week + week_sq + age:treatment + gender:treatment + week:treatment + week_sq:treatment,
                  data = aids,
                  random = ~ 1 + week_sq | id,
                  method = "ML")


anova(model_quad2, model_quad_a, model_quad_g, model_quad_w, model_quad_w2)

# model_quad_w (random effect on week) is preferred (AIC lowest)

model_quad2_no_age <- lme(log_cd4 ~ gender + week + week_sq + age:treatment + gender:treatment + week:treatment + week_sq:treatment,
                          data = aids,
                          random = ~ 1 | id,
                          method = "ML")

anova(model_quad2, model_quad2_no_age)
# AIC_model_quad2 = AIC_model_quad2_no_age
# logLik_model_quad2 = logLik_model_quad2_no_age
# Both models acceptable. We will continue with model_quad2 for continuity's sake

########## Chosen Model #########################################################

summary(model_quad_w)

#create a data frame for Y_hats along with ids (using id = 469, id = 1172)
tibble(id = names(fitted(model_quad_w)),
       week = aids$week,
       fitted_values = fitted(model_quad_w),
       observed = aids$log_cd4) %>% 
  View()

# RESIDUAL ANALYSIS -------------------------------------------------------

#Transform residuals--------------------------------------
#r_i = Y_i - X_i beta_hat (marginal (population) residual)
res_population <- residuals(model_quad_w, type = "response", level = 0)

# transformed residuals
Sigma_i <- extract.lme.cov(model_quad_w, aids)
L_i <- t(chol(Sigma_i)) # block matrix of lower triangular Cholesky factors
res_transformed <- solve(L_i) %*% res_population

# Histogram of untransformed residuals
tibble(r = res_population) %>% 
  ggplot(aes(x = r)) +
  theme_clean()+
  geom_histogram(aes(y = stat(density)), bins = 14, color = "black", fill = "gray") +
  labs(x = "Residuals", y = "Density", title = "Histogram of Untransformed Residuals")

# Histogram of transformed residuals
tibble(r_star = res_transformed) %>% 
  ggplot(aes(x = r_star)) +
  theme_clean() +
  geom_histogram(aes(y = stat(density)), bins = 14, color = "black", fill = "gray") +
  geom_function(fun = dnorm, color = "purple") +
  labs(x = "Transformed Residuals", y = "Density", title = "Histogram of Transformed Residuals")
# Looking at the histogram of Transformed Residuals, we see that they approximately follow a normal distribution

# QQ Plot -----------------------------------------------------------------

# QQ plot for untransformed residual

# QQ plot for transformed residual
tibble(r_star = res_transformed) %>% 
  ggplot(aes(sample = r_star)) +
  theme_clean() +
  geom_qq_line(color = "purple") +
  geom_qq(shape = 1) +
  labs(x = "Quantiles of Standard Normal", y = "Quantiles of Transformed Residuals")
# Must look at other graphs to make conclusion
# Normality assumption is not met, departs noticeably from straight line
# ? Can attribute the heavy tails to the fact that experiment is randomized
# ? It is expected that we would have large residuals at baseline

# Mahalanobis Distance (d_i) ----------------------------------------------

mahalanobis_data <- tibble(id = aids$id, r_star = res_transformed) %>% 
  group_by(id) %>% 
  nest()

mahalanobis_data <- mahalanobis_data %>% 
  mutate(df = map_dbl(data, ~nrow(.x)))

mahalanobis_distance <- function(x){
  x <- as.matrix(x)
  t(x) %*% x
}

mahalanobis_data <- mahalanobis_data %>% 
  mutate(d = map_dbl(data, ~mahalanobis_distance(.x)))

mahalanobis_data <- mahalanobis_data %>% 
  mutate(p_value = pchisq(d, df, lower.tail = FALSE))

mahalanobis_data %>% 
  arrange(p_value) %>% 
  filter(p_value <= .05)

# 134 outliers < Expected number of outliers
# We expect 252 outliers (0.05*5036 = 251.8)
# We can attribute these outliers to random chance


# Scatter Plots (Residuals ~ Predicted Value (Time)) ----------------------------------------------------

time <- aids$week
time_transformed <- solve(L_i) %*% time

tibble(x = time_transformed, y = res_transformed) %>% 
  ggplot(aes(x = x, y = y)) +
  theme_clean()+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(shape = 1) +
  geom_smooth(method = "loess", se = FALSE, col = "purple") +
  labs(x = "Transformed Time", y = "Transformed Residual")
# Scatterplot fluctuates around 0 quite well
# Suggests assumption of constant variance is adequate
# Solidifies our belief that quadratic/knot term is needed

# Absolute Transformed Residuals------------------------------------------

mu_hat <- fitted(model_quad_w, level = 0)
mu_hat_transformed <- solve(L_i) %*% mu_hat
abs_res_transformed <- abs(res_transformed)

# Absolute Transformed Residuals on Transformed Predicted Values
tibble(x = mu_hat_transformed, y = abs_res_transformed) %>% 
  ggplot(aes(x = x, y = y)) +
  theme_clean()+
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_point(shape = 1) +
  geom_smooth(method = "loess", se = FALSE, col = "purple") +
  labs(x = "Transformed Predicted Value", y = "Absolute Transformed Residual")
# Scatterplot fluctuates around 0.8 quite well
# No systematic departure from 0.8 suggests transformed residuals follow N(0,1)


# Absolute Transformed Residuals on Transformed Time
tibble(x = time_transformed, y = abs_res_transformed) %>% 
  ggplot(aes(x = x, y = y)) +
  theme_clean() +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_point(shape = 1) +
  geom_smooth(method = "loess", se = FALSE, col = "purple") +
  labs(x = "Transformed Time", y = "Absolute Transformed Residual")
# Scatterplot fluctuates around 0.8 quite well
# No systematic departure from 0.8 suggests transformed residuals follow N(0,1)

# Semi_Variogram ----------------------------------------------------------

Variogram(model_quad_w,
          data = aids,
          form = ~ week | id,
          resType = "normalized") %>% 
  as_tibble() %>% 
  ggplot(aes(x = dist, y = variog)) +
  theme_clean() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(shape = 1) +
  geom_smooth(method = "loess", se = FALSE, span = 0.3, col = "purple") +
  labs(x = "Dist", y = "Variogram", title = "Variogram with span = 0.3")
# Variogram fluctuates around 1, with no systematic trend
# Conclude covariance is adequate for the model (and variance and correlation)

# GLME --------------------------------------------------------------------


aids$count_cd4 <- round(exp(aids$log_cd4) - 1)

aids %>% 
  ggplot(aes(x = week, y = count_cd4, color = factor(treatment))) +
  theme_clean()+
  scale_color_manual(values=c("pink","orchid","purple", "deeppink4"))+
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Week", y = "Count of CD4", title = "Scatterplot of CD4 Over Time")

glme_model <- glmer(count_cd4 ~ age + week + week_sq + week:treatment + week_sq:treatment + (1 + week | id),
                    data = aids,
                    family = poisson,
                    nAGQ = 0,
                    na.action = na.omit)

summary(glme_model) # unable to get main effect of gender due to model not converging, so we use random effect of gender as substitute

# Wald test (are there differences among different treatments over time?)
# week:treatment (different treatments have differing effects over time)
# week_sq:treatment (rates of change of treatments change over time)

#week:treatment
L <- matrix(
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
    ncol = 10,
  nrow = 3
)

glme_beta_hat <- summary(glme_model)$coefficients[, 1]
glme_cov_beta_hat <- vcov(glme_model) %>% as.matrix()

wald_stat_glme<- t(L %*% glme_beta_hat) %*% solve(L %*% glme_cov_beta_hat %*% t(L)) %*% (L %*% glme_beta_hat)
wald_stat_glme # test stat = 121,3127
pchisq(wald_stat_glme, df = nrow(L), lower.tail = FALSE)
# p = 4.024577e-26; reject the null, treatments are different over time

#week_sq:treatment

L_2 <- matrix(
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
  ncol = 10,
  nrow = 3
)

wald_stat_glme_sqr <- t(L_2 %*% glme_beta_hat) %*% solve(L_2 %*% glme_cov_beta_hat %*% t(L_2)) %*% (L_2 %*% glme_beta_hat)
wald_stat_glme_sqr # test stat = 593.125
pchisq(wald_stat_glme_sqr, df = nrow(L), lower.tail = FALSE)
# p = 3.11733e-128; reject the null, rates of change of treatments are different over time

tibble(id = names(fitted(glme_model)),
       week = aids$week,
       fitted_values = fitted(glme_model),
       observed = aids$count_cd4) %>%
  View()