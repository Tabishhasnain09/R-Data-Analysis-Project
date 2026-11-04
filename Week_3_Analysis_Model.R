# ==================================================================
# Week 3: Statistical Analysis and Predictive Modeling Using R
# Dataset: Coffee Quality Institute ratings (cleaned in Week 1)
# Task   : regression - predict total_cup_points
# Run    : open in RStudio, set the working directory to the folder that
#          contains coffee_ratings_clean.csv (Session > Set Working
#          Directory > To Source File Location) and run the whole script.
# ==================================================================

# 1. Packages -------------------------------------------------------
# Install once (only if missing):
# install.packages(c("ggplot2", "dplyr", "tidyr", "scales", "caret",
#                    "glmnet", "ranger", "car", "lmtest", "broom", "corrplot"))
needed  <- c("ggplot2", "dplyr", "tidyr", "scales", "caret", "glmnet",
             "ranger", "car", "lmtest", "broom", "corrplot")
missing <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) stop("Please install: ", paste(missing, collapse = ", "))

library(ggplot2)    # charts
library(dplyr)      # data preparation
library(tidyr)      # reshaping data
library(caret)      # data split, cross-validation, model training (uses glmnet and ranger)
library(corrplot)   # correlation plot

set.seed(2026)      # one seed for the whole project (reset before each random step)

# 2. Folders and plotting style --------------------------------------
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

cb <- c(blue = "#0072B2", orange = "#E69F00", green = "#009E73",
        vermillion = "#D55E00", sky = "#56B4E9", grey = "#999999")
theme_set(theme_minimal(base_size = 12) +
          theme(plot.title = element_text(face = "bold"),
                panel.grid.minor = element_blank(),
                plot.background = element_rect(fill = "white", colour = NA)))
save_plot <- function(p, file, w = 8, h = 5) {
  ggsave(file.path("outputs/figures", file), plot = p, width = w, height = h, dpi = 300, bg = "white")
}

# 3. Import the cleaned Week 1 dataset ---------------------------------
data_file <- "coffee_ratings_clean.csv"
if (!file.exists(data_file)) stop("Place ", data_file, " in the working directory: ", getwd())
coffee <- read.csv(data_file, stringsAsFactors = FALSE)

dim(coffee)
names(coffee)

# 4. Inspect the data ---------------------------------------------------
str(coffee, vec.len = 2, nchar.max = 30)
sum(is.na(coffee))             # missing values
sum(duplicated(coffee))        # duplicate rows
summary(coffee$total_cup_points)
table(coffee$altitude_imputed)
table(coffee$moisture_imputed)

# 5. Prepare the modelling table ----------------------------------------
# The ten sensory scores add up to the target (total_cup_points), so they are
# NOT used as predictors. Only origin, processing, lot and quality-control
# variables are used.
coffee$species           <- relevel(factor(coffee$species), ref = "Arabica")
coffee$processing_method <- relevel(factor(coffee$processing_method), ref = "Washed / Wet")
coffee$variety_group     <- relevel(factor(coffee$variety_group), ref = "Caturra")
coffee$country_of_origin <- factor(coffee$country_of_origin)

# Week 1 filled in some altitudes and moistures with medians of the WHOLE data.
# To avoid using information from the test set, those filled-in values are set
# back to NA and are imputed again later using training data only.
model_data <- data.frame(
  total_cup_points  = coffee$total_cup_points,
  species           = coffee$species,
  country_of_origin = coffee$country_of_origin,
  processing_method = coffee$processing_method,
  variety_group     = coffee$variety_group,
  altitude_mean_meters = ifelse(coffee$altitude_imputed, NA, coffee$altitude_mean_meters),
  moisture          = ifelse(coffee$moisture_imputed, NA, coffee$moisture),
  total_defects     = coffee$total_defects,
  quakers           = coffee$quakers,
  grading_year      = coffee$grading_year,
  log_bags          = log1p(coffee$number_of_bags)
)

dim(model_data)
colSums(is.na(model_data))
sapply(model_data, function(x) class(x)[1])

# 6. Train / test split (80% / 20%) ---------------------------------------
set.seed(2026)
train_idx <- createDataPartition(model_data$total_cup_points, p = 0.8, list = FALSE)
train <- model_data[train_idx, ]
test  <- model_data[-train_idx, ]

# Rare countries (fewer than 20 TRAINING lots) are grouped as "Other".
# The rule uses training counts only; the test set follows the same rule.
ctry_counts <- sort(table(train$country_of_origin), decreasing = TRUE)
keep_ctry   <- names(ctry_counts)[ctry_counts >= 20]
group_country <- function(x) factor(ifelse(as.character(x) %in% keep_ctry, as.character(x), "Other"),
                                    levels = c(keep_ctry, "Other"))
train$country_grp <- group_country(train$country_of_origin)
test$country_grp  <- group_country(test$country_of_origin)
train$country_of_origin <- NULL
test$country_of_origin  <- NULL

# Test factors use exactly the training levels
for (v in c("species", "processing_method", "variety_group"))
  test[[v]] <- factor(test[[v]], levels = levels(train[[v]]))

c(train = nrow(train), test = nrow(test))
data.frame(set = c("train", "test"),
           mean = round(c(mean(train$total_cup_points), mean(test$total_cup_points)), 2),
           sd   = round(c(sd(train$total_cup_points), sd(test$total_cup_points)), 2))
table(train$country_grp)

# 7. Exploratory analysis (training data only) ----------------------------
skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3
y <- train$total_cup_points
round(c(n = length(y), mean = mean(y), median = median(y), sd = sd(y), min = min(y),
        q1 = unname(quantile(y, .25)), q3 = unname(quantile(y, .75)), max = max(y),
        skewness = skewness(y)), 2)

num_vars <- c("altitude_mean_meters", "moisture", "total_defects", "quakers",
              "grading_year", "log_bags")
desc <- data.frame(variable = num_vars,
  n_missing = sapply(train[num_vars], function(x) sum(is.na(x))),
  mean   = sapply(train[num_vars], mean, na.rm = TRUE),
  median = sapply(train[num_vars], median, na.rm = TRUE),
  sd     = sapply(train[num_vars], sd, na.rm = TRUE),
  min    = sapply(train[num_vars], min, na.rm = TRUE),
  max    = sapply(train[num_vars], max, na.rm = TRUE))
rownames(desc) <- NULL
desc[, -(1:2)] <- round(desc[, -(1:2)], 2)
desc

round(prop.table(table(train$processing_method)) * 100, 1)
round(prop.table(table(train$species)) * 100, 1)

p1 <- ggplot(train, aes(x = total_cup_points)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = cb[["sky"]], colour = "white") +
  geom_density(colour = cb[["blue"]], linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(y), sd = sd(y)),
                colour = cb[["vermillion"]], linetype = "dashed", linewidth = 0.9) +
  labs(title = "Total cup points (training data)",
       subtitle = "Solid = estimated density, dashed = normal curve with the same mean and SD",
       x = "Total cup points", y = "Density")
save_plot(p1, "fig01_target_distribution.png")

box_levels <- names(which(table(train$processing_method) >= 30))
box_df <- subset(train, processing_method %in% box_levels)
box_df$processing_method <- droplevels(box_df$processing_method)
box_df$label <- paste0(box_df$processing_method, "\n(n=", ave(box_df$total_cup_points, box_df$processing_method, FUN = length), ")")
p2 <- ggplot(box_df, aes(x = reorder(label, total_cup_points, median), y = total_cup_points)) +
  geom_boxplot(fill = cb[["sky"]], outlier.colour = cb[["vermillion"]], outlier.size = 1.2) +
  labs(title = "Total cup points by processing method", x = NULL, y = "Total cup points")
save_plot(p2, "fig02_box_processing.png")

alt_df <- train[!is.na(train$altitude_mean_meters), ]
p3 <- ggplot(alt_df, aes(x = altitude_mean_meters, y = total_cup_points)) +
  geom_point(alpha = 0.35, colour = cb[["blue"]]) +
  geom_smooth(method = "lm", se = FALSE, colour = cb[["vermillion"]]) +
  geom_smooth(method = "loess", se = FALSE, colour = cb[["orange"]], linetype = "dashed") +
  labs(title = "Altitude and total cup points (lots with recorded altitude)",
       subtitle = "Solid = straight line, dashed = flexible (loess) curve",
       x = "Mean farm altitude (metres)", y = "Total cup points")
save_plot(p3, "fig03_scatter_altitude.png")

ctry_ci <- train %>%
  filter(country_grp != "Other") %>%
  group_by(country_grp) %>%
  summarise(n = n(), mean = mean(total_cup_points),
            half = qt(0.975, n() - 1) * sd(total_cup_points) / sqrt(n()), .groups = "drop")
p4 <- ggplot(ctry_ci, aes(x = mean, y = reorder(country_grp, mean))) +
  geom_vline(xintercept = mean(y), linetype = "dashed", colour = "grey40") +
  geom_errorbarh(aes(xmin = mean - half, xmax = mean + half), height = 0.25, colour = cb[["blue"]]) +
  geom_point(size = 2.5, colour = cb[["blue"]]) +
  labs(title = "Mean total cup points by country (95% CI)",
       subtitle = "Countries with at least 20 training lots; dashed line = overall mean",
       x = "Mean total cup points", y = NULL)
save_plot(p4, "fig04_country_means.png")

cor_cols <- c("total_cup_points", num_vars)
cor_m <- cor(train[cor_cols], method = "spearman", use = "pairwise.complete.obs")
png("outputs/figures/fig05_correlation.png", width = 7, height = 7, units = "in", res = 300, bg = "white")
corrplot(cor_m, method = "color", type = "upper",
         col = colorRampPalette(c(cb[["vermillion"]], "white", cb[["blue"]]))(200),
         addCoef.col = "black", number.cex = 0.8, tl.col = "black", tl.srt = 45)
invisible(dev.off())
round(cor_m[, "total_cup_points"], 2)

# 8. Hypothesis tests (training data, alpha = 0.05) -------------------------
# ---- Test 1: altitude and score ---------------------------------------
# H0: no monotonic association (rho = 0)   H1: rho != 0
nrow(alt_df)
shapiro.test(alt_df$total_cup_points)            # normality check of the score
t1_sp <- cor.test(alt_df$altitude_mean_meters, alt_df$total_cup_points,
                  method = "spearman", exact = FALSE)
t1_sp
t1_pe <- cor.test(alt_df$altitude_mean_meters, alt_df$total_cup_points, method = "pearson")
t1_pe$estimate; t1_pe$conf.int
t1_sp$estimate^2          # share of rank variation shared

# ---- Test 2: Washed vs Natural processing ---------------------------------
# H0: scores have the same centre in both groups   H1: they differ
two <- subset(train, processing_method %in% c("Washed / Wet", "Natural / Dry"))
two$processing_method <- droplevels(two$processing_method)
g <- split(two$total_cup_points, two$processing_method)
sapply(g, length)
sapply(g, function(x) round(c(mean = mean(x), median = median(x), sd = sd(x)), 2))
sapply(g, function(x) round(shapiro.test(x)$statistic, 3))   # normality in each group

t2_w <- wilcox.test(g[[1]], g[[2]], conf.int = TRUE, exact = FALSE)   # primary test
t2_w
t2_t <- t.test(g[[1]], g[[2]])                                        # sensitivity check (Welch)
t2_t

pooled_sd <- sqrt(((length(g[[1]]) - 1) * var(g[[1]]) + (length(g[[2]]) - 1) * var(g[[2]])) /
                  (length(g[[1]]) + length(g[[2]]) - 2))
round((mean(g[[1]]) - mean(g[[2]])) / pooled_sd, 3)                    # Cohen's d
round(2 * unname(t2_w$statistic) / (length(g[[1]]) * length(g[[2]])) - 1, 3)   # rank-biserial r

# ---- Test 3: differences between countries --------------------------------
# H0: all countries have the same score distribution   H1: at least one differs
big <- subset(train, country_grp != "Other")
big$country_grp <- droplevels(big$country_grp)
nrow(big); nlevels(big$country_grp)

t3 <- kruskal.test(total_cup_points ~ country_grp, data = big)
t3
round(unname(t3$statistic) / (nrow(big) - 1), 3)      # epsilon-squared effect size

# One-way ANOVA on the same data as a cross-check
summary(aov(total_cup_points ~ country_grp, data = big))

# Post-hoc: the four largest countries, Holm-adjusted p-values
top4 <- subset(big, country_grp %in% names(ctry_counts)[1:4])
top4$country_grp <- droplevels(top4$country_grp)
pairwise.wilcox.test(top4$total_cup_points, top4$country_grp, p.adjust.method = "holm", exact = FALSE)

# ---- Test 4: processing method and specialty grade -------------------------
# H0: grade group (below 80 / 80+) is independent of processing method
# H1: they are associated
train$specialty <- factor(ifelse(train$total_cup_points >= 80, "80+", "Below 80"), levels = c("Below 80", "80+"))
chi_df <- subset(train, processing_method %in% names(which(table(train$processing_method) >= 30)))
chi_df$processing_method <- droplevels(chi_df$processing_method)
tab <- table(chi_df$processing_method, chi_df$specialty)
tab
round(prop.table(tab, 1) * 100, 1)

t4 <- chisq.test(tab)
t4
round(t4$expected, 1)                                  # all expected counts should be 5 or more
round(sqrt(unname(t4$statistic) / (sum(tab) * (min(dim(tab)) - 1))), 3)   # Cramer's V
round(t4$stdres, 2)                                    # standardised residuals

# Holm adjustment for the four primary tests
p_raw <- c(altitude = t1_sp$p.value, washed_vs_natural = t2_w$p.value,
           country = t3$p.value, processing_grade = t4$p.value)
signif(p_raw, 3)
signif(p.adjust(p_raw, method = "holm"), 3)

# 9. Modelling inputs ---------------------------------------------------------
# Dummy variables (one reference category dropped per factor). The dummy
# coding is fixed by the training factor levels, so it uses no test information.
predictors <- setdiff(names(train), c("total_cup_points", "specialty"))
dv <- dummyVars(~ ., data = train[predictors], fullRank = TRUE)
x_train <- as.data.frame(predict(dv, train[predictors]))
x_test  <- as.data.frame(predict(dv, test[predictors]))
names(x_train) <- names(x_test) <- make.names(names(x_train))
y_train <- train$total_cup_points
y_test  <- test$total_cup_points

dim(x_train); dim(x_test)
colSums(is.na(x_train))[colSums(is.na(x_train)) > 0]   # NAs are imputed inside each CV fold

# 10. Cross-validation on the training data ---------------------------------------
# 10-fold cross-validation repeated 3 times. Every model uses the SAME folds.
# Imputation (median), centring and scaling are learned inside each fold.
set.seed(2026)
folds <- createMultiFolds(y_train, k = 10, times = 3)
ctrl  <- trainControl(method = "repeatedcv", number = 10, repeats = 3, index = folds)
pre   <- c("zv", "medianImpute", "center", "scale")

set.seed(2026)
m_base <- train(x_train, y_train, method = "null", trControl = ctrl)

set.seed(2026)
m_lm <- train(x_train, y_train, method = "lm", preProcess = pre, trControl = ctrl)

set.seed(2026)
m_enet <- train(x_train, y_train, method = "glmnet", preProcess = pre, trControl = ctrl,
                tuneGrid = expand.grid(alpha = c(0, 0.5, 1),
                                       lambda = 10^seq(-4, -0.5, length.out = 15)))

set.seed(2026)
m_rf <- train(x_train, y_train, method = "ranger", preProcess = "medianImpute", trControl = ctrl,
              tuneGrid = expand.grid(mtry = c(3, 6, 10), splitrule = "variance",
                                     min.node.size = c(5, 15, 30)),
              num.trees = 500, importance = "permutation")

m_enet$bestTune
m_rf$bestTune

resamp <- resamples(list(Baseline = m_base, Linear = m_lm, ElasticNet = m_enet, RandomForest = m_rf))
cv_long <- resamp$values %>%
  select(-Resample) %>%
  pivot_longer(everything(), names_to = c("model", "metric"), names_sep = "~", values_to = "value")
cv_table <- cv_long %>%
  group_by(model, metric) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = metric, values_from = c(mean, sd)) %>%
  arrange(mean_RMSE)
cv_table <- as.data.frame(cv_table)
cv_table[, -1] <- round(cv_table[, -1], 3)
cv_table

# Paired differences in RMSE across the 30 resamples (Bonferroni-adjusted)
summary(diff(resamp, metric = "RMSE"))

best_name <- cv_table$model[1]                 # model with the lowest mean CV RMSE
best_name
final_model <- list(Linear = m_lm, ElasticNet = m_enet, RandomForest = m_rf)[[best_name]]

p6 <- cv_long %>%
  filter(metric == "RMSE") %>%
  mutate(model = factor(model, levels = cv_table$model)) %>%
  ggplot(aes(x = model, y = value)) +
  geom_boxplot(fill = cb[["sky"]], outlier.colour = cb[["vermillion"]]) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Cross-validated RMSE of the four models",
       subtitle = "30 resamples (10 folds x 3 repeats); diamond = mean",
       x = NULL, y = "RMSE (total cup points)")
save_plot(p6, "fig06_cv_rmse.png")

# Sensitivity check (cross-validation only, test set untouched):
# grading_year says WHEN a lot was graded, not what the coffee is. Does the random
# forest still work without it? Same folds and the same tuning values as the best model.
set.seed(2026)
m_rf_noyear <- train(x_train[, names(x_train) != "grading_year"], y_train, method = "ranger",
                     preProcess = "medianImpute", trControl = ctrl,
                     tuneGrid = m_rf$bestTune, num.trees = 500)
round(c(with_year    = mean(m_rf$resample$RMSE),
        without_year = mean(m_rf_noyear$resample$RMSE),
        linear       = mean(m_lm$resample$RMSE),
        baseline     = mean(m_base$resample$RMSE)), 3)
round(c(with_year = mean(m_rf$resample$Rsquared), without_year = mean(m_rf_noyear$resample$Rsquared)), 3)

# 11. One final evaluation on the held-out test set ------------------------------
# The test set is used once, for the model chosen by cross-validation, and for
# the training-mean baseline as a reference. No other model is scored on it.
pred_test <- predict(final_model, newdata = x_test)
base_pred <- rep(mean(y_train), length(y_test))

metrics <- function(obs, pred, ref_mean) {
  c(RMSE = sqrt(mean((obs - pred)^2)),
    MAE  = mean(abs(obs - pred)),
    R2_out_of_sample = 1 - sum((obs - pred)^2) / sum((obs - ref_mean)^2),
    R2_correlation   = if (sd(pred) > 0) cor(obs, pred)^2 else NA)
}
test_metrics <- rbind(Baseline = metrics(y_test, base_pred, mean(y_train)),
                      Final    = metrics(y_test, pred_test, mean(y_train)))
test_metrics <- round(test_metrics, 3)
rownames(test_metrics)[2] <- paste("Final:", best_name)
test_metrics

# Uncertainty: bootstrap 95% interval for the test RMSE (1000 resamples of the test lots)
set.seed(2026)
boot_rmse <- replicate(1000, { i <- sample(seq_along(y_test), replace = TRUE)
                               sqrt(mean((y_test[i] - pred_test[i])^2)) })
round(quantile(boot_rmse, c(0.025, 0.975)), 3)
round(1 - metrics(y_test, pred_test, mean(y_train))["RMSE"] / metrics(y_test, base_pred, mean(y_train))["RMSE"], 3)  # RMSE reduction vs baseline

# Paired bootstrap: how much lower is the final model's RMSE than the baseline's?
set.seed(2026)
boot_gain <- replicate(1000, { i <- sample(seq_along(y_test), replace = TRUE)
                               sqrt(mean((y_test[i] - base_pred[i])^2)) - sqrt(mean((y_test[i] - pred_test[i])^2)) })
round(quantile(boot_gain, c(0.025, 0.975)), 3)      # 95% interval for the RMSE gain

test_res <- data.frame(observed = y_test, predicted = pred_test, residual = y_test - pred_test)

p7 <- ggplot(test_res, aes(x = predicted, y = observed)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(alpha = 0.45, colour = cb[["blue"]]) +
  geom_smooth(method = "lm", se = FALSE, colour = cb[["vermillion"]]) +
  labs(title = "Predicted and observed total cup points (test set)",
       subtitle = "Dashed line = perfect prediction",
       x = "Predicted", y = "Observed")
save_plot(p7, "fig07_pred_vs_obs.png")

png("outputs/figures/fig08_test_residuals.png", width = 10, height = 5, units = "in", res = 300, bg = "white")
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
plot(test_res$predicted, test_res$residual, pch = 19, col = adjustcolor(cb[["blue"]], 0.4),
     xlab = "Predicted", ylab = "Residual (observed - predicted)", main = "Residuals vs predicted (test set)")
abline(h = 0, col = cb[["vermillion"]], lwd = 2)
hist(test_res$residual, breaks = 25, col = cb[["sky"]], border = "white",
     xlab = "Residual", main = "Distribution of test residuals")
invisible(dev.off())

round(c(mean_residual = mean(test_res$residual), sd_residual = sd(test_res$residual)), 3)
round(sort(test_res$residual)[1:3], 2)                # largest over-predictions
round(tail(sort(test_res$residual), 3), 2)            # largest under-predictions

# 12. Diagnostics of the linear regression (fitted on the training data) -----------
train_lm <- train[, c("total_cup_points", predictors)]
for (v in names(train_lm)[sapply(train_lm, is.numeric)])
  train_lm[[v]][is.na(train_lm[[v]])] <- median(train_lm[[v]], na.rm = TRUE)   # training medians
lm_fit <- lm(total_cup_points ~ ., data = train_lm)
summary(lm_fit)

round(car::vif(lm_fit), 2)                      # GVIF; the last column is comparable across predictors
lmtest::bptest(lm_fit)                          # Breusch-Pagan test for constant variance
shapiro.test(residuals(lm_fit))                 # Shapiro-Wilk test of the residuals
cooks <- cooks.distance(lm_fit)
sum(cooks > 4 / length(cooks))                  # lots above the 4/n rule of thumb
round(sort(cooks, decreasing = TRUE)[1:5], 3)
max(cooks)

png("outputs/figures/fig09_lm_diagnostics.png", width = 10, height = 8, units = "in", res = 300, bg = "white")
par(mfrow = c(2, 2))
plot(lm_fit, which = 1:4, col = adjustcolor(cb[["blue"]], 0.5), pch = 19)
invisible(dev.off())

# 13. Which predictors matter? ----------------------------------------------------------
vi <- varImp(final_model)$importance
vi_df <- data.frame(variable = rownames(vi), importance = vi[, 1])
vi_df <- vi_df[order(-vi_df$importance), ]
head(vi_df, 12)

p10 <- ggplot(head(vi_df, 12), aes(x = importance, y = reorder(variable, importance))) +
  geom_col(fill = cb[["blue"]]) +
  labs(title = paste("Variable importance:", best_name),
       subtitle = "Scaled so that the most important variable = 100",
       x = "Importance", y = NULL)
save_plot(p10, "fig10_importance.png")

# Coefficients of the linear model for interpretation (95% confidence intervals)
lm_coef <- broom::tidy(lm_fit, conf.int = TRUE)
lm_coef[, -1] <- round(lm_coef[, -1], 4)
as.data.frame(lm_coef[lm_coef$p.value < 0.05 & lm_coef$term != "(Intercept)", c("term", "estimate", "conf.low", "conf.high", "p.value")])

# 14. Export tables --------------------------------------------------------------------
hyp_table <- data.frame(
  test = c("Spearman correlation: altitude vs score", "Wilcoxon rank-sum: Washed vs Natural",
           "Kruskal-Wallis: score by country", "Chi-square: processing method vs grade group"),
  statistic = signif(c(unname(t1_sp$statistic), unname(t2_w$statistic), unname(t3$statistic), unname(t4$statistic)), 4),
  p_value = signif(p_raw, 3), p_holm = signif(p.adjust(p_raw, "holm"), 3))
write.csv(hyp_table, "outputs/tables/hypothesis_tests.csv", row.names = FALSE)
write.csv(cv_table, "outputs/tables/cv_results.csv", row.names = FALSE)
write.csv(test_metrics, "outputs/tables/test_metrics.csv")
write.csv(vi_df, "outputs/tables/variable_importance.csv", row.names = FALSE)
write.csv(as.data.frame(lm_coef), "outputs/tables/lm_coefficients.csv", row.names = FALSE)
hyp_table

R.version.string
sapply(c("caret", "glmnet", "ranger", "car", "lmtest", "broom", "ggplot2", "dplyr", "tidyr", "corrplot"),
       function(p) as.character(packageVersion(p)))
