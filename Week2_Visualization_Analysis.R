# --------------------------------------------------
# 1. Packages, folders and a common plotting style
# --------------------------------------------------
# Install once (only if the packages are missing):
# install.packages(c("ggplot2", "dplyr", "tidyr", "scales", "lattice", "corrplot"))

library(ggplot2)    # main plotting package
library(dplyr)      # data preparation and summaries
library(tidyr)      # reshaping data (wide <-> long)
library(scales)     # percentage labels
library(lattice)    # trellis charts (one chart in this project)
library(corrplot)   # correlation heat map

dir.create("figures", showWarnings = FALSE)   # graph images are saved here
dir.create("tables",  showWarnings = FALSE)   # summary tables are saved here

# Colour-blind friendly palette (Okabe-Ito)
cb <- c(blue = "#0072B2", orange = "#E69F00", green = "#009E73",
        vermillion = "#D55E00", sky = "#56B4E9", pink = "#CC79A7", grey = "#999999")

# One theme for every ggplot2 chart
theme_set(theme_minimal(base_size = 12) +
          theme(plot.title = element_text(face = "bold"),
                panel.grid.minor = element_blank(),
                plot.background = element_rect(fill = "white", colour = NA)))

# Helper that saves a ggplot at report quality (8 x 5 inches, 300 dpi)
save_plot <- function(p, file, w = 8, h = 5) {
  ggsave(file.path("figures", file), plot = p, width = w, height = h, dpi = 300, bg = "white")
}

# --------------------------------------------------
# 2. Import the cleaned Week 1 dataset
# --------------------------------------------------
# 'coffee_ratings_clean.csv' was produced by the Week 1 data cleaning project
coffee <- read.csv("coffee_ratings_clean.csv", stringsAsFactors = FALSE)

# First rows (seven of the 42 columns are shown so the output fits the page)
head(coffee[, c("country_of_origin", "species", "processing_method",
                "total_cup_points", "altitude_mean_meters",
                "grading_year", "quality_grade")])
dim(coffee)

# --------------------------------------------------
# 3. Inspect the data
# --------------------------------------------------
str(coffee, vec.len = 3, nchar.max = 40)

summary(coffee[, c("total_cup_points", "aroma", "flavor", "aftertaste", "acidity",
                   "body", "balance", "moisture", "total_defects",
                   "altitude_mean_meters", "number_of_bags", "grading_year")])

sum(is.na(coffee))        # missing values in the whole table
sum(duplicated(coffee))   # duplicated rows

# --------------------------------------------------
# 4. Prepare the data types
# --------------------------------------------------
# read.csv() stores text as character, so categories are turned into factors.
# Unordered factors are sorted by frequency (largest group first).
coffee$grading_date <- as.Date(coffee$grading_date)

cat_vars <- c("species", "country_of_origin", "variety", "processing_method",
              "color", "variety_group")
coffee[cat_vars] <- lapply(coffee[cat_vars],
                           function(x) factor(x, levels = names(sort(table(x), decreasing = TRUE))))

# Ordered factors for quality grade and altitude band
coffee$quality_grade <- factor(coffee$quality_grade, ordered = TRUE,
  levels = c("Below Specialty (<80)", "Very Good (80-84.99)",
             "Excellent (85-89.99)", "Outstanding (90+)"))
coffee$altitude_band <- factor(coffee$altitude_band, ordered = TRUE,
  levels = c("Low (<1000 m)", "Medium (1000-1499 m)",
             "High (1500-1999 m)", "Very high (2000+ m)"))

# A three-level grade: the single "Outstanding" lot is merged with "Excellent"
coffee$grade3 <- ifelse(coffee$quality_grade %in% c("Excellent (85-89.99)", "Outstanding (90+)"),
                        "Excellent or above (85+)", as.character(coffee$quality_grade))
coffee$grade3 <- factor(coffee$grade3, levels = c("Below Specialty (<80)",
                        "Very Good (80-84.99)", "Excellent or above (85+)"))

# Check the conversions
sapply(coffee[c("species", "country_of_origin", "quality_grade",
                "altitude_band", "grading_date")], function(x) class(x)[1])
table(coffee$grade3, useNA = "ifany")

# Altitude was filled in (imputed) for some lots in Week 1. Plots that use altitude
# are drawn only for lots with a recorded altitude, so filled-in values do not shape them.
recorded <- subset(coffee, !altitude_imputed)
nrow(recorded)

# Number of columns of each type
table(sapply(coffee, function(x) class(x)[1]))

# --------------------------------------------------
# 5. Descriptive statistics that support the graphs
# --------------------------------------------------
num_vars <- c("total_cup_points", "aroma", "flavor", "aftertaste", "acidity", "body",
              "balance", "uniformity", "clean_cup", "sweetness", "cupper_points",
              "moisture", "total_defects", "altitude_mean_meters", "number_of_bags")

desc_stats <- data.frame(
  variable = num_vars,
  n      = sapply(coffee[num_vars], function(x) sum(!is.na(x))),
  mean   = sapply(coffee[num_vars], mean),
  median = sapply(coffee[num_vars], median),
  sd     = sapply(coffee[num_vars], sd),
  min    = sapply(coffee[num_vars], min),
  q1     = sapply(coffee[num_vars], quantile, 0.25),
  q3     = sapply(coffee[num_vars], quantile, 0.75),
  max    = sapply(coffee[num_vars], max)
)
rownames(desc_stats) <- NULL
desc_stats[, -1] <- round(desc_stats[, -1], 2)
desc_stats

# Group-wise summaries of the total score
country_stats <- coffee %>%
  group_by(country_of_origin) %>%
  summarise(lots = n(), mean = round(mean(total_cup_points), 2),
            median = round(median(total_cup_points), 2),
            sd = round(sd(total_cup_points), 2), iqr = round(IQR(total_cup_points), 2),
            .groups = "drop") %>%
  filter(lots >= 20) %>%
  arrange(desc(median))
as.data.frame(country_stats)

as.data.frame(coffee %>% group_by(processing_method) %>%
  summarise(lots = n(), mean = round(mean(total_cup_points), 2),
            median = round(median(total_cup_points), 2), .groups = "drop"))

as.data.frame(coffee %>% group_by(species) %>%
  summarise(lots = n(), mean = round(mean(total_cup_points), 2),
            median = round(median(total_cup_points), 2), .groups = "drop"))

# Save the tables for the report
write.csv(desc_stats,    "tables/descriptive_statistics.csv", row.names = FALSE)
write.csv(country_stats, "tables/country_statistics.csv",     row.names = FALSE)

# --------------------------------------------------
# Visualization 1: Bar chart - lots per country
# --------------------------------------------------
country_counts <- coffee %>%
  count(country_of_origin, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(share = round(100 * n / nrow(coffee), 1))

p1 <- ggplot(country_counts, aes(x = reorder(country_of_origin, n), y = n)) +
  geom_col(fill = cb[["blue"]]) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.8) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Ten countries with the most coffee lots",
       x = NULL, y = "Number of lots")
save_plot(p1, "vis01_bar_country.png")

as.data.frame(country_counts)
round(100 * sum(country_counts$n[1:4]) / nrow(coffee), 1)   # share of the top four

# --------------------------------------------------
# Visualization 2: Histogram - total cup points
# --------------------------------------------------
skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3
score_median <- median(coffee$total_cup_points)

p2 <- ggplot(coffee, aes(x = total_cup_points)) +
  geom_histogram(binwidth = 1, boundary = 0, fill = cb[["blue"]], colour = "white") +
  geom_vline(xintercept = 80, linetype = "dashed", colour = cb[["vermillion"]], linewidth = 0.9) +
  geom_vline(xintercept = score_median, colour = cb[["orange"]], linewidth = 0.9) +
  annotate("text", x = 79.5, y = 330, label = "80-point specialty threshold",
           hjust = 1, colour = cb[["vermillion"]], size = 3.8) +
  annotate("text", x = score_median + 0.5, y = 330, label = paste("Median:", score_median),
           hjust = 0, colour = "#9A6A00", size = 3.8) +
  labs(title = "Distribution of total cup points",
       x = "Total cup points", y = "Number of lots")
save_plot(p2, "vis02_histogram_score.png")

round(c(mean = mean(coffee$total_cup_points), median = score_median,
        sd = sd(coffee$total_cup_points), skewness = skewness(coffee$total_cup_points)), 2)
range(coffee$total_cup_points)
round(100 * mean(coffee$total_cup_points >= 80), 1)   # % of lots at 80 points or more
sum(coffee$total_cup_points < 75)                      # lots in the low tail

# --------------------------------------------------
# Visualization 3: Scatter plot - altitude and total score
# --------------------------------------------------
p3 <- ggplot(recorded, aes(x = altitude_mean_meters, y = total_cup_points)) +
  geom_point(alpha = 0.4, colour = cb[["blue"]], size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, colour = cb[["vermillion"]], fill = cb[["vermillion"]],
              alpha = 0.15) +
  labs(title = "Higher farms score slightly higher, but with wide scatter",
       x = "Mean farm altitude (metres)", y = "Total cup points")
save_plot(p3, "vis03_scatter_altitude.png")

round(cor(recorded$altitude_mean_meters, recorded$total_cup_points), 2)        # Pearson
round(cor(recorded$altitude_mean_meters, recorded$total_cup_points, method = "spearman"), 2)
fit <- lm(total_cup_points ~ altitude_mean_meters, data = recorded)
round(coef(fit)["altitude_mean_meters"] * 100, 2)   # points gained per extra 100 m
round(summary(fit)$r.squared, 3)                     # share of variation explained

# --------------------------------------------------
# Visualization 4: Line chart - average score by grading year
# --------------------------------------------------
yearly <- coffee %>%
  group_by(grading_year) %>%
  summarise(lots = n(), mean_score = round(mean(total_cup_points), 2),
            median_score = round(median(total_cup_points), 2), .groups = "drop")

p4 <- ggplot(yearly, aes(x = grading_year, y = mean_score)) +
  geom_hline(yintercept = 80, linetype = "dashed", colour = cb[["vermillion"]]) +
  geom_line(colour = cb[["blue"]], linewidth = 1) +
  geom_point(aes(size = lots), colour = cb[["blue"]]) +
  scale_x_continuous(breaks = 2010:2018) +
  scale_y_continuous(limits = c(78, 86)) +
  scale_size_continuous(range = c(2, 8), name = "Lots graded") +
  labs(title = "Average total score by year of grading",
       x = "Year the lot was graded", y = "Mean total cup points")
save_plot(p4, "vis04_line_year.png")

as.data.frame(yearly)

# --------------------------------------------------
# Visualization 5: Boxplot - score by country
# --------------------------------------------------
big <- country_stats$country_of_origin                        # countries with 20+ lots
box_data <- coffee %>%
  filter(country_of_origin %in% big) %>%
  mutate(country_label = paste0(country_of_origin, " (n=", ave(total_cup_points, country_of_origin, FUN = length), ")"))
box_data$country_label <- reorder(box_data$country_label, box_data$total_cup_points, FUN = median)

p5 <- ggplot(box_data, aes(x = country_label, y = total_cup_points)) +
  geom_hline(yintercept = 80, linetype = "dashed", colour = cb[["vermillion"]]) +
  geom_boxplot(fill = cb[["sky"]], colour = "grey25", outlier.colour = cb[["vermillion"]],
               outlier.size = 1.2) +
  coord_flip() +
  labs(title = "Total cup points by country of origin",
       subtitle = "Countries with at least 20 lots; dashed line = 80 points",
       x = NULL, y = "Total cup points")
save_plot(p5, "vis05_boxplot_country.png", h = 6)

as.data.frame(country_stats[, c("country_of_origin", "lots", "median", "iqr")])

# --------------------------------------------------
# Visualization 6: Correlation heat map (Spearman)
# --------------------------------------------------
cor_vars <- c("total_cup_points", "aroma", "flavor", "aftertaste", "acidity", "body",
              "balance", "uniformity", "clean_cup", "sweetness", "cupper_points",
              "moisture", "total_defects", "altitude_mean_meters")
cor_sp <- cor(coffee[cor_vars], method = "spearman", use = "pairwise.complete.obs")
cor_pe <- cor(coffee[cor_vars], method = "pearson",  use = "pairwise.complete.obs")

png("figures/vis06_correlation_heatmap.png", width = 8, height = 8, units = "in", res = 300, bg = "white")
corrplot(cor_sp, method = "color", type = "upper", order = "hclust",
         col = colorRampPalette(c(cb[["vermillion"]], "white", cb[["blue"]]))(200),
         addCoef.col = "black", number.cex = 0.65, tl.col = "black", tl.cex = 0.9,
         tl.srt = 45, cl.cex = 0.8)
invisible(dev.off())

round(sort(cor_sp[, "total_cup_points"], decreasing = TRUE), 2)
round(max(abs(cor_sp - cor_pe)), 2)       # largest gap between Spearman and Pearson

# --------------------------------------------------
# Visualization 7: 100% stacked bar - quality grade by processing method
# --------------------------------------------------
grade_pct <- coffee %>%
  count(processing_method, grade3) %>%
  group_by(processing_method) %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  ungroup() %>%
  filter(total >= 50)                                       # methods with 50+ lots
grade_pct$method_label <- paste0(grade_pct$processing_method, "\n(n=", grade_pct$total, ")")

p7 <- ggplot(grade_pct, aes(x = method_label, y = pct, fill = grade3)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(pct), "%")), position = position_stack(vjust = 0.5), size = 3.6) +
  scale_fill_manual(values = c(cb[["orange"]], cb[["sky"]], cb[["blue"]]), name = "Quality grade") +
  labs(title = "Share of lots in each quality grade, by processing method",
       x = NULL, y = "Percentage of lots") +
  theme(legend.position = "bottom")
save_plot(p7, "vis07_stacked_grade_method.png")

as.data.frame(grade_pct[, c("processing_method", "grade3", "n", "total")] %>%
  mutate(pct = round(100 * n / total, 1)))

# --------------------------------------------------
# Visualization 8: Faceted scatter plot - altitude and score within countries
# --------------------------------------------------
rec_counts <- table(recorded$country_of_origin)
facet_countries <- names(rec_counts)[rec_counts >= 50]      # 50+ recorded altitudes
facet_data <- subset(recorded, country_of_origin %in% facet_countries)
facet_data$country_of_origin <- droplevels(facet_data$country_of_origin)

p8 <- ggplot(facet_data, aes(x = altitude_mean_meters, y = total_cup_points)) +
  geom_point(alpha = 0.4, colour = cb[["blue"]], size = 1.4) +
  geom_smooth(method = "lm", se = FALSE, colour = cb[["vermillion"]], linewidth = 0.9) +
  facet_wrap(~ country_of_origin, ncol = 3, scales = "free_x") +
  labs(title = "Altitude and score within each country",
       subtitle = "Each panel has its own altitude scale",
       x = "Mean farm altitude (metres)", y = "Total cup points")
save_plot(p8, "vis08_facet_altitude_country.png", h = 5.5)

facet_data %>%
  group_by(country_of_origin) %>%
  summarise(lots = n(), r = round(cor(altitude_mean_meters, total_cup_points), 2),
            slope_per_100m = round(coef(lm(total_cup_points ~ altitude_mean_meters))[2] * 100, 2),
            .groups = "drop") %>%
  as.data.frame()

# Median altitude and median score of every country with 20+ recorded altitudes
country_alt <- recorded %>%
  group_by(country_of_origin) %>%
  summarise(lots = n(), median_altitude = median(altitude_mean_meters),
            median_score = round(median(total_cup_points), 2), .groups = "drop") %>%
  filter(lots >= 20) %>%
  arrange(desc(median_altitude))
as.data.frame(country_alt)
round(cor(country_alt$median_altitude, country_alt$median_score, method = "spearman"), 2)

# --------------------------------------------------
# Visualization 9: Density plot - Washed vs Natural processing
# --------------------------------------------------
two_methods <- subset(coffee, processing_method %in% c("Washed / Wet", "Natural / Dry"))
two_methods$processing_method <- droplevels(two_methods$processing_method)

p9 <- ggplot(two_methods, aes(x = total_cup_points, fill = processing_method,
                              colour = processing_method)) +
  geom_density(alpha = 0.35, linewidth = 0.9) +
  geom_vline(xintercept = 80, linetype = "dashed", colour = "grey30") +
  scale_fill_manual(values = c(cb[["blue"]], cb[["orange"]]), name = NULL) +
  scale_colour_manual(values = c(cb[["blue"]], cb[["orange"]]), name = NULL) +
  coord_cartesian(xlim = c(68, 92)) +
  labs(title = "Score distribution of the two main processing methods",
       x = "Total cup points", y = "Density") +
  theme(legend.position = "top")
save_plot(p9, "vis09_density_method.png")

as.data.frame(two_methods %>% group_by(processing_method) %>%
  summarise(lots = n(), median = round(median(total_cup_points), 2),
            sd = round(sd(total_cup_points), 2),
            pct_80_plus = round(100 * mean(total_cup_points >= 80), 1), .groups = "drop"))

# --------------------------------------------------
# Visualization 10: Heat map - sensory profile of each country
# --------------------------------------------------
attrs <- c("aroma", "flavor", "aftertaste", "acidity", "body", "balance",
           "uniformity", "clean_cup", "sweetness", "cupper_points")
prof_countries <- names(which(table(coffee$country_of_origin) >= 40))

profile <- coffee %>%
  filter(country_of_origin %in% prof_countries) %>%
  group_by(country_of_origin) %>%
  summarise(across(all_of(attrs), mean), mean_total = mean(total_cup_points), .groups = "drop") %>%
  mutate(country_of_origin = reorder(as.character(country_of_origin), mean_total))

overall_mean <- sapply(coffee[attrs], mean)
overall_sd   <- sapply(coffee[attrs], sd)

profile_long <- profile %>%
  pivot_longer(all_of(attrs), names_to = "attribute", values_to = "mean_score") %>%
  mutate(attribute = factor(attribute, levels = attrs),
         z = (mean_score - overall_mean[as.character(attribute)]) / overall_sd[as.character(attribute)])

p10 <- ggplot(profile_long, aes(x = attribute, y = country_of_origin, fill = z)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", mean_score)), size = 3.2) +
  scale_fill_gradient2(low = cb[["vermillion"]], mid = "white", high = cb[["blue"]],
                       midpoint = 0, name = "Difference from\nall-lot average\n(std. units)") +
  labs(title = "Average sensory scores by country",
       subtitle = "Colour = how far a country's average is from the all-lot average",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid = element_blank())
save_plot(p10, "vis10_heatmap_sensory_country.png", h = 5.5)

as.data.frame(profile_long %>% group_by(attribute) %>%
  summarise(max_country = as.character(country_of_origin[which.max(z)]),
            max_mean = round(max(mean_score), 2),
            min_country = as.character(country_of_origin[which.min(z)]),
            min_mean = round(min(mean_score), 2), .groups = "drop"))

# --------------------------------------------------
# Visualization 11: Base R plots - flavor/aftertaste and altitude
# --------------------------------------------------
png("figures/vis11_base_r_plots.png", width = 10, height = 5, units = "in", res = 300, bg = "white")
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1), cex.lab = 1.1, cex.main = 1.2)

plot(coffee$flavor, coffee$aftertaste, pch = 19, col = adjustcolor(cb[["blue"]], alpha.f = 0.35),
     xlab = "Flavor score", ylab = "Aftertaste score", main = "Flavor and aftertaste")
abline(lm(aftertaste ~ flavor, data = coffee), col = cb[["vermillion"]], lwd = 2)

hist(recorded$altitude_mean_meters, breaks = 25, col = cb[["sky"]], border = "white",
     xlab = "Mean farm altitude (metres)", ylab = "Number of lots",
     main = "Recorded farm altitudes")
abline(v = median(recorded$altitude_mean_meters), col = cb[["vermillion"]], lwd = 2, lty = 2)
legend("topright", legend = "Median", col = cb[["vermillion"]], lty = 2, lwd = 2, bty = "n")
invisible(dev.off())

round(cor(coffee$flavor, coffee$aftertaste), 2)
round(coef(lm(aftertaste ~ flavor, data = coffee)), 2)
round(quantile(recorded$altitude_mean_meters, c(0.05, 0.25, 0.5, 0.75, 0.95)))

# --------------------------------------------------
# Visualization 12: Lattice boxplot - defects by quality grade
# --------------------------------------------------
png("figures/vis12_lattice_defects_grade.png", width = 8, height = 5, units = "in", res = 300, bg = "white")
print(bwplot(total_defects ~ grade3, data = coffee,
             xlab = "Quality grade", ylab = "Defects per lot (category one + two)",
             main = "Number of defects by quality grade",
             scales = list(x = list(cex = 0.9)),
             par.settings = list(box.rectangle = list(col = cb[["blue"]], fill = cb[["sky"]]),
                                 box.umbrella = list(col = cb[["blue"]]),
                                 plot.symbol = list(col = cb[["vermillion"]], pch = 19, cex = 0.5),
                                 box.dot = list(col = "black"))))
invisible(dev.off())

as.data.frame(coffee %>% group_by(grade3) %>%
  summarise(lots = n(), median_defects = median(total_defects),
            mean_defects = round(mean(total_defects), 2),
            q1 = quantile(total_defects, 0.25), q3 = quantile(total_defects, 0.75),
            max_defects = max(total_defects),
            pct_no_defects = round(100 * mean(total_defects == 0), 1), .groups = "drop"))
