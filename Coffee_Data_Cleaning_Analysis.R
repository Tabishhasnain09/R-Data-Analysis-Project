# --------------------------------------------------
# 1. Load Required Packages
# --------------------------------------------------
# Install once (only if the packages are missing):
# install.packages(c("readr", "dplyr", "tidyr", "stringr", "forcats",
#                    "lubridate", "ggplot2", "corrplot", "caret"))

library(readr)      # reading CSV files
library(dplyr)      # data manipulation
library(tidyr)      # reshaping data for faceted plots
library(stringr)    # text cleaning
library(forcats)    # working with factors
library(lubridate)  # date handling
library(ggplot2)    # visualisation
library(corrplot)   # correlation heat map
library(caret)      # one-hot / dummy encoding (dummyVars)

dir.create("figures", showWarnings = FALSE)   # folder for saved charts
theme_set(theme_minimal(base_size = 11) +     # common look for all charts
          theme(plot.background = element_rect(fill = "white", colour = NA)))
coffee_brown <- "#6F4E37"

# --------------------------------------------------
# 2. Import Dataset
# --------------------------------------------------
data_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-07-07/coffee_ratings.csv"

# Download the file once and keep a local copy
if (!file.exists("coffee_ratings.csv")) {
  download.file(data_url, destfile = "coffee_ratings.csv", mode = "wb")
}

# Read the CSV file into a data frame (tibble)
raw_data <- read_csv("coffee_ratings.csv", show_col_types = FALSE)

# 'raw_data' is never modified; all cleaning is done on 'data'
data <- raw_data

head(data)

# --------------------------------------------------
# 3. Initial Data Inspection
# --------------------------------------------------
tail(data)
str(data, give.attr = FALSE, vec.len = 3, nchar.max = 40)
summary(data)
dim(data)
names(data)

# --------------------------------------------------
# 4. Data Quality Assessment
# --------------------------------------------------
# 4a. Missing values
missing_summary <- data.frame(
  variable = names(data),
  missing  = colSums(is.na(data)),
  percent  = round(colMeans(is.na(data)) * 100, 2)
)
missing_summary <- missing_summary[missing_summary$missing > 0, ]
missing_summary <- missing_summary[order(-missing_summary$missing), ]
rownames(missing_summary) <- NULL
missing_summary

sum(is.na(data))   # total number of missing cells

ggplot(missing_summary, aes(x = reorder(variable, percent), y = percent)) +
  geom_col(fill = coffee_brown) +
  coord_flip() +
  labs(title = "Missing values by variable",
       x = NULL, y = "Missing values (%)")
ggsave("figures/fig01_missing_values.png", width = 7, height = 5, dpi = 200)

# 4b. Duplicate records
sum(duplicated(data))   # completely identical rows

# 4c. Columns stored with an unsuitable data type
sapply(data[c("bag_weight", "harvest_year", "grading_date", "altitude")], class)
head(unique(data$bag_weight), 12)
head(unique(data$harvest_year), 12)
head(data$grading_date, 4)

# 4d. Inconsistent categorical values
sort(unique(data$country_of_origin))
table(data$color, useNA = "ifany")
table(data$processing_method, useNA = "ifany")

# 4e. Impossible or invalid values
sub_scores <- c("aroma", "flavor", "aftertaste", "acidity", "body",
                "balance", "uniformity", "clean_cup", "sweetness", "cupper_points")

sum(data$total_cup_points == 0)                 # records with a total score of 0
colSums(data[sub_scores] == 0)                  # zero sub-scores
sum(data$number_of_bags == 0)                   # lots with 0 bags
sum(data$moisture == 0)                         # moisture recorded as exactly 0
sum(data$altitude_mean_meters < 100 |
    data$altitude_mean_meters > 3000, na.rm = TRUE)   # unrealistic altitude (m)
sort(data$altitude_mean_meters)[1:5]            # smallest altitudes
sort(data$altitude_mean_meters, decreasing = TRUE)[1:5]   # largest altitudes

# --------------------------------------------------
# 5. Variable Selection and Missing Value Handling
# --------------------------------------------------
# Identifier, free-text and duplicated columns are not needed for the analysis
drop_cols <- c("owner", "farm_name", "lot_number", "mill", "ico_number",
               "company", "altitude", "region", "producer", "in_country_partner",
               "owner_1", "expiration", "certification_body",
               "certification_address", "certification_contact",
               "unit_of_measurement", "altitude_low_meters", "altitude_high_meters")

data <- data %>% select(-all_of(drop_cols))

# Missing values per variable before any cleaning (used later for comparison)
miss_raw <- colSums(is.na(raw_data))[names(data)]

dim(data)
names(data)

# Remove the single record in which every score is zero (a blank evaluation)
data <- data %>% filter(total_cup_points > 0)

# Disguised missing values: impossible entries are converted to NA
data <- data %>%
  mutate(
    altitude_mean_meters = if_else(altitude_mean_meters < 100 | altitude_mean_meters > 3000,
                                   NA_real_, altitude_mean_meters),
    moisture = if_else(moisture == 0, NA_real_, moisture)
  )

miss_recoded <- colSums(is.na(data))
nrow(data)
miss_recoded[miss_recoded > 0]

# Option 1: delete every row that contains a missing value
nrow(na.omit(data))

# Option 2: mean vs median for the numeric variable 'altitude_mean_meters'
mean(data$altitude_mean_meters, na
.rm = TRUE)
median(data$altitude_mean_meters, na.rm = TRUE)

# Option 3: what would mode imputation do to 'processing_method'?
mode_value   <- names(which.max(table(data$processing_method)))
mode_imputed <- data$processing_method
mode_imputed[is.na(mode_imputed)] <- mode_value
round(rbind(observed_only = prop.table(table(data$processing_method)),
            after_mode    = prop.table(table(mode_imputed))) * 100, 1)

# Categorical variables: missing values become their own category "Unknown"
data <- data %>%
  mutate(across(c(country_of_origin, variety, processing_method, color),
                ~ replace_na(.x, "Unknown")))
data$color[data$color == "None"] <- "Unknown"   # "None" is not a bean colour

# Keep flags so imputed values can always be identified
data <- data %>%
  mutate(altitude_imputed = is.na(altitude_mean_meters),
         moisture_imputed = is.na(moisture))

# Altitude: median altitude of the same country (altitude depends on origin)
data <- data %>%
  group_by(country_of_origin) %>%
  mutate(altitude_mean_meters = if_else(is.na(altitude_mean_meters),
                                        median(altitude_mean_meters, na.rm = TRUE),
                                        altitude_mean_meters)) %>%
  ungroup()
# Countries without any valid altitude use the overall median
data$altitude_mean_meters[is.na(data$altitude_mean_meters)] <-
  median(data$altitude_mean_meters, na.rm = TRUE)

# Moisture and quakers: overall median
data$moisture[is.na(data$moisture)] <- median(data$moisture, na.rm = TRUE)
data$quakers[is.na(data$quakers)]   <- median(data$quakers, na.rm = TRUE)

miss_imputed <- colSums(is.na(data))
miss_imputed[miss_imputed > 0]       # remaining gaps (harvest_year is handled in stage 7)
table(data$altitude_imputed)
table(data$moisture_imputed)

# --------------------------------------------------
# 6. Duplicate Records
# --------------------------------------------------
sum(duplicated(raw_data))            # on the original 43 columns
sum(duplicated(data))                # on the working data
data <- data %>% distinct()          # removes duplicates if any exist
nrow(data)

# --------------------------------------------------
# 7. Data Type Correction
# --------------------------------------------------
# 7a. grading_date: text such as "April 4th, 2015" -> Date
data <- data %>%
  mutate(grading_date = mdy(str_remove(grading_date, "(?<=\\d)(st|nd|rd|th)")),
         grading_year = year(grading_date))

# 7b. harvest_year: keep the first 4-digit year found in the text
data <- data %>%
  mutate(harvest_year = as.integer(str_extract(harvest_year, "20[01][0-9]")),
         # a harvest cannot happen after the coffee was graded
         harvest_year = if_else(harvest_year > grading_year, NA_integer_, harvest_year))

sum(is.na(data$harvest_year))
table(data$grading_year - data$harvest_year)       # gap between grading and harvest

# Most lots were graded in the harvest year (gap = 0), so missing years are
# filled with the grading year minus the most common gap
common_gap <- as.integer(names(which.max(table(data$grading_year - data$harvest_year))))
data$harvest_year <- as.integer(coalesce(data$harvest_year, data$grading_year - common_gap))

# 7c. bag_weight: text such as "60 kg" or "100 lbs" -> numeric kilograms
data <- data %>%
  mutate(
    bag_value = parse_number(bag_weight),
    bag_unit  = case_when(
      str_detect(bag_weight, "lbs") & !str_detect(bag_weight, "kg") ~ "lbs",
      str_detect(bag_weight, "kg") & !str_detect(bag_weight, "lbs") ~ "kg"),
    bag_weight_kg = case_when(bag_unit == "kg"  ~ bag_value,
                              bag_unit == "lbs" ~ bag_value * 0.45359237),
    # a single bag above 100 kg, or a missing/zero value, is not realistic
    bag_weight_kg = if_else(bag_weight_kg > 0 & bag_weight_kg <= 100,
                            bag_weight_kg, NA_real_)
  )
sum(is.na(data$bag_weight_kg))
data$bag_weight_kg[is.na(data$bag_weight_kg)] <- median(data$bag_weight_kg, na.rm = TRUE)
data <- data %>% select(-bag_weight, -bag_value, -bag_unit)

# 7d. Standardise country names, then convert text columns to factors
data <- data %>%
  mutate(country_of_origin = str_replace(country_of_origin, "^Cote d.Ivoire$", "Cote d'Ivoire"),
         country_of_origin = str_replace(country_of_origin, "^Tanzania.*$", "Tanzania")) %>%
  mutate(across(c(species, country_of_origin, variety, processing_method, color),
                ~ fct_infreq(as.factor(.x))))

str(data[c("species", "country_of_origin", "harvest_year", "grading_date",
           "bag_weight_kg", "grading_year")], give.attr = FALSE)
colSums(is.na(data))[colSums(is.na(data)) > 0]

# --------------------------------------------------
# 8. Outlier Detection
# --------------------------------------------------
box_vars <- c("total_cup_points", "aroma", "flavor", "aftertaste", "acidity",
              "body", "balance", "moisture", "category_one_defects",
              "category_two_defects", "altitude_mean_meters", "number_of_bags")

data %>%
  select(all_of(box_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = box_vars)) %>%
  ggplot(aes(x = "", y = value)) +
  geom_boxplot(fill = "#D2B48C", outlier.colour = "#B22222", outlier.size = 1) +
  facet_wrap(~ variable, scales = "free_y", ncol = 4) +
  labs(title = "Boxplots of numeric variables (red points = IQR outliers)",
       x = NULL, y = NULL)
ggsave("figures/fig02_boxplots.png", width = 8, height = 6, dpi = 200)

# IQR rule: a value is flagged if it is below Q1 - 1.5*IQR or above Q3 + 1.5*IQR
iqr_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  fence <- 1.5 * (q[2] - q[1])
  sum(x < q[1] - fence | x > q[2] + fence, na.rm = TRUE)
}

outlier_table <- data.frame(
  variable    = box_vars,
  n_outliers  = sapply(data[box_vars], iqr_outliers),
  percent     = round(sapply(data[box_vars], iqr_outliers) / nrow(data) * 100, 1)
)
rownames(outlier_table) <- NULL
outlier_table

# Z-score check for the total score
z_total <- as.numeric(scale(data$total_cup_points))
sum(abs(z_total) > 3)

# The five lowest-scoring lots
data %>%
  arrange(total_cup_points) %>%
  select(country_of_origin, species, total_cup_points, aroma, flavor, acidity) %>%
  head(5)

# Treatment: winsorise (cap at the 99th percentile) the count variables whose
# extreme values are far from the rest; other outliers are retained
cap99 <- function(x) pmin(x, floor(quantile(x, 0.99, na.rm = TRUE)))
count_vars <- c("category_one_defects", "category_two_defects", "quakers")

rbind(max_before = sapply(data[count_vars], max),
      cap_value  = sapply(data[count_vars], function(x) floor(quantile(x, 0.99))))

data <- data %>% mutate(across(all_of(count_vars), cap99))

rbind(max_after = sapply(data[count_vars], max))

# --------------------------------------------------
# 9. Data Transformation
# --------------------------------------------------
skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3

data <- data %>%
  mutate(
    total_defects = category_one_defects + category_two_defects,
    lot_size_kg   = number_of_bags * bag_weight_kg,
    log_lot_size  = log1p(lot_size_kg),
    log_defects   = log1p(total_defects),
    quality_grade = cut(total_cup_points,
                        breaks = c(-Inf, 80, 85, 90, Inf), right = FALSE,
                        labels = c("Below Specialty (<80)", "Very Good (80-84.99)",
                                   "Excellent (85-89.99)", "Outstanding (90+)"),
                        ordered_result = TRUE),
    altitude_band = cut(altitude_mean_meters,
                        breaks = c(0, 1000, 1500, 2000, Inf), right = FALSE,
                        labels = c("Low (<1000 m)", "Medium (1000-1499 m)",
                                   "High (1500-1999 m)", "Very high (2000+ m)"),
                        ordered_result = TRUE)
  )

# Before-and-after view of the new columns
data %>%
  select(number_of_bags, bag_weight_kg, lot_size_kg, log_lot_size,
         total_defects, log_defects) %>%
  head(5)

# Skewness before and after the log transformation
data.frame(variable = c("lot_size_kg", "log_lot_size", "total_defects", "log_defects"),
           skewness = round(c(skewness(data$lot_size_kg), skewness(data$log_lot_size),
                              skewness(data$total_defects), skewness(data$log_defects)), 2))

table(data$quality_grade)
table(data$altitude_band)

data %>%
  select(lot_size_kg, log_lot_size, total_defects, log_defects) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable,
         levels = c("lot_size_kg", "log_lot_size", "total_defects", "log_defects"))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = coffee_brown, colour = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(title = "Effect of the log transformation", x = NULL, y = "Frequency")
ggsave("figures/fig03_log_transform.png", width = 7, height = 5, dpi = 200)

# --------------------------------------------------
# 10. Normalization / Scaling
# --------------------------------------------------
minmax <- function(x) (x - min(x)) / (max(x) - min(x))

data <- data %>%
  mutate(
    altitude_minmax = minmax(altitude_mean_meters),               # Min-Max (0 to 1)
    altitude_z      = as.numeric(scale(altitude_mean_meters)),    # Z-score
    moisture_z      = as.numeric(scale(moisture)),
    log_lot_size_z  = as.numeric(scale(log_lot_size)),
    log_defects_z   = as.numeric(scale(log_defects))
  )

scale_summary <- function(x) round(c(mean = mean(x), sd = sd(x), min = min(x), max = max(x)), 3)
rbind(altitude_original = scale_summary(data$altitude_mean_meters),
      altitude_minmax   = scale_summary(data$altitude_minmax),
      altitude_zscore   = scale_summary(data$altitude_z),
      moisture_original = scale_summary(data$moisture),
      moisture_zscore   = scale_summary(data$moisture_z))

head(data[c("altitude_mean_meters", "altitude_minmax", "altitude_z")], 5)

# --------------------------------------------------
# 11. Categorical Encoding
# --------------------------------------------------
# Factor encoding is already done; rare coffee varieties are grouped
data <- data %>%
  mutate(variety_group = fct_lump_min(variety, min = 30, other_level = "Other"))

table(data$variety_group)

# Simple binary and ordinal codes
data <- data %>%
  mutate(is_robusta   = as.integer(species == "Robusta"),
         grade_ordinal = as.integer(quality_grade))
table(data$quality_grade, data$grade_ordinal)

# One-hot encoding (one column per category) with the caret package
onehot_model <- dummyVars(~ species + processing_method + color + variety_group,
                          data = data, fullRank = FALSE)
onehot_data  <- as.data.frame(predict(onehot_model, newdata = data))
dim(onehot_data)

# Dummy variables (one reference category dropped per factor)
dummy_model <- dummyVars(~ species + processing_method + color + variety_group,
                         data = data, fullRank = TRUE)
dummy_data  <- as.data.frame(predict(dummy_model, newdata = data))
dim(dummy_data)
names(dummy_data)
head(dummy_data[, 1:4], 4)

# --------------------------------------------------
# 12. Cleaned Dataset
# --------------------------------------------------
clean_data   <- data
encoded_data <- bind_cols(clean_data, dummy_data)

str(clean_data, give.attr = FALSE, vec.len = 3, nchar.max = 40)
dim(clean_data)
colSums(is.na(clean_data))

write_csv(clean_data, "coffee_ratings_clean.csv")
write_csv(encoded_data, "coffee_ratings_encoded.csv")

summary(clean_data)

# --------------------------------------------------
# 13. Exploratory Data Analysis
# --------------------------------------------------
# A. Histogram of the total cup score
ggplot(clean_data, aes(x = total_cup_points)) +
  geom_histogram(binwidth = 1, fill = coffee_brown, colour = "white") +
  geom_vline(xintercept = 80, linetype = "dashed", colour = "#B22222") +
  labs(title = "Distribution of total cup points",
       subtitle = "Dashed line = 80 points (specialty threshold)",
       x = "Total cup points", y = "Number of lots")
ggsave("figures/fig04_hist_total.png", width = 7, height = 4.5, dpi = 200)

# B. Boxplot of total cup points by processing method
ggplot(clean_data, aes(x = fct_reorder(processing_method, total_cup_points, median),
                       y = total_cup_points)) +
  geom_boxplot(fill = "#D2B48C") +
  coord_flip() +
  labs(title = "Total cup points by processing method",
       x = "Processing method", y = "Total cup points")
ggsave("figures/fig05_box_processing.png", width = 7, height = 4.5, dpi = 200)

# C. Bar charts for categorical variables
clean_data %>%
  count(country_of_origin, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = fct_reorder(country_of_origin, n), y = n)) +
  geom_col(fill = coffee_brown) +
  coord_flip() +
  labs(title = "Top 10 countries by number of lots", x = NULL, y = "Number of lots")
ggsave("figures/fig06_bar_country.png", width = 7, height = 4.5, dpi = 200)

ggplot(clean_data, aes(x = quality_grade)) +
  geom_bar(fill = coffee_brown) +
  labs(title = "Lots by SCA quality grade", x = NULL, y = "Number of lots") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("figures/fig07_bar_grade.png", width = 7, height = 4.5, dpi = 200)

# D. Scatter plots
# Only lots with a recorded altitude are plotted (imputed values are excluded)
clean_data %>%
  filter(!altitude_imputed) %>%
  ggplot(aes(x = altitude_mean_meters, y = total_cup_points)) +
  geom_point(alpha = 0.35, colour = coffee_brown) +
  geom_smooth(method = "lm", se = FALSE, colour = "#B22222") +
  labs(title = "Altitude and total cup points",
       x = "Mean altitude (m)", y = "Total cup points")
ggsave("figures/fig08_scatter_altitude.png", width = 7, height = 4.5, dpi = 200)

ggplot(clean_data, aes(x = flavor, y = aftertaste)) +
  geom_point(alpha = 0.35, colour = coffee_brown) +
  geom_smooth(method = "lm", se = FALSE, colour = "#B22222") +
  labs(title = "Flavor and aftertaste scores", x = "Flavor", y = "Aftertaste")
ggsave("figures/fig09_scatter_flavor.png", width = 7, height = 4.5, dpi = 200)

# --------------------------------------------------
# 14. Correlation Analysis
# --------------------------------------------------
cor_vars <- c("total_cup_points", "aroma", "flavor", "aftertaste", "acidity",
              "body", "balance", "uniformity", "clean_cup", "sweetness",
              "cupper_points", "moisture", "total_defects",
              "altitude_mean_meters", "log_lot_size")

cor_matrix <- cor(clean_data[, cor_vars], use = "complete.obs")

# Correlation of every variable with the total score
round(sort(cor_matrix[, "total_cup_points"], decreasing = TRUE), 2)

# Strongest pairs among the explanatory variables (total score excluded)
cor_pairs <- as.data.frame(as.table(cor_matrix))
names(cor_pairs) <- c("var1", "var2", "r")
cor_pairs <- cor_pairs %>%
  filter(var1 != "total_cup_points", var2 != "total_cup_points",
         as.character(var1) < as.character(var2)) %>%
  arrange(desc(abs(r)))
head(cor_pairs, 8)

# Rank-based check for altitude
cor(clean_data$altitude_mean_meters, clean_data$total_cup_points, method = "spearman")

# Is the total score simply the sum of the ten sub-scores?
mean(abs(rowSums(clean_data[sub_scores]) - clean_data$total_cup_points) < 0.02)

png("figures/fig10_corr_heatmap.png", width = 1800, height = 1800, res = 200, bg = "white")
corrplot(cor_matrix, method = "color", type = "upper",
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.cex = 0.8, tl.srt = 45)
dev.off()

# F. Additional charts
# Average score by country (countries with at least 20 lots)
clean_data %>%
  group_by(country_of_origin) %>%
  summarise(lots = n(), mean_points = mean(total_cup_points), .groups = "drop") %>%
  filter(lots >= 20) %>%
  ggplot(aes(x = fct_reorder(country_of_origin, mean_points), y = mean_points)) +
  geom_col(fill = coffee_brown) +
  geom_text(aes(label = round(mean_points, 1)), hjust = 1.1, colour = "white", size = 3) +
  coord_flip(ylim = c(70, 86)) +
  labs(title = "Mean total cup points by country (at least 20 lots)",
       x = NULL, y = "Mean total cup points")
ggsave("figures/fig11_country_mean.png", width = 7, height = 4.5, dpi = 200)

# Score by altitude band (recorded altitudes only)
clean_data %>%
  filter(!altitude_imputed) %>%
  ggplot(aes(x = altitude_band, y = total_cup_points)) +
  geom_boxplot(fill = "#D2B48C") +
  labs(title = "Total cup points by altitude band", x = "Altitude band", y = "Total cup points") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("figures/fig12_box_altitude.png", width = 7, height = 4.5, dpi = 200)

# --------------------------------------------------
# 15. Descriptive Statistics
# --------------------------------------------------
desc_vars <- c("total_cup_points", "aroma", "flavor", "aftertaste", "acidity",
               "body", "balance", "uniformity", "clean_cup", "sweetness",
               "cupper_points", "moisture", "category_one_defects",
               "category_two_defects", "quakers", "altitude_mean_meters",
               "number_of_bags")

desc_stats <- data.frame(
  variable = desc_vars,
  n      = sapply(clean_data[desc_vars], function(x) sum(!is.na(x))),
  mean   = sapply(clean_data[desc_vars], mean),
  median = sapply(clean_data[desc_vars], median),
  sd     = sapply(clean_data[desc_vars], sd),
  min    = sapply(clean_data[desc_vars], min),
  q1     = sapply(clean_data[desc_vars], quantile, 0.25),
  q3     = sapply(clean_data[desc_vars], quantile, 0.75),
  max    = sapply(clean_data[desc_vars], max)
)
rownames(desc_stats) <- NULL
desc_stats[, -1] <- round(desc_stats[, -1], 2)
desc_stats

# Group summaries of the total score
clean_data %>%
  group_by(species) %>%
  summarise(lots = n(), mean = round(mean(total_cup_points), 2),
            median = median(total_cup_points), sd = round(sd(total_cup_points), 2))

clean_data %>%
  group_by(processing_method) %>%
  summarise(lots = n(), mean = round(mean(total_cup_points), 2),
            median = median(total_cup_points), sd = round(sd(total_cup_points), 2)) %>%
  arrange(desc(mean))
