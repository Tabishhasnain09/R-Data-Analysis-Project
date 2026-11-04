# ==================================================================
# Week 4: run the complete project (Weeks 1 to 3) from raw data to model
# Stage 1: data cleaning     -> coffee_ratings_clean.csv, figures/fig01-fig12
# Stage 2: visualization     -> figures/vis01-vis12, tables/
# Stage 3: statistics+model  -> outputs/figures, outputs/tables
# Run in RStudio: Session > Set Working Directory > To Source File Location,
# then run this file. An internet connection is needed once, to download the
# raw data in Stage 1. Each stage runs in its own environment.
# ==================================================================
stages <- c("Stage1_Data_Cleaning.R", "Stage2_Visualization.R", "Stage3_Statistics_Modeling.R")
for (s in stages) {
  if (!file.exists(s)) stop("Missing script: ", s)
  message("Running ", s, " ...")
  source(s, local = new.env(), echo = FALSE)
}
message("All stages finished.")
