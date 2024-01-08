# Formatting tables for RQ1

# Setup----
## Read data
### Descriptive stats
descriptive_stats <- readRDS("./RQ1/R_output/descriptive_stats.Rds")
### Regression results
regression_results <- readRDS("./RQ1/R_output/ordinal_regression_results.Rds")

#List to fill
tables <- list()

# Descriptive stats----
#Remove milk total and milk high fat categories from the individual categories as they are not used
descriptive_stats$`By risk or impairtment operationalisation`$`Healthy Life Index` <- descriptive_stats$`By risk or impairtment operationalisation`$`Healthy Life Index` %>%
  dplyr::filter(!subcategory %in% c("Milk total", "Milk high fat"))
# Only format the quartiles df, ue age in HUNT3 for behaviors, and age in HUNT4 for intrinsic capacity
descriptive_stats$`By summary score quartiles` <- full_join(descriptive_stats$`By summary score quartiles`$`Healthy Life Index` %>%
                    dplyr::filter(variable != "age_hunt4") %>% mutate(variable = fct_recode(variable, "Age" = "age_hunt3")),
                  descriptive_stats$`By summary score quartiles`$`Intrinsic capacity` %>%
                    dplyr::filter(variable != "age_hunt3") %>% mutate(variable = fct_recode(variable, "Age" = "age_hunt4")))

test <- c(" ", " ", 
          names(descriptive_stats$`By summary score quartiles`) %>%
            str_replace_all(c(
              "__" = " <",
              "--" = " ≥",
              "_" = ", n = "
              )))

tables$descriptive_stats <- descriptive_stats

# Regression results----
# Formatting ok from regression_models script, add to tables
tables$regression_results <- regression_results

# Save----
saveRDS(tables, "./RQ1/R_output/article1_tables.Rds")

