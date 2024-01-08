# Setup----

## Libraries
library(nnet)
library(lme4)
library(MASS)
library(car)
library(broom)
library(gghalves)
library(ggfortify)
library(ggbeeswarm)

## Data
dta <- readRDS("./RQ1/R_output/data_for_regression.Rds")

# Categorise the outcome variable for ordinal regression
tmp <- dta$summary_scores %>%
  mutate(
    `Q25_ici` = quantile(ici, .25),
    `Q50_ici` = quantile(ici, .5),
    `Q75_ici` = quantile(ici, .75),
    quantile_group_ici = case_when(
      ici >= `Q75_ici` ~ "4th quantile",
      ici >= `Q50_ici` ~ "3rd quantile",
      ici >= `Q25_ici` ~ "2nd quantile",
      TRUE ~ "1st quantile"),
    six_point_ici = case_when(
      ici >= 5.5 ~ 6,
      ici >= 4.5 ~ 5,
      ici >= 3.5 ~ 4,
      ici >= 2.5 ~ 3,
      ici >= 1.5 ~ 2,
      TRUE ~ 1
    ),
    five_point_ici = case_when(
      
      ici >= 5.5 ~ "6",
      ici >= 4.5 ~ "5",
      ici >= 3.5 ~ "4",
      ici >= 2.5 ~ "3",
      TRUE ~ "1_2"
    ),
    quantile_group_ici = factor(quantile_group_ici, levels = c("1st quantile", "2nd quantile", "3rd quantile", "4th quantile")),
    six_point_ici = factor(six_point_ici, levels = c(1,2,3,4,5,6)),
    five_point_ici = factor(five_point_ici, levels = c("1_2","3","4","5","6")),
    `Q25_hli` = quantile(hli, .25),
    `Q50_hli` = quantile(hli, .5),
    `Q75_hli` = quantile(hli, .75),
    quantile_group_hli = case_when(
      hli >= `Q75_hli` ~ "4th quantile",
      hli >= `Q50_hli` ~ "3rd quantile",
      hli >= `Q25_hli` ~ "2nd quantile",
      TRUE ~ "1st quantile"),
    quantile_group_hli = factor(quantile_group_hli, levels = c("1st quantile", "2nd quantile", "3rd quantile", "4th quantile")),
    # Add age groups
    `75_yo_age` = case_when(
      age_hunt3 <= 75 ~ "<=75",
      TRUE ~ ">75"),
    `75_yo_age` = factor(`75_yo_age`, levels = c("<=75", ">75")),
    age_group = case_when(
      age_hunt4 <= 69.9999999 ~ "65-69",
      age_hunt4 <= 74.9999999 ~ "70-74",
      age_hunt4 <= 79.9999999 ~ "75-79",
      age_hunt4 <= 84.9999999 ~ "80-84",
      age_hunt4 >= 85 ~ "85+")
  )

ggplot(tmp, aes(factor(age_group), y = ici, color = sex)) +
  #geom_violin() +
  geom_boxplot(width = 0.5, position = position_dodge(width = .95)) +
  labs(
    title = "Intrinsic capacity index vs age and sex",
    color = "Sex",
    y = "Intrinsic capacity index",
    x = "Age group") +
  geom_text(data = tmp %>%
              summarise(.by = c(sex, age_group), n = n()) %>%
              mutate (label = paste0("n = ", n)), aes(x = age_group, y = -0.1, label = label),
            position = position_dodge(width = .95)) +
  scale_color_manual(values = c("Women" = "#1E8F88", "Men" = "#EE5A45"))

# Build the models----
models <- list(
  
  adjusted = list(),
  unadjusted = list()
  
)

## Summary intrinsic capcity and healthy life index----
models$adjusted$summary_hli <- polr(five_point_ici ~ hli + number_of_illness + age_hunt4 + sex +
                                      marital_status + education_years_hunt4, data = tmp, Hess = TRUE)
models$unadjusted$summary_hli <- polr(five_point_ici ~ hli, data = tmp, Hess = TRUE)

# With interaction
model_summary_hli_interaction <- polr(five_point_ici ~ hli + number_of_illness + age_hunt4 + sex +
                                      marital_status + education_years_hunt4 +`75_yo_age`*hli + sex*hli,
                                      data = tmp, Hess = TRUE)

#Check proportional odds assumption
car::poTest(models$unadjusted$summary_hli)
car::poTest(models$adjusted$summary_hli)
car::poTest(model_summary_hli_interaction)

# Visualise 
ggplot(tmp, aes(y = hli, x = as_factor(ici))) +
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) + geom_quasirandom(alpha = 0.2) + facet_wrap(~number_of_illness)

ggplot(tmp, aes(y = hli, x = quantile_group_ici)) +
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_text(data = tmp %>% summarise(.by = quantile_group_ici,
                                     n = n(), y_lab = 0.7),
            aes(x = quantile_group_ici, y = y_lab, label = paste0("n = ", n)))


ggplot(tmp, aes(y = hli, x = five_point_ici)) +
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_text(data = tmp %>% summarise(.by = five_point_ici,
                                     n = n(), y_lab = 0.7),
            aes(x = five_point_ici, y = y_lab, label = paste0("n = ", n)))

# Individual healthy behaviors----

# Split data into different subcategories for lapply, add all subcategories to their own column
individual_behavior_dta <- tmp %>%
  left_join(dta$individual_risk_impairment_categories$`Healthy Life Index` %>%
              dplyr::select(pid, subcategory, operationalisation) %>% distinct()) %>%
  split(.$subcategory)
#Reorder operationalisation categories for fish and saturated fat intake since noone is in the moderate risk groups
individual_behavior_dta$Fish_intake <- individual_behavior_dta$Fish_intake %>% mutate(operationalisation = factor(operationalisation, levels = c("High risk", "No/mild risk")))
individual_behavior_dta$Saturated_fat_intake <- individual_behavior_dta$Saturated_fat_intake %>% mutate(operationalisation = factor(operationalisation, levels = c("High risk", "No/mild risk")))

# Find the individual risk categories for all behaviors to add them to the dataframes so they can be adjusted for
tmp2 <- lapply(individual_behavior_dta, function(x) {
  
  x %>% dplyr::select(pid, subcategory, operationalisation) %>% rename(., setNames("operationalisation", unique(x$subcategory))) %>%
    dplyr::select(-subcategory)
  
}) %>% reduce(left_join)

#Add back to the individual dataframes
individual_behavior_dta <- lapply(individual_behavior_dta, function(x) {
  
  x %>% left_join(tmp2)
  
})

#Unadjusted models
models$unadjusted <- c(models$unadjusted, lapply(individual_behavior_dta, function(x) {
  
  polr(five_point_ici ~ operationalisation, data = x, Hess = TRUE)
  
}))

# Adjusted model
# Adjust only for sociodemographics
models$adjusted1 <- c(models$adjusted, lapply(individual_behavior_dta, function(x) {
  
  polr(five_point_ici ~ operationalisation + number_of_illness +
         age_hunt4 + sex + marital_status + education_years_hunt4, data = x, Hess = TRUE)
  
}))

#Adjustments to do for each individual behavior
behavior_model_adjustment_sets <- list(
  
  Alcohol = "+ `Social Interaction`",
  
  Fish_intake = "+ Alcohol + Sleep + Smoking + `Social Interaction`",
  
  FruitVegetable_intake = "+ Alcohol + Sleep + Smoking + `Social Interaction`",
  
  "Milk high fat" = "",
  
  "Milk low fat" = "+ Alcohol + Sleep + Smoking + `Social Interaction`",
  
  "Milk total" = "",
  
  Milk_saturated_fat = "+ Alcohol + Sleep + Smoking + `Social Interaction`",
  
  "Physical activity" = "+ `Social Interaction` + Sleep + Smoking + Fish_intake + FruitVegetable_intake + `Milk low fat` + Milk_saturated_fat + Saturated_fat_intake",
  
  Saturated_fat_intake = "+ Alcohol + Sleep + Smoking + `Social Interaction`",
  
  Sleep = "+ Alcohol + Smoking + `Social Interaction`",
  
  Smoking = "+ Alcohol",
  
  `Social Interaction` = ""
)

#Adjusted models
models$adjusted2 <- mapply(function(dataframe, adjustment_set) {
  
  #Run the same model adjusting for different variables based on the DAG
  polr(paste0("five_point_ici ~ operationalisation + number_of_illness +
         age_hunt4 + sex + marital_status + education_years_hunt4", adjustment_set),
       data = dataframe, Hess = TRUE)
  
}, dataframe = individual_behavior_dta, adjustment_set = behavior_model_adjustment_sets, SIMPLIFY = FALSE)


# Run PoTest
lapply(models$unadjusted, poTest)
lapply(models$adjusted1, poTest)
lapply(models$adjusted2, poTest)

# Format results table----
## Calculate odds ratio and confidence intervals
models_OR_ci <- list(
  
  adjusted1 = lapply(models$adjusted1, function(x) {
    broom::tidy(x, conf.int = TRUE, exponentiate = TRUE)
  }) %>% bind_rows(., .id = "Model"),
  
  adjusted2 = lapply(models$adjusted2, function(x) {
    broom::tidy(x, conf.int = TRUE, exponentiate = TRUE)
  }) %>% bind_rows(., .id = "Model"),
  
  unadjusted = lapply(models$unadjusted, function(x) {
    broom::tidy(x, conf.int = TRUE, exponentiate = TRUE)
  }) %>% bind_rows(., .id = "Model")
  
)

##Build result table
formatted_models <- lapply(models_OR_ci, function(x) {
  
  x %>%
    dplyr::filter(str_detect(term, "hli|operationalisation")) %>%
    mutate(
      #Round values
      across(c(estimate, conf.low, conf.high), ~round(., 2)),
      #New OR column with ci's
      OR_ci = paste0(estimate, " (", conf.low, ";", conf.high, ")")) %>%
    dplyr::select(Model, term, OR_ci)
  
})
#Add adjusted/unadjusted to model columns
formatted_models$adjusted1 <- formatted_models$adjusted1 %>%
  rename(OR_ci_adjusted1 = OR_ci)
formatted_models$adjusted2 <- formatted_models$adjusted2 %>%
  rename(OR_ci_adjusted2 = OR_ci)
formatted_models$unadjusted <- formatted_models$unadjusted %>%
  rename(OR_ci_unadjusted = OR_ci)

## Factor order for plots and tables
factor_orders <- list(
  "Healthy Life Index" = c("summary_hli", "Physical activity", "Social Interaction", "Smoking", "Alcohol", "Sleep",
                           "FruitVegetable_intake", "Fish_intake", "Milk total", "Milk_saturated_fat",
                           "Milk high fat", "Milk low fat", "Saturated_fat_intake"
  ),
  "Healthy Life Index_labels" = c("Healthy Life Index", "Physical activity", "Social Interaction", "Smoking", "Alcohol", "Sleep",
                                  "Fruit and vegetables intake", "Fatty fish intake", "Milk total", "Choose low fat dairy",
                                  "Milk high fat", "Milk low fat", "Choose oils or soft margarine on bread or cooking"))


#Bind together
all_models <- full_join(formatted_models$unadjusted, formatted_models$adjusted1) %>%
  full_join(., formatted_models$adjusted2) %>%
  #Remove healthy behaviors that were not part of the healthy life index
  dplyr::filter(!Model %in% c("Milk high fat", "Milk total")) %>%
  #Organise lifestyle factors
  mutate(Model = factor(Model, levels = factor_orders$`Healthy Life Index`, labels = factor_orders$`Healthy Life Index_labels`)) %>%
  arrange(Model) %>%
  mutate(term = str_replace_all(term, c(
    "hli" = "Healthy Life Index",
    "operationalisation" = ""
  )))



#Save
saveRDS(all_models, "./RQ1/R_output/ordinal_regression_results.Rds")





