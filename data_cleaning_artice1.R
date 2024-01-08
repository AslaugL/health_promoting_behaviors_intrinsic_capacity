# Clean up and data for analysis

# Setup ----
## Libraries
library(haven)
library(mixgb)
library(gghalves)
library(BiocManager)
library(ComplexHeatmap)

## Source color vectors used in all RQs
source("common_functions_variables.R")

# Functions only used in this script----
#Set answer order for categorical variables, for plots and tables.
#' @param df Tidy dataframe with categorical values from HUNT.
setAnswerFactorValues <- function(df) {
  
  df %>% mutate(factor_value = factor(value,
                                      levels = c(
                                        #Ja nei
                                        "Ja", "Nei",
                                        #Aldri/sjelden ol
                                        "Bruker ikke", "Sjelden eller aldri", "Aldri", "Aldri drukket alkohol", "Aldri røykt", "Ingen", "Ingen gang",
                                        "Tidligere røyker", "Av og til", "Røyker av og til", "Ikke drukket alkohol siste fire uker", "Ja, 1-2 ganger", "Ja, 3 ganger",
                                        
                                        #Minutter
                                        "Mindre enn 15 minutter", "15-29 minutter", "30 minutter - 1 time",
                                        #Timer
                                        "Mer enn 6 timer", "4-6 timer", "3 timer eller mer", "1-3 timer", "1-2 timer", "Mer enn 1 time", "Mindre enn 1 time", 
                                        
                                        #Dag
                                        "Daglig", "Ja, daglig", "Daglig røyker",
                                        "6 eller flere per dag", "4-5 per dag", "4 eller mer per dag",
                                        "2 ganger eller mer per dag", "2-3 per dag", "1 gang per dag", "1 per dag", "Omtrent hver dag", 
                                        #Uke
                                        "5-7 ganger per uke", "4-7 ganger per uke", "4-6 ganger per uke",
                                        "1-3 ganger per uke", "1-6 per uke", "2-3 ganger per uke", "Mer enn 1 gang per uke", "Flere ganger per uke", 
                                        "1 gang per uke", "Omtrent 1 gang per uke", "Ukentlig", "Sjeldnere enn 1 gang per uke",
                                        "0-4 ganger per uke",
                                        
                                        #Måned
                                        "Mer enn 3 ganger per måned", "2-3 ganger per måned", "1-3 ganger per måned", "0-3 ganger per måned", 
                                        "Omtrent 1 gang per måned", "Månedlig", "1-6 ganger siste 6 måneder", "1-5 ganger siste 6 måneder", 
                                        
                                        #År
                                        "Noen få ganger per år", "Ingen ganger siste år", 
                                        
                                        #Matvarer
                                        "Oljer", "Myk/lett margarin", "Hard margarin", "Meierismør",
                                        
                                        #Aktivitet
                                        "Tar det rolig, blir ikke andpusten eller svett", "Tar det hardt, blir andpusten og svett", "Tar meg nesten helt ut",
                                        
                                        #SPPB/Støtte ved seksminutters gåtest
                                        "Uten", "Krykke/stokk", "Rullator", "Annet",
                                        
                                        #Gripestyrke
                                        "Høyre", "Venstre",
                                        
                                        #HADS
                                        "Ikke i det hele tatt", "Svært sjelden", "Ikke så ofte", "Avgjort ikke som før", "Ikke like mye nå som før", "Litt", "Bare lite grann",
                                        "Ja, jeg har sluttet å bry meg", "Ikke som jeg burde", "Kan hende ikke nok",
                                        "Ikke fullt så mye", "Litt, bekymrer meg lite", "Ja, ikke så veldig ille", "Ja, og noe svært ille", "Noen ganger", "Svært mye", "Svært ofte",
                                        "For det meste",  "Vanligvis", "Ja, helt klart", "Ganske ofte", "Like mye nå som før", "Bryr meg som før",

                                        "Avgjort like mye", "Ofte","Uten tvil svært ofte",
                                        
                                        #Annet
                                        "Vet ikke",
                                        
                                        #Sanser
                                        "Ikke nedsatt", "Litt nedsatt", "Middels nedsatt", "Mye nedsatt", "Other impairment", "No long term impairment",
                                        
                                        #Last
                                        "Missing", "NA"
                                        
                                      )))
  
}

## List to fill with various items to avoid cluttering the workplace
various <- list()

# Raw data ----
tmp <- read_sav("../Data/updated_2023-01-18_112680_data-og-tillegg.sav", encoding = "latin1")

#Read the raw data from HUNT DB, filter out the individuals in both HUNT3 and HUNT4, impute missing values
raw <- tmp %>%
  # Rename columns for easier work later
  rename(PID = `PID@112680`,
         Age_HUNT3 = `PartAg@NT3BLQ1`,
         Age_HUNT4 = `PartAg@NT4BLM`) %>%
  # Filter out the participants that are in both HUNT3 and HUNT4, while ages >= 65 years when participating in HUNT3
  dplyr::filter(if_any(starts_with("Part@NT3"), ~ . == 1) & `Part@NT4Eld` == 1 & Age_HUNT3 >= 65) %>%
  # Fix some column values
  mutate(
    # First turn everything into factors so column values are readable
    across(everything(), ~as_factor(.)),
    #Turn into characters so column values can be edited
    
    # Chronic impairment values, depending on the answering to "DisChr", participants could skip "Imp" questions automatically giving an NA answer while it is in fact "No impairment"
    # Turn to character so values can be modified
    across(contains("Imp@NT3", ignore.case = FALSE), ~ case_when(
      `DisChr@NT3BLQ1` == "Nei" ~ "No long term impairment",
      `DisChr@NT3BLQ1` == "Ja" & is.na(.) ~ "Other impairment",
      TRUE ~ .)),
    across(contains("Imp@NT4", ignore.case = FALSE), ~ case_when(
      `DisChr@NT4BLQ1` == "Nei" ~ "No long term impairment",
      `DisChr@NT4BLQ1` == "Ja" & is.na(.) ~ "Other impairment",
      TRUE ~ .)),
    
    # Add value for NAs for stimulants questions only to be answered if another question was answered confirmatory
    `SmoNev@NT3BLQ1` = case_when(
      
      !is.na(`SmoPre@NT3BLQ1`) |
        !is.na(`SmoCigOc@NT3BLQ1`) |
        !is.na(`SmoCigarPipeOc@NT3BLQ1`) |
        !is.na(`SmoCigDy@NT3BLQ1`) |
        !is.na(`SmoCigarPipeDy@NT3BLQ1`) ~ 'Røyker eller har røykt tidligere',
      
      TRUE ~ `SmoNev@NT3BLQ1`
      
    ),
    `SmoPre@NT3BLQ1` = case_when(
      
      !is.na(`SmoCigOc@NT3BLQ1`) |
        !is.na(`SmoCigarPipeOc@NT3BLQ1`) |
        !is.na(`SmoCigDy@NT3BLQ1`) |
        !is.na(`SmoCigarPipeDy@NT3BLQ1`) ~ 'Røyker',
      
      TRUE ~ `SmoPre@NT3BLQ1`
      
    ),
    across(`SmoPre@NT3BLQ1`:`SmoCigarPipeDy@NT3BLQ1`,
           ~ case_when(str_detect(`SmoNev@NT3BLQ1`, "Nei") ~ "Aldri røykt", TRUE ~ .)),
    across(`SmoCigOc@NT3BLQ1`:`SmoCigarPipeDy@NT3BLQ1`,
           ~ case_when(str_detect(`SmoPre@NT3BLQ1`, "Nei") ~ "Er tidligere røyker", TRUE ~ .)),
    
    across(`SmoCigOc@NT3BLQ1`:`SmoCigarPipeDy@NT3BLQ1`,
           ~ case_when(str_detect(`SmoStat@NT3BLQ1`, "Daglig") & is.na(.) ~ "Røyker noe annet daglig",
                       str_detect(`SmoStat@NT3BLQ1`, "av og til") & is.na(.) ~ "Røyker noe annet av og til",
                       TRUE ~ .)),
    `AlcL4WInto@NT3BLQ1` = case_when(
      `AlcL4W@NT3BLQ1` == "Nei" ~ "Ikke drukket alkohol siste fire uker",
      TRUE ~ `AlcL4WInto@NT3BLQ1`),
  ) %>%
  #Turn into long format to easier clean up multiple character answer categories
  pivot_longer(.,
               cols = -PID,
               names_to = "Variable",
               values_to = "value") %>%
  #Clean up some answer categories, fixing æøå and using the same spelling for ggr etc throughout
  mutate(value = case_when(
    
    #Change some answer categories
    str_detect(value, "Tar det rolig uten ") ~ "Tar det rolig, blir ikke andpusten eller svett",
    str_detect(value, "jeg blir andpusten og svet") ~ "Tar det hardt, blir andpusten og svett",
    str_detect(value, "Daglig r|Tidligere r|yker av og til|Nei, jeg har aldri") ~ str_replace(value, "r.yk", "røyk"),
    TRUE ~ value),
    value = value %>%
      str_replace("Ã¥", "å") %>%
      str_replace("Ã¸", "ø") %>%
      str_replace("Ã¦", "æ") %>%
      str_replace("3 timer el mer", "3 timer eller mer") %>%
      str_replace("1 g/uke|En gang i uka", "1 gang per uke") %>%
      str_replace("Sjeldnere enn en gang i uka", "Sjeldnere enn 1 gang per uke") %>%
      str_replace("ggr/uka", "ganger per uke") %>%
      str_replace("/uke", " ganger per uke") %>%
      str_replace("ganger i uka", "ganger per uke") %>%
      str_replace("g/mnd.", "ganger per måned") %>%
      str_replace("g siste 6 mnd.", "ganger siste 6 måneder") %>%
      str_replace("Under 1 time", "Mindre enn 1 time") %>%
      str_replace("/dag", " per dag") %>%
      str_replace("\\bel\\b", "eller") %>%
      str_replace("glass per dag", "per dag") %>%
      str_replace("glass per uke", "per uke") %>%
      str_replace("4 glass eller mer per dag", "4 eller mer per dag") %>%
      str_replace("Ca.", "Omtrent") %>%
      str_replace("Aldri/sjelden", "Sjelden eller aldri") %>%
      str_replace("30 minutter-1 time", "30 minutter - 1 time") %>%
      str_replace("16-30 minutter", "15-29 minutter")  %>%
      str_replace("Grunnskole 7-10 år, framhaldsskole, folkehøgskole|7-årig folkeskole eller kortere|9-årig grunnskole|Framhalds- eller fortsettelsesskole|Real- eller middelskole, grunnskolen 10.år|Real- eller middelskole/ ungdomsskole/ 10-årig grunnskole", "Grunnskole") %>%
      str_replace("Realskole, middelskole, yrkesskole 1-2 årig videregående skole|1-2årig videregående skole|Ett- eller toårig videregående skole", "1-2 årig videregående skole") %>%
      str_replace("Artium, øk.gymnas, allmennfaglig retning i videregående skole|Artium, økonomisk gymnas eller almenfaglig retning i vgs.|Gymnas, 3-årig videregående skole", "3 år i videregående skole") %>%
      str_replace("Høgskole/universitet, mindre enn 4 år|Høyskole eller universitet, mindre enn 4 år", "Høyskole/universitet, mindre enn 4 år") %>%
      str_replace("Høgskole/universitet, 4 år eller mer|Høyskole eller universitet, 4 år eller mer", "Høyskole/universitet, 4 år eller mer") %>%
      str_replace("Høyskole eller universitet", "Høyskole/universitet, mindre enn 4 år") #From HUNT3 oral
  ) %>%
  # Turn back into wide format and turn into factors again
  pivot_wider(.,
              names_from = Variable,
              values_from = value) %>%
  mutate(across(everything(), ~as.factor(.))) %>%
  # Change education level to years of education. If lower in HUNT4 than previous HUNT waves, use highest value
  # Assumed unlikely someone increased their education between HUNT3 and HUNT4, more likely mistaken answer category
  mutate(across(starts_with("Educ"), ~ as.character(.)),
         across(starts_with("Educ"), ~ case_when(
           . == "Grunnskole" ~ "10",
           . == "1-2 årig videregående skole" ~ "12",
           . == "Fagbrev eller svennebrev" ~ "12", #Keep same as 2 years vgs
           . == "3 år i videregående skole" ~ "13",
           . == "Høyskole/universitet, mindre enn 4 år" ~ "16",
           . == "Høyskole/universitet, 4 år eller mer" ~ "17",
         )),
         across(starts_with("Educ"), ~ as.numeric(as.character(.))),
         #Max years of education
         education_years_hunt4 = pmax(`Educ@NT2BLQ1`, `Educ@NT4BLQ1`, `Educ@NT1BLQ2`, `Educ@NT3OralQ`, na.rm = TRUE)) %>%
  #Remove education columns from each HUNT wave, keeping only the highest reported level of education
  dplyr::select(-c(`Educ@NT2BLQ1`, `Educ@NT4BLQ1`, `Educ@NT1BLQ2`, `Educ@NT3OralQ`)) %>% 
  #Turn highest education level into factors
  mutate(education_years_hunt4 = as.character(education_years_hunt4),
         education_years_hunt4 = education_years_hunt4 %>%
           str_replace("10", "Primary school") %>%
           str_replace("12", "Vocational education") %>%
           str_replace("13", "Upper secondary school") %>%
           str_replace("16", "University/Community college less than four years") %>%
           str_replace("17", "University/Community college four years or more"),
         education_years_hunt4 = factor(education_years_hunt4, levels = 
                                          c("Primary school", "Vocational education", "Upper secondary school", 
                                            "University/Community college less than four years", "University/Community college four years or more"))
  )
rm(tmp)

# Data exploration ----
# Percentage of missing data in each variable
various$pct_NA_variables <- sapply(raw, function(x) (sum(is.na(x))/nrow(raw))*100, simplify = FALSE ) %>% # Count NAs and calcuate pct missing
  as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "pct_missing")

# Is variable continuous or categorical
various$type_of_variable  <- raw %>%
  #Turn long to use grouped analysis
  mutate(across(starts_with("Age_HUNT"), ~ factor(.)),
         education_years_hunt4 = factor(education_years_hunt4)) %>% #Have values in the same format
  pivot_longer(.,
               cols = everything(),
               names_to = "variable",
               values_to = "value") %>%
  #Identify if variable is numeric or factor
  group_by(variable) %>%
  mutate(type_of_variable = case_when(
    any(!str_detect(value, "[a-z]")) ~ "continuous",
    TRUE ~ "categorical"
  )) %>% ungroup() %>%
  #Only keep variable name and type of variable as factor
  dplyr::select(-value) %>% unique() %>%
  #fix for wor variables
  mutate(
    type_of_variable = case_when(
      str_detect(variable, "WorTi") ~ "categorical",
      TRUE ~ type_of_variable),
    #Turn into factor
    type_of_variable = factor(type_of_variable, levels = c("categorical", "continuous"))
  ) %>%
  rename(original_variable_names = variable) %>%
  #Add janitor cleaned names
  mutate(janitor_names = names(raw %>% clean_names()))

# Categorical variable keys
various$categorical_variables_value_keys <- raw %>%
  #Select the factor variables
  dplyr::select(contains(various$type_of_variable %>%
                    dplyr::filter(type_of_variable == "categorical") %>%
                    dplyr::select(original_variable_names) %>% unique() %>%
                    pull(original_variable_names))) %>%
  #turn long
  pivot_longer(.,
               cols = everything(),
               names_to = "original_variable_names",
               values_to = "value") %>%
  #Get the correct order of the answer categories
  setAnswerFactorValues() %>%
  arrange(original_variable_names, factor_value) %>%
  unique() %>%
  #Add numeric values
  mutate(.by = original_variable_names, numeric_value = seq_along(value)) %>%
  dplyr::select(-c(factor_value)) %>%
  #Add janitor cleaned names
  left_join(., various$type_of_variable %>%
              dplyr::select(original_variable_names, janitor_names) %>%
              rename(Variable = janitor_names)) %>%
  mutate(key = paste0(numeric_value, " = ", value)) %>% unique() %>%
  #Turn into character values for joining
  mutate(across(c(value, Variable), ~as.character(.))) %>%
  drop_na(value) %>%
  rename(character_value = value)

#Janitor names
various$janitor_names <- tibble(
  original_variable_names = names(raw),
  janitor_names = names(raw %>% clean_names())
)

# List of plots to avoid cluttering environment
data_exploration_plots <- list(
  
  #Actualt plots
  plots = list(),
  #Dataframes to use to build plots when needed
  plots_df = list()
  
)

# Exposure and outcome variables
# Only need a tibble with one participant to get the column names
tmp <- raw %>% dplyr::filter(PID == "1126800000077")
various$exposure_outcome_variables <- list(
  
  "Intrinsic capacity" = list(
    
    "Mental health" = tmp %>%
      dplyr::select(PID, c(starts_with(c("HADS", "Feel", "MentHealtS"), ignore.case = FALSE), `SatLif@NT4BLQ1`)) %>%
      dplyr::select(-starts_with("FeelStro")),
    
    "Cognition" = tmp %>%
      dplyr::select(PID, starts_with("Moca")),
    
    "Locomotion" = tmp %>%
      dplyr::select(PID, starts_with(c("Bal", "Gait", "Chair", "Sppb"))) %>%
      dplyr::select(-contains(c("Fla", "Moca"), ignore.case = FALSE)),
    
    "Vitality" = tmp %>%
      dplyr::select(PID, starts_with(c("PooAppeL4W", "GripS", "WeiRedL6M")), `FeelStro@NT4BLQ1`),
    
    "Sensory capacity" = tmp %>%
      dplyr::select(PID, starts_with(c("DisC", "VisImp", "HearImp"))) %>%
      mutate(
        across(contains(c("Imp", "DisC"), ignore.case = FALSE), ~ as.character(.)),
        across(contains("Imp@NT4", ignore.case = FALSE), ~ case_when(
          `DisChr@NT4BLQ1` == "Nei" ~ "No long term impairment",
          TRUE ~ .)),
        across(contains(c("Imp", "DisC"), ignore.case = FALSE), ~ as.factor(.)),
      )
    
  ) %>% lapply(., function(x) {
    x %>%
      #Only select variables from HUNT4 as this is the outcome
      dplyr::select(contains("NT4", ignore.case = FALSE)) %>%
      mutate(across(everything(), ~as.character(.))) %>% 
      pivot_longer(
        cols = everything(),
        names_to = "variable",
        values_to = "value") %>%
      dplyr::select(-value)
  }) %>% bind_rows(., .id = "Subcategory"),
  
  "Healthy Life Index" = list(
    
    #Not relevant ofr RQ1
    # "Lifetime physical activity" = tmp %>%
    #   dplyr::select(PID, starts_with("Exe")) %>%
    #   dplyr::select(-contains("NT4")),
    
    "Physical activity" = tmp %>%
      dplyr::select(PID, starts_with("Exe")) %>%
      dplyr::select(-contains("DuLY")),
    
    "Diet" = tmp %>%
      dplyr::select(PID, starts_with(c("Fo","WhiBread","SemiWho","WhoGr","FatBread","FatCook","Dri"))) %>%
      dplyr::select(-contains(c("Lem", "Jui", "Wat", "Suppl", "Ric", "Sau", "Choc"))),
    
    "Stimulants" = tmp %>%
      dplyr::select(PID, starts_with(c("SmoStat", "Alc"))),
    
    "Sleep" = tmp %>%
      dplyr::select(PID, starts_with(c("Sle", "Insom"))),
    
    "Leisure" = tmp %>%
      dplyr::select(PID, starts_with(c("Act", "Cul"))),
    
    "Social Interaction" = tmp %>%
      dplyr::select(PID, starts_with(c("FeelL"))),
    
    "Work" = tmp %>%
      dplyr::select(PID, starts_with("wor"))) %>%
    lapply(., function(x) {
      x %>%
        #Only select variables from HUNT3 as this is the exposure
        dplyr::select(contains("NT3", ignore.case = FALSE)) %>%
        dplyr::select(-contains("BLQNP")) %>%
        mutate(across(everything(), ~as.character(.))) %>% 
        pivot_longer(
          cols = everything(),
          names_to = "variable",
          values_to = "value") %>%
        dplyr::select(-value)
      }) %>% bind_rows(., .id = "Subcategory"),
  
  "General health" = list(
    
    "Anthropometry" = tmp %>%
      dplyr::select(starts_with(c("WaistCirc","HipCirc","Hei","Wei"))) %>%
      #Remove deviance columns
      dplyr::select(-contains("Dev")),
    
    "Other measurements" = tmp %>%
      dplyr::select(starts_with(c("BP","Se","Healt", "SatLifSty@NT4BLQ1"))) %>%
      dplyr::select(-c(Sex, starts_with("SemiWho"))),
    
    "Frailty Multimorbidity" = tmp %>%
      dplyr::select(starts_with(
        c("Healt", "SatLif@", "DisChr", "HearI", "VisI", "MotImp", "DisSomImp", "DisPsycImp", "DisLimL4W")),
        contains("Ev@", ignore.case = FALSE)) %>%
      dplyr::select(contains("NT3"))
    
  ) %>%
    lapply(., function(x) {
      x %>%
        dplyr::select(-contains("BLQNP")) %>%
        mutate(across(everything(), ~as.character(.))) %>% 
        pivot_longer(
          cols = everything(),
          names_to = "variable",
          values_to = "value") %>%
        dplyr::select(-value)
      }) %>% bind_rows(., .id = "Subcategory")
) %>% bind_rows(., .id = "Category") #

#Filter out the original variables in the project description
various$original_variables_to_use <- various$exposure_outcome_variables %>%
  #Only keep total score of the HADS, SSPB, MOCA
  dplyr::filter(
    !(str_detect(variable, "Sppb|Moca|Gait|Bal|Chair") & !str_detect(variable, "Tot|Anx|Depr")) &
      !Subcategory %in% c("Lifetime physical activity") &
      !(Subcategory == "Vitality" & !variable %in% c("GripStre@NT4Eld", "WeiRedL6M@NT4BLQ1")) & 
      !(str_detect(variable, "Alc") & !str_detect(variable, "Red|Crit|Guilt|Morn") ) &
      !str_detect(variable, "DisChr|Feel|MentHealtS|SatLif|Insom|ActMor30Min") & Category != "General health" &
      !Subcategory == "Work"
  )

#The exposure/outcome variables to be used in the analyses, after missingness etc has been looked at
various$variables_to_use_analysis <- various$exposure_outcome_variables %>%
  #Only keep total score of the HADS, SSPB, MOCA
  dplyr::filter(
    !(str_detect(variable, "Sppb|Moca|Gait|Bal|Chair") & !str_detect(variable, "Tot|Anx|Depr")) &
      !(str_detect(variable, "Sppb|Moca|Gait|Bal|Chair") & str_detect(variable, "TotSC")) &
      !Subcategory %in% c("Lifetime physical activity") &
      !(Subcategory == "Vitality" & !variable %in% c("GripStre@NT4Eld", "WeiRedL6M@NT4BLQ1")) & 
      !(str_detect(variable, "Alc") & !str_detect(variable, "TotUnit") ) &
      !(str_detect(variable, "Feel") & Category == "Intrinsic capacity" ) &
      !str_detect(variable, "DisChr|MentHealtS|Sle|ActMor30Min|HADS|WeiRedL6M|WhiBread|GrBread") & 
      !Subcategory %in% c("Anthropometry", "Other measurements") &
      !Subcategory %in% c("Work", "Leisure") 
  )

## Save the pct missing of the variables that were used
various$pct_NA_hli_ici <- various$pct_NA_variables %>%
  dplyr::filter(variable %in% various$variables_to_use_analysis$variable) %>%
  # Format to fit descriptive stat table
  mutate(subcategory = case_when(
    
    # Healthy life index
    str_detect(variable, "Exe") ~ "Physical activity",
    variable == "FeelLoneL2W@NT3BLQ1" ~ "Social Interaction",
    variable == "SmoStat@NT3BLQ1" ~ "Smoking",
    variable == "AlcTotUnitW@NT3BLQ1" ~ "Alcohol",
    str_detect(variable, "Insom") ~ "Sleep",
    variable %in% c("FoFruF@NT3BLQ1", "FoVegF@NT3BLQ1") ~ "FruitVegetable_intake",
    variable == "FoFishF@NT3BLQ1" ~ "Fish_intake",
    variable == "DriMilk1Gl@NT3BLQ1" ~ "Milk_saturated_fat;Milk total;Milk high fat",
    variable == "DriMilk2Gl@NT3BLQ1" ~ "Milk_saturated_fat;Milk total;Milk low fat",
    variable %in% c("FatBreadTyp@NT3BLQ2", "FatCookTyp@NT3BLQ2") ~ "Saturated_fat_intake",
    
    # Intrinsic capacity index
    variable == "SppbTotS@NT4Eld" ~ "Locomotion",
    variable == "MocaTotS@NT4Eld" ~ "Cognition",
    variable == "GripStre@NT4Eld" ~ "Vitality",
    variable == "SatLif@NT3BLQ1" ~ "Mental health",
    variable == "HearImp@NT4BLQ1" ~ "Hearing",
    variable == "VisImp@NT4BLQ1" ~ "Vision"
  )) %>% drop_na() %>%
  #Separate milk into the different categories
  separate_rows(., subcategory, sep = ";") %>%
  #Find the range of missingness
  mutate(pct_missing = round(pct_missing, 1)) %>%
  mutate(.by = subcategory,
         range = case_when(
           min(pct_missing) != max(pct_missing) ~ paste0(min(pct_missing), "-", max(pct_missing), "%"),
           TRUE ~ paste0(max(pct_missing), "%")
         )
  ) %>% select(subcategory, range) %>% distinct() %>% rename("Percent imputed values" = range)
saveRDS(various$pct_NA_hli_ici, "./RQ1/R_output/pct_NA_hli_ici.Rds")

# Missing values of the original exposure and outcome variables
# Percent missing of the original outcome/exposure variables
tmp <- various$original_variables_to_use %>%
  left_join(various$pct_NA_variables) %>%
  #Arrange in order
  arrange(Category, Subcategory, variable) %>%
  # Set factor order for plot
  mutate(Category = factor(Category, levels = c("Healthy Life Index", "Intrinsic capacity")),
         Subcategory = factor(Subcategory, levels = c("Diet", "Leisure", "Physical activity", "Sleep", "Stimulants",
                                                      "Locomotion", "Cognition", "Vitality", "Mental health", "Sensory capacity"))
         )

# Build heatmap annotations
data_exploration_plots$plots_df$heatmap_anno <- HeatmapAnnotation(
  Category = tmp$Category,
  Subcategory = tmp$Subcategory,
  annotation_legend_param = list(
    Category = list(
      title = "Category",
      labels = unique(tmp$Category)),
    Subcategory = list(
      title = "Subcategory",
      at = unique(tmp$Subcategory),
      labels = unique(tmp$Subcategory))
  ),
  `Pct missing` = anno_barplot(tmp$pct_missing, ylim = c(0, 65), height = unit(3, "cm"),
                               axis_param = list(
                                 side = "right",
                                 at = c(0, 20, 40, 60),
                                 labels = c("0% missing", "20% missing", "40% missing", "60% missing")
                               )),
  #Colors
  col = list(
    Category = color_vectors$categories %>% enframe(., name = "Category", value = "color_value") %>%
      right_join(., tmp %>% mutate(Category = as.character(Category)) %>% dplyr::select(variable, Category)) %>% dplyr::select(Category, color_value) %>% deframe(),
    Subcategory = color_vectors$categories %>% enframe(., name = "Subcategory", value = "color_value") %>%
      right_join(., tmp %>% mutate(Subategory = as.character(Subcategory)) %>% dplyr::select(variable, Subcategory)) %>%
      dplyr::select(Subcategory, color_value) %>% deframe()
  ),
  #Row annotation
  which = "column",
  #Don't show in legend
  show_annotation_name = FALSE)
    
# Build plot
# Heatmap matrix
tmp <- raw %>% dplyr::select(contains(various$original_variables_to_use$variable)) %>%
  #Use 0's and 1's for missing/not missing
  mutate(across(everything(), ~case_when(
    
    !is.na(.) ~ "Present",
    TRUE ~ "Missing"
    
  ))) %>% as.matrix()
  
  #Draw plot
data_exploration_plots$heatmap_missing_values <- Heatmap(tmp,
                                                        name = "Data completeness",
                                                        top_annotation = data_exploration_plots$plots_df$heatmap_anno,
                                                        col = c("#FFFFFF", "#000000"),
                                                        cluster_rows = FALSE,
                                                        cluster_columns = FALSE,
                                                        column_title = paste0("Data completeness variables needed to answer RQ 1"),
                                                        show_column_names = FALSE,
                                                        show_row_names = FALSE,
                                                        use_raster = TRUE, raster_quality = 10)
  


## Mental health variables----
# Check how well the "Life Satisfaction" variable correlates with the HADS and CONOR mental health questionnaires
tmp <- raw %>%
  dplyr::select(contains(c("PID", "SatLif@", "HADSAnxi@NT4", "HADSDepr@NT4", "HADSTot@NT4", "MentHealtS@NT4"))) %>%
  #Use janitor names
  clean_names() %>%
  #Turn into numerics for plot
  mutate(across(everything(), ~as.character(.)),
         across(starts_with("sat_lif"), ~case_when(
           . == "Svært misfornøyd" ~ "1",
           . == "Meget misfornøyd" ~ "2",
           . == "Nokså misfornøyd" ~ "3",
           . == "Både/og" ~ "4",
           . == "Ganske fornøyd" ~ "5",
           . == "Meget fornøyd" ~ "6",
           . == "Svært fornøyd" ~ "7")),
         across(everything(), ~as.numeric(as.character(.)))) %>%
  #Turn tidy and split into a HUNT 3 and HUNT 4 category
  pivot_longer(.,
               cols = -c(pid, contains("Sat")),
               names_to = "MH_questionnaire",
               values_to = "MH_score") %>%
  pivot_longer(., 
               cols = contains("Sat"),
               names_to = "LifeSatisfaction",
               values_to = "LifeSatisfaction_value")

# Plot
data_exploration_plots$plots$lifesatisfaction_mentalhealth <- ggplot(tmp, aes(x = factor(LifeSatisfaction_value), y = MH_score)) +
  #Add half violin/half boxplot and counts of how many participants are in each satlif answer category
  geom_half_boxplot(side = "r") +
  geom_half_violin() +
  geom_text(
    data = tmp %>%
      mutate(
        # Y axis position of text of individuals per score
        yloc = case_when(
          str_detect(MH_questionnaire, "hads") ~ -0.75,
          TRUE ~ 0.95
      )) %>%
      group_by(LifeSatisfaction_value, MH_questionnaire, yloc) %>%
      summarise(N = sum(!(is.na(MH_score)))) %>%
      ungroup() %>%
      mutate(label = paste0("n = ", N)),
    mapping = aes(y = yloc, label = label),
    size = 3
  ) +
  #Change labs
  labs(
    title = "Mental health score vs self-assessed life satisfaction score",
    y = "Mental health test score",
    x = "Self-assessed life satisfaction"
  ) +
  #One plot for each HADS score
  facet_wrap(~MH_questionnaire, scales = "free_y", ncol = 4, nrow = 1,
             #Fix labels
             labeller = as_labeller(
               c
               ("hads_anxi_nt4blq2" = "HADS Anxiety",
                "hads_depr_nt4blq2" = "HADS Depression", 
                "hads_tot_nt4blq2" = "HADS Total",
                 "ment_healt_s_nt4blq1" = "CON-MHI")))
  
# Data imputation ----
## To impute
## Only impute variables with <= 20% missing data
tmp <- raw %>%
  dplyr::select(various$pct_NA_variables %>% dplyr::filter(pct_missing <= 20) %>% pull(variable)) %>%
  # Remove columns that should not be imputed
  dplyr::select(-contains(c("PID", "Part@", "GaitAid"))) %>% 
  # Format column values in line with mixgb
  mutate(across(starts_with("WorTi"), ~as.character(.)),
         `WorTitISCO2@NT3BLI` = na_if(`WorTitISCO2@NT3BLI`, ""),
         `WorTitISCO3@NT3BLI` = na_if(`WorTitISCO2@NT3BLI`, ""),
          across(starts_with("WorTi"), ~as.factor(.)),
          across(contains(various$type_of_variable %>% 
                           #dplyr::filter(!str_detect(janitor_names, "wor_")) %>%
                            dplyr::filter(type_of_variable == "continuous") %>%
                           pull(., original_variable_names)),
                ~as.numeric(as.character(.))),
         across(contains(various$type_of_variable %>% 
                           #dplyr::filter(!str_detect(janitor_names, "wor_")) %>%
                           dplyr::filter(type_of_variable != "continuous") %>%
                           pull(., original_variable_names)),
                ~as.numeric()),
  ) %>%
  #Create cleaner names using janitor for mixgb
  clean_names() #%>%
  #Check with mixgb cleaning function
  #mixgb::data_clean()

#Set seed for imputation
set.seed(2022)

#check how many nrounds mixgb recommends tu run
cv_results <- mixgb_cv(tmp, verbose = FALSE)
cv_results$response
cv_results$best.nrounds
#Go with 30 as most best.nrounds were below this number
cv_results <- NULL

### Run xgboost imputation----
imputed_data <- mixgb(tmp, nrounds = 30)
#Name the df's
names(imputed_data) <- c("imputation1", "imputation2", "imputation3", "imputation4", "imputation5")
## Save imputation data----
#Mixgb results alone
saveRDS(imputed_data, "./RQ1/R_output/all_mixgb_imputations.Rds")

#Use median values for analysis
imputed_data <- readRDS("./RQ1/R_output/all_mixgb_imputations.Rds")

median_imputed_values <- imputed_data %>%
  #Bind all the imputed dataframes together, find the median value of all variables for each participant
  lapply(., function(x) {x %>% bind_cols(raw %>% dplyr::select(PID))}) %>%
  bind_rows(., .id = "imputed_df") %>%
  #Turn long to use summarise
  mutate(across(-PID, ~as.character(as.factor(.)))) %>% #Must be character so both continuous and categorical values van be in one column
  pivot_longer(.,
               cols = -c(imputed_df, PID),
               names_to = "Variable",
               values_to = "character_value") %>%
  #Add the categorical value keys
  left_join(various$categorical_variables_value_keys %>%
              dplyr::select(Variable, character_value, numeric_value)) %>%
  dplyr::filter(!str_detect(Variable, "wor_tit")) %>%
  #Use the numeric values to take the median
  mutate(numeric_value = case_when(
    is.na(numeric_value) ~ as.numeric(character_value),
    TRUE ~ numeric_value)) %>% 
  mutate(.by = c(PID, Variable), value = median(numeric_value)) %>%
  dplyr::select(-c(ends_with("_value"), imputed_df)) %>%
  distinct() %>%
  #Turn the categorical values back to characters, pivot wider and turn them to factors and continuous to numerics
  left_join(., various$categorical_variables_value_keys %>% rename(value = numeric_value) %>% dplyr::select(-c(key, original_variable_names)),
            by = c("Variable", "value")) %>%
  mutate(
    value = as.character(value),
    value = case_when(
      !is.na(character_value) ~ character_value,
      TRUE ~ value
  )) %>% dplyr::select(-character_value) %>%
  #Add back the original HUNT variable names
  left_join(., various$janitor_names %>% rename(Variable = janitor_names)) %>%
  dplyr::select(-Variable) %>%
  pivot_wider(.,
              names_from = original_variable_names,
              values_from = value) %>%
  #Turn to factor/numeric
  mutate(
    across(contains(various$categorical_variables_value_keys %>% pull(original_variable_names)), ~as.factor(.)),
    across(-c(PID,contains(various$categorical_variables_value_keys %>% pull(original_variable_names))), ~as.numeric(.))) %>%
  #Order columns
  dplyr::select(PID, everything())
  
# Select the columns to be used for analysis
data_for_analysis <- median_imputed_values %>%
  dplyr::select(c(PID, Sex, contains(c("Age", "Ev", "education", "Marit"), ignore.case = FALSE),
           contains(various$variables_to_use_analysis %>% dplyr::filter(!Subcategory == "Frailty Multimorbidity") %>% pull(variable)))) %>% 
  rename(marital_status = `MaritStat@NT3BLQ1`) %>%
  #Turn Sex into english, and create only two marriage status categories
  mutate(
    Sex = as.character(Sex) %>% str_replace_all(c("Kvinne" = "Women", "Mann" = "Men")),
    marital_status = as.character(marital_status),
    marital_status = case_when(
      marital_status == "Gift" ~ "Married",
      TRUE ~ "Unmarried/Widow/Divorced/Separated")
    )
  
# Operationalise frailty and multimorbidity as explained in the project description for RQ2-3
tmp <- median_imputed_values %>%
  dplyr::select(c(PID, 
           contains(
             various$variables_to_use_analysis %>%
               dplyr::filter(Subcategory == "Frailty Multimorbidity") %>% pull(variable)))) %>%
  #Find number of chronic illnesses for the summary frailty/multimorbdity index
  pivot_longer(
    cols = contains("Ev@", ignore.case = FALSE),
    names_to = "Disease",
    values_to = "Presence_of_disease") %>%
  summarise(.by = c(PID, contains(c("Healt", "SatLif", "Imp", "LimL4W", "Presence_of_disease"), ignore.case = FALSE)),
                    nr_chronic_illness = n()) %>%
  dplyr::filter(Presence_of_disease == "Ja") %>% dplyr::select(-Presence_of_disease) %>% distinct() %>%
  #Operationalise frailty measures
  mutate(
    general_health = case_when(
      `Healt@NT3BLQ1` %in% c("Dårlig", "Ikke helt god") ~ "Frailty_predisposition",
      TRUE ~ "No_frailty_predisposition"),
    mental_health = case_when(
      `SatLif@NT3BLQ1` %in% c("Meget misfornøyd","Svært misfornøyd") ~ "Frailty_predisposition",
      TRUE ~ "No_frailty_predisposition"),
    physical_impairment = case_when(
      (`MotImp@NT3BLQ1` %in% c("Middels nedsatt", "Mye nedsatt") | 
        `DisSomImp@NT3BLQ1` %in% c("Middels nedsatt", "Mye nedsatt") | 
        `VisImp@NT3BLQ1` %in% c("Middels nedsatt", "Mye nedsatt") |
        `HearImp@NT3BLQ1` %in% c("Middels nedsatt", "Mye nedsatt")) ~ "Frailty_predisposition",
      TRUE ~ "No_frailty_predisposition"),
    social_impairment = case_when(
      `DisLimL4W@NT3BLQ1` %in% c("Mye", "Kunne ikke ha sosial omgang") ~ "Frailty_predisposition",
      TRUE ~ "No_frailty_predisposition")
    ) %>%
  #Clean up columns and count frailty measures
  dplyr::select(-contains("NT3")) %>%
  pivot_longer(cols = -c(PID, nr_chronic_illness),
               names_to = "frailty_measure",
               values_to = "frailty_operationalisation") %>%
  mutate(.by = c(PID, frailty_operationalisation), nr_frailty_measure = n()) %>%
  dplyr::filter(frailty_operationalisation == "Frailty_predisposition") %>%
  dplyr::select(-starts_with("frailty")) %>%
  distinct() %>%
  mutate(frailty_multimorbidity = case_when(
    
    nr_chronic_illness >= 3 & nr_frailty_measure >= 2 ~ "Frail_multimorbid"
    
  )) %>% drop_na()%>% dplyr::select(PID, frailty_multimorbidity) %>%
  #Add back all the participants without frailty
  right_join(data_for_analysis %>% dplyr::select(PID)) %>%
  replace_na(list(frailty_multimorbidity = "Not frail_multimorbid")) %>%
  #Turn to factor
  mutate(frailty_multimorbidity = factor(frailty_multimorbidity, levels = c("Frail_multimorbid", "Not frail_multimorbid")))
  
# Add back to data for analysis
data_for_analysis <- data_for_analysis %>% left_join(tmp) 

# Save imputed dataframe and the various dataframes for more data exploration
saveRDS(data_for_analysis, "./RQ1/R_output/cleaned_imputed_data_for_analysis.Rds")
variables_to_use_analysis <- various$variables_to_use_analysis
saveRDS(variables_to_use_analysis, "./RQ1/R_output/variables_for_analysis.Rds")

# Operatinalise healthy life index and intrinsic capacity index----
data_for_analysis <- readRDS("./RQ1/R_output/cleaned_imputed_data_for_analysis.Rds")
variables_to_use_analysis <- readRDS("./RQ1/R_output/variables_for_analysis.Rds")
pct_NA_hli_ici <- readRDS("./RQ1/R_output/pct_NA_hli_ici.Rds")

## Factor order for plots and tables
factor_orders <- list(
  "Healthy Life Index" = c("Physical activity", "Social Interaction", "Smoking", "Alcohol", "Sleep",
                           "FruitVegetable_intake", "Fish_intake", "Milk total", "Milk_saturated_fat",
                           "Milk high fat", "Milk low fat", "Saturated_fat_intake"
  ),
  "Healthy Life Index_labels" = c("Physical activity", "Social interaction", "Smoking", "Alcohol intake", "Sleep quality",
                                  "Fruit and vegetables intake", "Fatty fish intake", "Milk total", "Choose low fat dairy",
                                  "Milk high fat", "Milk low fat", "Choose oils or soft margarine on bread or cooking"),
  "risk_order" = c("High risk", "Moderate risk", "No/mild risk"),
  "Intrinsic capacity" = c("Locomotion", "Cognition", "Vitality", "Mental health", "Hearing", "Vision"),
  "impairment_order" = c("High impairment", "Moderate impairment", "No/mild impairment"),
  "participant_meta_variables" = c("age_hunt3", "age_hunt4", "sex", "marital_status", "education_years_hunt4", "number_of_illness"),
  "participant_meta_variables_labels" = c("age_hunt3", "age_hunt4", "Sex", "Marital status", "Education level", "Number of long term conditions"),
  "participant_meta_values" = c("Age", "Women", "Men", "Married", "Unmarried/Widow/Divorced/Separated",
                                "Primary school", "Vocational education", "Upper secondary school", "University/Community college less than four years",
                                "University/Community college four years or more", "None", "One condition", "Two conditions", "Three or more conditions")
)

#Add a factor column with 5-year age groups to the dataframe to separate participants by.
#' @param df A dataframe with HUNT data a PID column with unique ID's for each participant, and Age_HUNT3 and/or Age_HUNT4 columns
#' @param HUNT_survey Should age groups be made from participant's age at HUNT3 or HUNT4?
addAgegroup <- function(df, HUNT_survey = "3") {
  
  #Janitor clean names
  cleaned_names <- df %>%
    dplyr::select(starts_with("age")) %>%
    clean_names()
  
  if(HUNT_survey == "3") {
    
    added_age <- cleaned_names %>%
      #Create age groups for easier plots
      mutate(Age_group = case_when(
        age_hunt3 <= 69.9999999 ~ "65-69",
        age_hunt3 <= 74.9999999 ~ "70-74",
        age_hunt3 <= 79.9999999 ~ "75-79",
        age_hunt3 <= 84.9999999 ~ "80-84",
        age_hunt3 >= 85 ~ "85+"
      ))
    
  } else if(HUNT_survey == 4) {
    
    added_age <- cleaned_names %>%
      #Create age groups for easier plots
      mutate(Age_group = case_when(
        age_hunt4 <= 69.9999999 ~ "65-69",
        age_hunt4 <= 74.9999999 ~ "70-74",
        age_hunt4 <= 79.9999999 ~ "75-79",
        age_hunt4 <= 84.9999999 ~ "80-84",
        age_hunt4 >= 85 ~ "85+"
      ))
    
  }
  
  added_age <- bind_cols(df, added_age)
  
  added_age  
}

#Format the data
tmp <- data_for_analysis %>%
      #Get the variables that are used in the HLI
      dplyr::select(c(PID, Sex, contains(variables_to_use_analysis %>%
                 pull(variable)))) %>%
      #Turn into character
      mutate(across(everything(), ~as.character(.))) %>%
      pivot_longer(.,
                   cols = -c(PID, Sex, contains("Age")),
                   names_to = "variable",
                   values_to = "value"
                   ) %>%
  left_join(., variables_to_use_analysis) %>%
  mutate(Subcategory = case_when(
    str_detect(variable, "Smo") ~ "Smoking",
    str_detect(variable, "Alc") ~ "Alcohol",
    TRUE ~ Subcategory)) %>%
  #Split into indivudal columns
  split(.$Subcategory) 

#Compare the imputed value distributions with the raw data
test <- tmp$Vitality %>%
  left_join(raw %>% dplyr::select(PID, contains(tmp$Vitality$variable)) %>% 
              pivot_longer(., cols = -PID, names_to = "variable", values_to = "original_value")) %>%
  rename(imputed = value,
         original = original_value) %>%
  pivot_longer(.,
               cols = c(original, imputed),
               names_to = "Dataframe",
               values_to = "value") %>%
  mutate(value = as.numeric(value))

ggplot(test, aes(x = value, fill = Sex)) + geom_density() + facet_wrap(~Dataframe, scales = "free")
  
## Healthy Life Index----
#Select variables
format_indexes_columns <- function(df) {
  
  df %>%
    clean_names() %>%
    mutate(pid = as.numeric(pid)) %>%
    dplyr::select(contains(c("pid", "category", "operationalisation", "numeric")))
  
}

# Operationalisation
operationalisation_scores <- list()

### Alcohol----
operationalisation_scores$operationalisation$Alcohol <- tmp$Alcohol %>%
  #Turn back to numeric
  mutate(
    value = as.numeric(value),
    #Operationalisation
    operationalisation = case_when(
      value <= 2 ~ "No/mild risk",
      value <= 6 ~ "Moderate risk",
      value >6 ~ "High risk"),
    operationalisation2 = case_when(
      value <= 4 ~ "No/mild/moderate risk",
      value >4 ~ "Moderate/high risk")) %>%
  rename(numeric = value)
  
### Diet----
operationalisation_scores$operationalisation$Diet <- tmp$Diet %>%
  #Is the individual food categories at risk or not
  #Turn wide
  pivot_wider(.,
            names_from = variable,
            values_from = value)  %>%
  #Use janitor name
  clean_names() %>%
  # Compare with guideline intakes
  mutate(
    fish_intake = case_when(
      #All other categories give an intake of >=2 servings/week
      fo_fish_f_nt3blq1 == "0-3 ganger per måned" ~ "High risk",
      TRUE ~ "Not at risk"),
    #Find the number of days a week a participant eat fruit/veg or fish
    across(c("fo_fru_f_nt3blq1", "fo_veg_f_nt3blq1", "fo_pot_f_nt3blq1", "fo_fish_f_nt3blq1"), ~case_when(
      
      #Use the mean for daily intake
      . == "4-6 ganger per uke" ~ "0.7",
      . == "1 gang per dag" ~ "1",
      . == "1-3 ganger per uke" ~ "0.3",
      . == "0-3 ganger per måned" ~ "0",
      . == "2 ganger eller mer per dag" ~ "2"
      
    )),
    across(c("fo_fru_f_nt3blq1", "fo_veg_f_nt3blq1", "fo_pot_f_nt3blq1", "fo_fish_f_nt3blq1"), ~as.numeric(as.character(.))),
    #At risk or not
    fruit_vegetables_intake = case_when(
      fo_fru_f_nt3blq1 + fo_veg_f_nt3blq1 + fo_pot_f_nt3blq1 < 5 ~ "High risk",
      TRUE ~ "Not at risk"
    ),
    #Glasses of milk/day
    across(c("dri_milk1gl_nt3blq1", "dri_milk2gl_nt3blq1"), ~case_when(
      
      . == "Sjelden eller aldri" ~ "0",
      . == "1-6 per uke" ~ "0.4",
      . == "1 per dag" ~ "1",
      . == "2-3 per dag" ~ "2.5",
      . == "4 eller mer per dag" ~ "4"
      
    )),
    across(c("dri_milk1gl_nt3blq1", "dri_milk2gl_nt3blq1"), ~as.numeric(as.character(.))),
    #At risk or not, for only drinkink low fat, high fat or both
    milk_total_intake = case_when(
      dri_milk1gl_nt3blq1 + dri_milk2gl_nt3blq1 < 2 ~ "High risk",
      TRUE ~ "Not at risk"
    ),
    milk_high_fat_intake = case_when(
      dri_milk1gl_nt3blq1 < 2 ~ "High risk",
      TRUE ~ "Not at risk"
    ),
    milk_low_fat_intake = case_when(
      dri_milk2gl_nt3blq1 < 2 ~ "High risk",
      TRUE ~ "Not at risk"
    ),
    #Chooses more high fat milk than not
    choose_low_fat_milk_intake = case_when(
      dri_milk1gl_nt3blq1 > dri_milk2gl_nt3blq1 ~ "High risk",
      dri_milk1gl_nt3blq1 == dri_milk2gl_nt3blq1 ~ "Moderate risk",
      TRUE ~ "Not at risk"
    ),
    #Choose soft margarine or oils
    use_unsaturatedFat_intake = case_when(
      (fat_cook_typ_nt3blq2 %in% c("Myk/lett margarin", "Oljer") &
        fat_bread_typ_nt3blq2 %in% c("Bruker ikke", "Myk/lett margarin", "Oljer")) ~ "Not at risk",
      (fat_cook_typ_nt3blq2 %in% c("Myk/lett margarin", "Oljer") &
         !fat_bread_typ_nt3blq2 %in% c("Bruker ikke", "Myk/lett margarin", "Oljer")) |
        (!fat_cook_typ_nt3blq2 %in% c("Myk/lett margarin", "Oljer") &
           fat_bread_typ_nt3blq2 %in% c("Bruker ikke", "Myk/lett margarin", "Oljer")) ~ "Moderate risk",
      TRUE ~ "High risk"
    ),
    #Risk/not at risk as factors
    across(ends_with("_intake"), ~as.factor(.))) %>%
  #Intake/day for each
  mutate(
    fruit_intake_day = fo_fru_f_nt3blq1,
    potato_intake_day = fo_pot_f_nt3blq1,
    vegetable_intake_day = fo_veg_f_nt3blq1,
    fruit_and_veg_intake_day = fo_fru_f_nt3blq1 + fo_veg_f_nt3blq1,
    fruit_and_veg_and_potato_intake_day = fo_fru_f_nt3blq1 + fo_veg_f_nt3blq1 + fo_pot_f_nt3blq1,
    potato_and_vegetable_intake_day = fo_veg_f_nt3blq1 + fo_pot_f_nt3blq1,
    milk_total_intake_day = dri_milk1gl_nt3blq1 + dri_milk2gl_nt3blq1,
    milk_low_fat_intake_day = dri_milk2gl_nt3blq1,
    milk_high_fat_intake_day = dri_milk1gl_nt3blq1,
    fish_intake_day = fo_fish_f_nt3blq1,
    choose_low_fat_milk_intake_day = case_when(
      dri_milk1gl_nt3blq1 > dri_milk2gl_nt3blq1 ~ 1,
      dri_milk2gl_nt3blq1 == dri_milk1gl_nt3blq1 ~ 0.5,
      TRUE ~ 0),
    use_unsaturatedFat_intake_day = case_when(
      use_unsaturatedFat_intake == "High risk" ~ 1,
      TRUE ~ 0)
  ) %>%
  #Remove originals
  dplyr::select(-ends_with(c("blq1", "blq2"))) %>%
  #Turn longer, needs to be done in two rounds
  pivot_longer(.,
               cols = ends_with("_intake"),
               names_to = "dietary_factor",
               values_to = "risk_status_dietary_factor") %>%
  dplyr::select(-c(dietary_factor, risk_status_dietary_factor)) %>%
  unique() %>%
  pivot_longer(.,
               cols = ends_with("_day"),
               names_to = "dietary_factor",
               values_to = "value") %>% #Portions per day
  mutate(dietary_factor = case_when(
    dietary_factor == "milk_total_intake_day" ~ "Milk total",
    dietary_factor == "milk_low_fat_intake_day" ~ "Milk low fat",
    dietary_factor == "milk_high_fat_intake_day" ~ "Milk high fat",
    dietary_factor == "choose_low_fat_milk_intake_day" ~ "Milk_saturated_fat", # 1 = higher intake
    dietary_factor == "use_unsaturatedFat_intake_day" ~ "Saturated fat intake", # 1 = higher intake
    dietary_factor == "fish_intake_day" ~ "Fish",
    dietary_factor == "potato_intake_day" ~ "Potatoes",
    dietary_factor == "vegetable_intake_day" ~ "Vegetables",
    dietary_factor == "fruit_intake_day" ~ "Fruit",
    dietary_factor == "potato_and_vegetable_intake_day" ~ "Potatoes and vegetables",
    dietary_factor == "fruit_and_veg_intake_day" ~ "Vegetables and fruit",
    dietary_factor == "fruit_and_veg_and_potato_intake_day" ~ "Potatoes, vegetables and fruit",
  )) %>%
  #Add back individual risk for each dietary factor for sankey plot
  mutate(risk_dietary_factor = case_when(
    dietary_factor == "Milk total" & value < 1 ~ "High risk",
    dietary_factor == "Milk total" & value < 2 ~ "Moderate risk",
    dietary_factor == "Milk total" & value >= 2 ~ "No/mild risk",
    dietary_factor == "Milk low fat" & value < 1 ~ "High risk",
    dietary_factor == "Milk low fat" & value < 2 ~ "Moderate risk",
    dietary_factor == "Milk low fat" & value >= 2 ~ "No/mild risk",
    dietary_factor == "Milk high fat" & value < 1 ~ "High risk",
    dietary_factor == "Milk high fat" & value < 2 ~ "Moderate risk",
    dietary_factor == "Milk high fat" & value >= 2 ~ "No/mild risk",
    dietary_factor == "Milk_saturated_fat" & value == 1 ~ "High risk",
    dietary_factor == "Milk_saturated_fat" & value == 0.5 ~ "Moderate risk",
    dietary_factor == "Milk_saturated_fat" & value == 0 ~ "No/mild risk",
    dietary_factor == "Saturated fat intake" & value == 1 ~ "High risk",
    dietary_factor == "Saturated fat intake" & value == 0 ~ "No/mild risk",
    dietary_factor == "Fish" & value < (1/7) ~ "High risk",
    dietary_factor == "Fish" & value < 0.3 ~ "Moderate risk",
    dietary_factor == "Fish" & value >= 0.3 ~ "No/mild risk",
    dietary_factor == "Vegetables and fruit" & value < 2 ~ "High risk",
    dietary_factor == "Vegetables and fruit" & value < 4 ~ "Moderate risk",
    dietary_factor == "Vegetables and fruit" & value >= 4 ~ "No/mild risk"
  )) %>%
  mutate(subcategory = case_when(
    str_detect(dietary_factor, "Milk")  ~ dietary_factor,
    dietary_factor == "Fish" ~ "Fish_intake",
    dietary_factor == "Vegetables and fruit" ~ "FruitVegetable_intake",
    dietary_factor == "Saturated fat intake" ~ "Saturated_fat_intake"
  )) %>%
  rename(operationalisation = risk_dietary_factor) %>%
  #Only keep necessary columns, keep numeric values in their own column
  drop_na(operationalisation) %>%
  rename(numeric = value)

### Physical activity----
operationalisation_scores$operationalisation$`Physical activity` <- tmp$`Physical activity` %>%
  rename(character_value = value) %>%
  #Find the amount of activity/week.
  #New values
  mutate(
    value = case_when(
      #Change duration to minutes, using the mean value of the interval
      str_detect(variable, "ExeDu") & character_value == "30 minutter - 1 time" ~ 45,
      str_detect(variable, "ExeDu") & character_value == "15-29 minutter" ~ 22.5,
      str_detect(variable, "ExeDu") & character_value == "Mindre enn 15 minutter" ~ 7.5, #Check what others have done with this value
      str_detect(variable, "ExeDu") & character_value == "Mer enn 1 time" ~ 60, #Check what others have done with this value
      #Change frequency to the mean frequency per week, check how others have done this
      str_detect(variable, "ExeF") & character_value == "Sjeldnere enn 1 gang per uke" ~ 0,
      str_detect(variable, "ExeF") & character_value == "1 gang per uke" ~ 1,
      str_detect(variable, "ExeF") & character_value == "2-3 ganger per uke" ~ 2.5,
      str_detect(variable, "ExeF") & character_value == "Omtrent hver dag" ~ 5,
      #Intensity, give twice the amount for hard activty than slow activity, following national guidelines
      str_detect(variable, "ExeInt") & character_value == "Tar det hardt, blir andpusten og svett" ~ 2,
      str_detect(variable, "ExeInt") & character_value == "Tar meg nesten helt ut" ~ 2.0000001, #To separate from above answer later
      str_detect(variable, "ExeInt") & character_value == "Tar det rolig, blir ikke andpusten eller svett" ~ 1)) %>%
  dplyr::select(PID, variable, value) %>%
  #Pivot wider to calculate minutes per week activity
  pivot_wider(.,
              names_from = variable,
              values_from = value) %>%
  #Janitor names
  clean_names() %>%
  mutate(minutes_week_moderate = exe_du_nt3blq1*exe_f_nt3blq1*exe_int_nt3blq1,
         operationalisation = case_when(
           minutes_week_moderate < 75 ~ "High risk",
           minutes_week_moderate < 150 ~ "Moderate risk",
           TRUE ~ "No/mild risk"),
         operationalisation = as.factor(operationalisation),
         operationalisation2 = case_when(
           minutes_week_moderate <= 125 ~ "Moderate/high risk",
           minutes_week_moderate > 125 ~ "No/mild/moderate risk"),
         operationalisation2 = as.factor(operationalisation2)
  ) %>%
  #Return values to character value for sankey plot
  mutate(
    `Exercise duration` = case_when(
      exe_du_nt3blq1 == 45 ~ "30 minutes - 1 hour",
      exe_du_nt3blq1 == 22.5 ~ "15-29 minutes",
      exe_du_nt3blq1 == 7.5 ~ "Less than 15 minutes",
      exe_du_nt3blq1 == 60 ~ "More than 1 hour"),
    `Exercise frequency` = case_when(
      exe_f_nt3blq1 == 0 ~ "Less than once a week",
      exe_f_nt3blq1 == 1 ~ "Once a week",
      exe_f_nt3blq1 == 2.5 ~ "2-3 times a week",
      exe_f_nt3blq1 == 5 ~ "About every day"),
    `Exercise intensity` = case_when(
      exe_int_nt3blq1 == 2.0000001 ~ "I practically exhaust myself",
      exe_int_nt3blq1 == 2 ~ "I push myself until I’m out of breath",
      exe_int_nt3blq1 == 1 ~ "I take it easy")
  ) %>%
  #Factor order
  mutate(`Exercise duration` = factor(`Exercise duration`, levels = c("Less than 15 minutes", "15-29 minutes",
                                                                      "30 minutes - 1 hour", "More than 1 hour")),
         `Exercise frequency` = factor(`Exercise frequency`, levels = c("Less than once a week", "Once a week",
                                                                        "2-3 times a week", "About every day")),
         `Exercise intensity` = factor(`Exercise intensity`, levels = c("I take it easy", "I push myself until I’m out of breath",
                                                                        "I practically exhaust myself"))) %>%
  #Group minutes of activity/week for plots later
  mutate(minutes_groups = case_when(
    
    minutes_week_moderate <= 30 ~ "<30",
    minutes_week_moderate <= 60  ~ "31-60",
    minutes_week_moderate <= 90 ~ "61-90",
    minutes_week_moderate <= 120 ~ "91-120",
    minutes_week_moderate <= 150 ~ "121-150",
    minutes_week_moderate <= 225 ~ "151-225",
    minutes_week_moderate <= 300 ~ "226-300",
    minutes_week_moderate <= 450 ~ "301-450",
    minutes_week_moderate <= 600 ~ "451-600",
    
    #Find out later why two values with 600 min/activity is not picked up, in the meantime hardcode:
    TRUE ~ "451-600"
    
  ),
  minutes_groups = factor(minutes_groups, levels = c("<30","31-60","61-90","91-120",
                                                     "121-150","151-225","226-300",
                                                     "301-450","451-600"))) %>%
  mutate(Subcategory = "Physical activity",
         Category = "Healthy Life Index") %>%
  #Keep numbers of minutes as numeric value
  mutate(numeric = minutes_week_moderate) %>%
  #Only keep necessary columns
  dplyr::select(-starts_with(c("exe", "minutes")))

### Sleep----
operationalisation_scores$operationalisation$Sleep <- tmp$Sleep %>%
  group_by(PID, value, Category, Subcategory) %>%
  #Count number of timepoints where there is insomnia
  summarise(n = n()) %>% ungroup() %>%
  pivot_wider(.,
              names_from = value,
              values_from = n) %>%
  mutate(
    operationalisation = case_when(
      `Flere ganger per uke` == 3 ~ "High risk",
      `Flere ganger per uke` %in% c(1,2) ~ "Moderate risk",
      TRUE ~ "No/mild risk"),
    numeric = case_when(
      
      operationalisation == "High risk" ~ 3,
      operationalisation == "Moderate risk" ~ 2,
      operationalisation == "No/mild risk" ~ 1
      
    ),
    operationalisation = as.factor(operationalisation)) %>%
  distinct()

### Smoking----
operationalisation_scores$operationalisation$Smoking <- tmp$Smoking %>%
  mutate(
    operationalisation = case_when(
      value %in% c("Daglig røyker", "Røyker av og til") ~ "High risk",
      value == "Tidligere røyker" ~ "Moderate risk",
      TRUE ~ "No/mild risk"),
    numeric = case_when(
        operationalisation == "High risk" ~ 3,
        operationalisation == "Moderate risk" ~ 2,
        operationalisation == "No/mild risk" ~ 1
      )
  )

### Social Interaction----
operationalisation_scores$operationalisation$`Social Interaction` <- tmp$`Social Interaction` %>%
  mutate(operationalisation = case_when(
    value %in% c("En god del", "Svært mye") ~ "High risk",
    value == "Litt" ~ "Moderate risk",
    value == "Nei" ~ "No/mild risk"
  )) %>%
  #Create a numeric category as well, higher is better
  mutate(numeric = case_when(
    value == "Nei" ~ 3,
    value == "Litt" ~ 2,
    value == "En god del" ~ 1,
    value == "Svært mye" ~ 0
  )) %>%
  dplyr::select(-c(variable, value))

## Intrinsic capacity----
### Cognition----
# Stratify based on age, sex and education level
# < 1 SD - mild cognitive impairment, <2 SD moderate cognitive impairment according to WHO
# Also used here: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5545909/
operationalisation_scores$operationalisation$Cognition <- tmp$Cognition %>%
  mutate(value = as.numeric(value)) %>%
  left_join(data_for_analysis %>% dplyr::select(PID, Sex, Age_HUNT4, education_years_hunt4 ) %>% mutate(PID = as.character(PID))) %>%
  addAgegroup(., HUNT_survey = "4") %>%
  group_by(Sex, Age_group, education_years_hunt4) %>%
  mutate(z_score = scale(value)) %>%
  mutate(
    operationalisation = case_when(
      z_score < -2 ~ "High impairment",
      z_score < -1 ~ "Moderate impairment",
      TRUE ~ "No/mild impairment"),
    operationalisation = factor(operationalisation),
    operationalisation2 = case_when(
      z_score < -1.5 ~ "Moderate/High impairment",
      TRUE ~ "No/Mild/Moderate impairment")
  ) %>%
  ungroup() %>%
  #Clean up columns and keep raw numeric values
  dplyr::select(PID, Category, Subcategory, value, starts_with("operationalisation")) %>%
  rename(numeric = value)

### Locomotion ----
operationalisation_scores$operationalisation$Locomotion <- tmp$Locomotion %>%
  mutate(value = as.numeric(value)) %>%
  mutate(
    operationalisation = case_when(
      between(value, 0, 6) ~ "High impairment",
      between(value, 7, 9) ~ "Moderate impairment",
      between(value, 10, 12) ~ "No/mild impairment"),
    operationalisation = as.factor(operationalisation),
    operationalisation2 = case_when(
      between(value, 0, 7) ~ "Moderate/High impairment",
      between(value, 8, 12) ~ "No/Mild/Moderate impairment")
  ) %>%
  rename(numeric = value)

### Mental health ----
operationalisation_scores$operationalisation$`Mental health` <- tmp$`Mental health` %>%
  mutate(
    operationalisation = case_when(
      value %in% c("Meget misfornøyd","Svært misfornøyd") ~ "High impairment",
      value %in% c("Både/og", "Nokså misfornøyd") ~ "Moderate impairment",
      TRUE ~ "No/mild impairment"),
    operationalisation = as.factor(operationalisation),
    operationalisation2 = case_when(
      value %in% c("Meget misfornøyd","Svært misfornøyd", "Både/og") ~ "Moderate/High impairment",
      TRUE ~ "No/Mild/Moderate impairment"
    )) %>%
  #Create new numeric value where higher score indicate higher level of functioning
  mutate(
    numeric = case_when(
      value == "Meget fornøyd" ~ 7,
      value == "Svært fornøyd" ~ 6,
      value == "Ganske fornøyd" ~ 5,
      value == "Både/og" ~ 4,
      value == "Nokså misfornøyd" ~ 3,
      value == "Meget misfornøyd" ~ 2,
      value == "Svært misfornøyd" ~ 1
    )
  ) %>%
  dplyr::select(-c(variable, value))

### Sensory capacity ----
operationalisation_scores$operationalisation$`Sensory capacity` <- tmp$`Sensory capacity` %>%
  mutate(
    operationalisation = case_when(
      value == "Mye nedsatt" ~ "High impairment",
      value == "Middels nedsatt" ~ "Moderate impairment",
      TRUE ~ "No/mild impairment"),
  ) %>%
  #Create a new numeric value where higher score = less impairment in line with locomtion and cognition
  mutate(numeric = case_when(
    value == "Mye nedsatt" ~ 1,
    value == "Middels nedsatt" ~ 2,
    TRUE ~ 3
  )) %>% dplyr::select(-c(value)) %>%
  mutate(Subcategory = case_when(
    
    str_detect(variable, "VisImp") ~ "Vision",
    str_detect(variable, "HearImp") ~ "Hearing",
    
  ))

### Vitality ----
operationalisation_scores$operationalisation$Vitality <- tmp$Vitality %>%
  dplyr::select(PID, value, Category, Subcategory) %>%
  #Operationalise, use values from here: https://tidsskriftet.no/2021/03/originalartikkel/skropelighet-blant-eldre-pasienter-med-hjemmesykepleie
  #Need sex of participants for operationalisation
  left_join(., data_for_analysis %>% dplyr::select(PID, Sex) %>% mutate(PID = as.character(PID))) %>%
  mutate(
    value = as.numeric(value),
    operationalisation = case_when(
      Sex == "Women" & value >= 20 ~ "No/mild impairment",
      Sex == "Women" & between(value, 16, 19.9) ~ "Moderate impairment",
      Sex == "Women" & value < 16 ~ "High impairment",
      
      Sex == "Men" & value >= 32 ~ "No/mild impairment",
      Sex == "Men" & between(value, 26, 31.9) ~ "Moderate impairment",
      Sex == "Men" & value < 26 ~ "High impairment"),
    operationalisation2 = case_when(
      Sex == "Women" & value >= 18 ~ "No/Mild/Moderate impairment",
      TRUE ~ "Moderate/High impairment",
      
      Sex == "Men" & value >= 29 ~ "No/Mild/Moderate impairment",
      TRUE ~ "Moderate/High impairment")
  ) %>%
  rename(numeric = value)

# Summary scores ----
operationalisation_scores$operationalisation <- lapply(operationalisation_scores$operationalisation, format_indexes_columns) %>%
    bind_rows() %>%
    split(.$category)

operationalisation_scores$summary_scores <- lapply(operationalisation_scores$operationalisation, function(x) {
  
  x %>%
    mutate(tmp = case_when(
      
      #Don't count alcohol or glasses of milk with high fat
      subcategory %in% c("Alcohol", "Milk low fat", "Milk high fat") ~ 0,
      
      operationalisation %in% c("High risk", "High impairment") ~ 0,
      operationalisation %in% c("Moderate risk", "Moderate impairment") ~ 0.5,
      operationalisation %in% c("No/mild risk", "No/mild impairment") ~ 1,
      
    )) %>%
    group_by(pid) %>%
    summarise(score = sum(tmp)) %>% ungroup()
  
})

##Turn operationalisations to factor
operationalisation_scores$operationalisation$`Healthy Life Index` <- operationalisation_scores$operationalisation$`Healthy Life Index` %>%
  mutate(operationalisation = factor(operationalisation, levels = factor_orders$risk_order))
operationalisation_scores$operationalisation$`Intrinsic capacity` <- operationalisation_scores$operationalisation$`Intrinsic capacity` %>%
  mutate(operationalisation = factor(operationalisation, levels = factor_orders$impairment_order))


# Descriptive statistics ----
addParticipantMetadata <- function(df) {
  
  df %>%
    clean_names() %>%
    left_join(., data_for_analysis %>%
                clean_names() %>%
                mutate(pid = as.numeric(as.character(pid))) %>%
                dplyr::select(contains(c("pid", "age_", "sex", "marital_status", "education", "_ev_"))) %>%
                #Turn categorical values to factor
                mutate(sex = factor(sex, levels = c("Women", "Men")),
                       marital_status = factor(marital_status, levels =c("Married", "Unmarried/Widow/Divorced/Separated")))%>%
                #Count the number of long term or chronic conditions
                pivot_longer(.,
                             cols = contains("_ev_"),
                             names_to = "name_illness",
                             values_to = "has_illness") %>%
                mutate(.by = c(pid, has_illness), number_of_illness = n()) %>%
                dplyr::select(-name_illness) %>% distinct() %>%
                pivot_wider(.,
                            names_from = has_illness,
                            values_from = number_of_illness) %>%
                # The "Ja" column is now the number of illnesses a person has
                replace_na(list(Ja = 0)) %>%
                # Create a factor column for "number of illness" as there are few with four or more illnesses
                mutate(number_of_illness = case_when(
                  Ja >= 3 ~ "3+",
                  TRUE ~ as.character(Ja)
                )) %>% mutate(number_of_illness = factor(number_of_illness, levels = c("0", "1", "2", "3+"), labels = c("None", "One condition", "Two conditions", "Three or more conditions"))) %>%
                #Remove the Ja/Nei columns as they are no longer needed
                dplyr::select(-c(Ja, Nei))
    )
  
}


# Descriptive stats
descriptive_stats <- list(
  "By risk or impairtment operationalisation" = lapply(operationalisation_scores$operationalisation, function(x) {
    
    x %>%
      #Summary statistics
      summarise(.by = c("subcategory", "operationalisation"),
                n_pct = paste0(n(), " (", round((n()/nrow(data_for_analysis))*100, 1), "%)")
      ) %>% pivot_wider(.,
                        names_from = operationalisation,
                        values_from = n_pct) %>%
      dplyr::select(subcategory, contains(c("No/mild", "Moderate", "High"))) %>%
      mutate(across(everything(), ~case_when(
        is.na(.) ~ "0 (0%)",
        TRUE ~ .))
      ) %>%
      # Add missing range
      left_join(., pct_NA_hli_ici)
  }),
  
  "By summary score quartiles" = lapply(operationalisation_scores$summary_scores, function(x) {
    
    df <- x %>%
      mutate(
        #Calculate quantile scores
        Q25 = quantile(score, 0.25),
        Q50 = quantile(score, 0.5),
        Q75 = quantile(score, 0.75),
        #Operationalise the quantile scores
        operationalisation = case_when(
          # score > median(score) ~ "Very high",
          # TRUE ~ "Average/High"
          score < Q25 ~ paste0("First quartile__", Q25),
          score < Q50 ~ paste0("Second quartile__", Q50),
          score < Q75 ~ paste0("Third quartile__", Q75),
          score >= Q75 ~ paste0("Fourth quartile--", Q75))
        )
    
    categorical <- df %>%
      # Calculate counts and percentages descriptive stats
      dplyr::select(pid, operationalisation) %>%
      distinct() %>%
      addParticipantMetadata() %>%
      dplyr::select(-contains("age")) %>%
      pivot_longer(.,
                   cols = -c(pid, operationalisation),
                   names_to = "variable",
                   values_to = "value") %>%
      #Number in each lifestyle_operationalisation and different values
      summarise(.by = c(operationalisation, variable, value), n = n()) %>%
      mutate(.by = c(operationalisation, variable), total_n = sum(n)) %>%
      mutate(operationalisation = as.character(operationalisation),
             operationalisation = paste0(operationalisation, "_", total_n),
             n_pct = paste0(n, " (", round((n/total_n)*100,1), "%)")
      ) %>%
      #Clean up for kable
      dplyr::select(-c(n, total_n)) %>%
      pivot_wider(.,
                  names_from = operationalisation,
                  values_from = n_pct)
    
    age <- df %>%
      mutate(.by = operationalisation, total_n = n()) %>%
      mutate(operationalisation = as.character(operationalisation),
             operationalisation = paste0(operationalisation, "_", total_n)) %>%
      dplyr::select(-total_n) %>%
      #Add age from hunt3
      addParticipantMetadata() %>%
      dplyr::select(pid, operationalisation, age_hunt4, age_hunt3, starts_with("Q")) %>%
      pivot_longer(.,
                   cols = c(age_hunt4, age_hunt3),
                   names_to = "HUNT_survey",
                   values_to = "Age") %>%
      mutate(Age = as.numeric(as.character(Age))) %>%
      summarise(
        .by = c(operationalisation, HUNT_survey),
        median = median(Age),
        Q25 = quantile(Age, 0.25),
        Q75 = quantile (Age, 0.75)) %>% ungroup() %>%
      mutate(median_iqr = paste0(median, " (", Q25, "-", Q75, ")")) %>%
      dplyr::select(-c(median, Q25, Q75)) %>%
      pivot_wider(.,
                  names_from = operationalisation,
                  values_from = median_iqr) %>%
      rename(variable = HUNT_survey) %>% mutate(value = "")
    
    bind_rows(age, categorical) %>%
      dplyr::select(variable, value, starts_with(c("First", "Second", "Third", "Fourth"))) %>%
      mutate(value = case_when(
        value == "" ~ variable,
        TRUE ~ value
      ))
    
  })
)

## Set the factor orders so they are the same across tables and figures
descriptive_stats$`By risk or impairtment operationalisation`$`Healthy Life Index` <- descriptive_stats$`By risk or impairtment operationalisation`$`Healthy Life Index` %>%
  mutate(subcategory = factor(subcategory, levels = factor_orders$`Healthy Life Index`, labels = factor_orders$`Healthy Life Index_labels`))

descriptive_stats$`By risk or impairtment operationalisation`$`Intrinsic capacity` <- descriptive_stats$`By risk or impairtment operationalisation`$`Intrinsic capacity` %>%
  mutate(subcategory = factor(subcategory, levels = factor_orders$`Intrinsic capacity`))

descriptive_stats$`By risk or impairtment operationalisation` <- lapply(descriptive_stats$`By risk or impairtment operationalisation`,
                                                                        function(x) {
                                                                          x %>% arrange(subcategory)
                                                                        })
descriptive_stats$`By summary score quartiles` <- descriptive_stats$`By summary score quartiles` %>%
  lapply(., function(x) {
    
    x %>% mutate(
      value = str_replace_all(value, "age_hunt3|age_hunt4", "Age"),
      variable = factor(variable, levels = factor_orders$participant_meta_variables, labels = factor_orders$participant_meta_variables_labels),
      value = factor(value, levels = factor_orders$participant_meta_values)
    ) %>% arrange(variable, value)
    
  })

#  Save
saveRDS(descriptive_stats, "./RQ1/R_output/descriptive_stats.Rds")

## Format for regression
data_for_regression <- list(
  
  summary_scores = full_join(operationalisation_scores$summary_scores$`Healthy Life Index` %>% rename(HLI = score),
                             operationalisation_scores$summary_scores$`Intrinsic capacity` %>% rename(ICI = score)
                             ) %>%
    #Add metadata
    addParticipantMetadata(),
  
  # Individual risk/impairment categories
  individual_risk_impairment_categories = lapply(operationalisation_scores$operationalisation, function(x) {
    
    x %>%
      #Add metadata
      addParticipantMetadata()
    
  })
  
  
)

saveRDS(data_for_regression, "./RQ1/R_output/data_for_regression.Rds")
 

# Data exploration with imputed data----

## Correlations between intrinsic capacity and healthy behaviors

test <- full_join(operationalisation_scores$summary_scores$`Healthy Life Index` %>% rename(HLI = score),
                  operationalisation_scores$summary_scores$`Intrinsic capacity` %>% rename(ICI = score)) %>%
  addParticipantMetadata() %>%
  #Add illes occurence as well
  full_join(., data_for_analysis %>% dplyr::select(PID, contains("Ev@", ignore.case = FALSE)))
  addAgegroup() %>%
  mutate(
    hli = factor(hli, levels = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8)),
    .by = c(hli, Age_group), n = n())

ggplot(test, aes(y = ici, x = hli)) +
  geom_boxplot() +
  geom_text(data = test %>% dplyr::select(hli, Age_group, n) %>% unique(), aes(label = paste0("n =\n", n), x = hli, y = 0)) +
  facet_wrap(~Age_group)

regression <- lm(ici ~ as.numeric(as.character(hli)) + `age_hunt3...4`, data = test) 
summary(regression)
