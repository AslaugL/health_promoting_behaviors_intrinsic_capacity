# Functions and variables relevant for all three RQs

## Color vectors for plots----
color_vectors <- list()
### Categories used in heatplot for missing values----
color_vectors$categories <- c(
  #Overarching categories
  "Healthy Life Index" = "#ccebc5",
  
  "Intrinsic capacity" = "#d01c8b",
  
  "Frailty" = "#e08214",
  
  #Subcategories
  "Lifetime physical activity" = "#4eb3d3", "Physical activity" = "#4eb3d3", "Diet" = "#ccebc5", 
  "Stimulants" = "#7bccc4", "Alcohol" = "#7bccc4", "Smoking" = "#7bccc4", "Sleep" = "#a8ddb5",
  "Leisure" = "#f7fcf0",
  
  
  "Mental health" = "#feebe2", "Cognition" = "#fbb4b9", "Locomotion" = "#f768a1", "Vitality" = "#7a0177",
  "Sensory capacity" = "#c51b8a", "Vision" = "#c51b8a", "Hearing" = "#c51b8a", "Illnesses" = "#8073ac"
)

### Color for the different imputation datasets----
color_vectors$imputation_dfs <- c(
  "Observed" = "#BBBBBB", "Imputation" = "#4EB265",
  "imputation1" = "#4EB265", "imputation2" = "#F6C141", "imputation3" = "#CAE0AB", "imputation4" = "#F7F056", "imputation5" = "#90C987")

### Color for sankey plots----
color_vectors$sankey <- c(
  
  #At risk or not
  "At risk" = "#EE7733", "High risk" = "#EE7733", "Moderate risk" = "#EE7733", "Not at risk" = "#CCBB44", "No/mild risk" = "#CCBB44",
  
  #Dietary factors
  "Milk" = "#DDDDDD", "Fatty fish" = "#0077BB", "Fruit and vegetables" = "#009988",
  
  #Physical activity
  ##Duration
  "Less than 15 minutes" = "#B7E6A5", "15-29 minutes" = "#46AEA0", "30 minutes - 1 hour" = "#00718B", "More than 1 hour" = "#003147",
  ##Intensity
  "I take it easy" = "#B7E6A5", "I push myself until I’m out of breath" = "#00718B", "I practically exhaust myself" = "#003147", 
  ##Frequency
  "Less than once a week" = "#B7E6A5", "Once a week" = "#46AEA0", "2-3 times a week" = "#00718B", "About every day" = "#003147",
  
  #Stimulants
  "Smoking" = "#BBBBBB", "Alcohol" = "#8073ac",
  
  #Sleep
  "Night" = "#081d58", "Morning" = "#ffffd9", "Evening" = "#225ea8"
  
)

#Age groups
color_vectors$age_groups <- c(
  "65-69" = "#fee8c8", "70-74" = "#fdbb84", "75-79" = "#ef6548", "80-84" = "#b30000", "85+" = "#7f0000")

#Sex
color_vectors$Sex <- c("Kvinne" = "#5aae61", "Women" = "#5aae61", "Mann" = "#9970ab", "Men" = "#9970ab")

#Intrinsic capacity operationalisation
color_vectors$ic_operationalisation <- c("No/mild impairment" = "#fee391", "Moderate impairment" = "#fe9929", "High impairment" = "#cc4c02")


