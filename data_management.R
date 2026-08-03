library(tidyverse)


# Import datasets per license type
datacharter <- read.csv("Data/data_raw_charter.csv")
datageneral <- read.csv("Data/data_raw_general.csv")
datasaltwater <- read.csv("Data/data_raw_saltwater.csv")
datashoreline <- read.csv("Data/data_raw_shoreline.csv")

# Combine datasets
anglerdata <- rbind(datacharter, datageneral, datasaltwater, datashoreline)


######### * DATA CORRECTIONS ######### 

### Convert any blank cells to NAs ###

anglerdata <- anglerdata %>%
  mutate(across(where(is.character), ~na_if(trimws(.), "")))

### Break up multiple selections into discrete answers ###

anglerdata <- anglerdata %>%
  mutate(REASON_FUN = ifelse(is.na(REASONS), NA, ifelse(grepl("sport", REASONS), 1, 0)),
         REASON_FOOD = ifelse(is.na(REASONS), NA, ifelse(grepl("own", REASONS), 1, 0)),
         REASON_SELL = ifelse(is.na(REASONS), NA, ifelse(grepl("sell", REASONS), 1, 0)),
         REASON_CHARTER = ifelse(is.na(REASONS), NA, ifelse(grepl("charter", REASONS), 1, 0)),
         REASON_OTHER = ifelse(is.na(REASONS), NA, ifelse(grepl("else", REASONS), 1, 0)),
         LOCATION_SHORE = ifelse(is.na(LOCATION), NA, ifelse(grepl("shoreline", LOCATION), 1, 0)),
         LOCATION_PIER = ifelse(is.na(LOCATION), NA, ifelse(grepl("pier", LOCATION), 1, 0)),
         LOCATION_VESSEL = ifelse(is.na(LOCATION), NA, ifelse(grepl("boat", LOCATION), 1, 0))) %>%
  mutate(REASONS_N = REASON_FUN + REASON_FOOD + REASON_SELL + REASON_CHARTER + REASON_OTHER,
         LOCATIONS_N = LOCATION_SHORE + LOCATION_PIER + LOCATION_VESSEL)

### Reclassify answers ###

anglerdata <- anglerdata %>%
  mutate(COUNTY = ifelse(is.na(COUNTY), NA, ifelse(COUNTY == "Other county:", "Other", COUNTY)),
         # After inspection, "Other" selections on GENDER do not appear to be legitimate, so nullify
         GENDER = ifelse(is.na(GENDER), NA, ifelse(GENDER == "Other description:", NA, GENDER)),
         PRESSURES_1 = ifelse(is.na(PRESSURES_1), NA, ifelse(PRESSURES_1 == "Yes", 1, 0)),
         PRESSURES_2 = ifelse(is.na(PRESSURES_2), NA, ifelse(PRESSURES_2 == "Yes", 1, 0)),
         PRESSURES_3 = ifelse(is.na(PRESSURES_3), NA, ifelse(PRESSURES_3 == "Yes", 1, 0)),
         PRESSURES_4 = ifelse(is.na(PRESSURES_4), NA, ifelse(PRESSURES_4 == "Yes", 1, 0)),
         PRESSURES_5 = ifelse(is.na(PRESSURES_5), NA, ifelse(PRESSURES_5 == "Yes", 1, 0)),
         PRESSURES_6 = ifelse(is.na(PRESSURES_6), NA, ifelse(PRESSURES_6 == "Yes", 1, 0)),
         PRESSURES_7 = ifelse(is.na(PRESSURES_7), NA, ifelse(PRESSURES_7 == "Yes", 1, 0)),
         PRESSURES_8 = ifelse(is.na(PRESSURES_8), NA, ifelse(PRESSURES_8 == "Yes", 1, 0)),
         AR_RELIEF = ifelse(is.na(AR_RELIEF), NA, 
                            ifelse(AR_RELIEF == "Low\n(1 ft. or less)", "Low",
                                   ifelse(AR_RELIEF == "Moderate\n(2 - 4 ft.)", "Moderate",
                                          ifelse(AR_RELIEF == "High\n(5 ft. or more)", "High", AR_RELIEF)))),
         AR_SIZE = ifelse(is.na(AR_SIZE), NA, 
                            ifelse(AR_SIZE == "Small\n(several square yards)", "Small",
                                   ifelse(AR_SIZE == "Medium\n(less than 1 acre)", "Medium",
                                          ifelse(AR_SIZE == "Large\n(1 acre or more)", "Large", AR_SIZE)))),
         AR_DEPTH = ifelse(is.na(AR_DEPTH), NA, 
                            ifelse(AR_DEPTH == "Shallow\n(10 ft. or less)", "Shallow",
                                   ifelse(AR_DEPTH == "Moderate\n(10 - 20 ft.)", "Moderate",
                                          ifelse(AR_DEPTH == "Deep\n(20 ft. or more)", "Deep", AR_DEPTH)))),
         AR_COMPLEX = ifelse(is.na(AR_COMPLEX), NA, 
                            ifelse(AR_COMPLEX == "Simple\n(mostly flat or smooth)", "Simple",
                                   ifelse(AR_COMPLEX == "Moderate\n(some nooks and crannies)", "Moderate",
                                          ifelse(AR_COMPLEX == "Complex\n(lots of holes, edges, and hiding spots)", "Complex", AR_COMPLEX))))) %>%
  # For all MAP columns, convert "On" to 1, "Off" to 0
  mutate(across(starts_with("MAP_"), ~ as.integer(. == "On"))) %>%
  mutate(PRESSURES_N = PRESSURES_1 + PRESSURES_2 + PRESSURES_3 + PRESSURES_4 + PRESSURES_5 + PRESSURES_6 + PRESSURES_7 + PRESSURES_8,
         SPECIES_OFTEN_N = rowSums(across(starts_with("SPECIES_"), ~ . %in% c("Often", "Always")), na.rm = TRUE),
         ZONES_N = rowSums(pick(starts_with("MAP_")), na.rm = TRUE))
         


######### * DATA CLEANING ######### 

### Exclusions ###

anglerdata <- anglerdata %>%
  # remove responses that did not reach the end of the survey
  filter(Finished != "FALSE") %>%
  # remove responses flagged as fraudulent
  filter(Q_RecaptchaScore >= 0.5) %>%
  # remove potential duplicate responses
  filter(is.na(Q_DuplicateRespondent)) %>%
  # remove responses that spent less than 4 minutes (240 seconds) on the survey
  filter(Duration..in.seconds. >= 240) %>%
  # remove responses that selected an unrealistic number of fishing zones in Tampa Bay
  filter(ZONES_N < 70) %>%
  # identify and remove respondents exhibiting response biases (repeatedly selecting same answer artificial reef questions)
  rowwise() %>%
  mutate(QC_DistinctValues = n_distinct(c_across(CONCRETE_1:TIRES_5), na.rm = TRUE)) %>%
  ungroup() %>%
  filter(QC_DistinctValues != 1)

### Renaming ###

# Rename certain fields for easier interpretation
anglerdata <- anglerdata %>%
  rename(LICENSE = TYPE,
         REASON_OTHER_TEXT = REASONS_5_TEXT,
         GENDER_OTHER_TEXT = GENDER_3_TEXT,
         COUNTY_OTHER_TEXT = COUNTY_7_TEXT,
         PRESSURE_STORMS = PRESSURES_1,
         PRESSURE_HABLOSS = PRESSURES_2,
         PRESSURE_WQUALITY = PRESSURES_3,
         PRESSURE_FISHPOP = PRESSURES_4,
         PRESSURE_ANGLERS = PRESSURES_5,
         PRESSURE_DEVELOP = PRESSURES_6,
         PRESSURE_COSTS = PRESSURES_7,
         PRESSURE_POLICY = PRESSURES_8,
         HABITAT_RIVER = HABITATS_1,
         HABITAT_SEAGRASS = HABITATS_2,
         HABITAT_SHORELINE = HABITATS_3,
         HABITAT_HARDBTM = HABITATS_4,
         HABITAT_ARTREEF = HABITATS_5,
         HABITAT_INFRASTR = HABITATS_6,
         SPECIES_SNOOK = SPECIES_1,
         SPECIES_SEATROUT = SPECIES_2,
         SPECIES_REDDRUM = SPECIES_3,
         SPECIES_TARPON = SPECIES_4,
         SPECIES_SHEEPSHEAD = SPECIES_5,
         SPECIES_SNAPPER = SPECIES_6,
         SPECIES_GROUPER = SPECIES_7,
         SPECIES_OTHER = SPECIES_8,
         SPECIES_OTHER_TEXT = SPECIES_8_TEXT,
         CONCRETE_ATTRACTFISH = CONCRETE_1,
         CONCRETE_WORSEWQ = CONCRETE_2,
         CONCRETE_SEDSTABILIZE = CONCRETE_3,
         CONCRETE_DAMAGEHAB = CONCRETE_4,
         CONCRETE_RESTORETOOL = CONCRETE_5,
         TIRE_ATTRACTFISH = TIRES_1,
         TIRE_WORSEWQ = TIRES_2,
         TIRE_SEDSTABILIZE = TIRES_3,
         TIRE_DAMAGEHAB = TIRES_4,
         TIRE_RESTORETOOL = TIRES_5,
         ACTION_MONITOR = ACTION_1,
         ACTION_STABILIZE = ACTION_2,
         ACTION_COVER = ACTION_3,
         ACTION_REMOVEPART = ACTION_4,
         ACTION_REMOVEALL = ACTION_5,
         AR_MATERIAL_CONCRETE = AR_MATERIAL_1,
         AR_MATERIAL_PREFAB = AR_MATERIAL_2,
         AR_MATERIAL_ROCK = AR_MATERIAL_3,
         AR_MATERIAL_TIRE = AR_MATERIAL_4) %>%
  rename_with(~ sub("^MAP_", "ZONE_", .x), starts_with("MAP_"))


### Reorganizing ###

# Keep and reorder the variables needed
anglerdata_final <- anglerdata %>%
  select(ResponseId, LICENSE, 
         # Demographics
         AGE:COSTS, 
         # Fishing experience
         FREQUENCY, DURATION, SKILL,
         # Fishing purpose
         REASONS_N, REASON_FUN, REASON_FOOD, REASON_SELL, REASON_CHARTER, REASON_OTHER, REASON_OTHER_TEXT,
         # Fishing locations
         LOCATIONS_N, LOCATION_VESSEL, LOCATION_SHORE, LOCATION_PIER,
         ZONES_N, ZONE_1:ZONE_76,
         # Location influences
         PRESSURES_N, PRESSURE_STORMS:PRESSURE_POLICY, 
         # Species targeted
         SPECIES_OFTEN_N, SPECIES_SNOOK:SPECIES_OTHER_TEXT, 
         # Habitats targeted
         HABITAT_SEAGRASS, HABITAT_HARDBTM, HABITAT_ARTREEF, HABITAT_RIVER, HABITAT_SHORELINE, HABITAT_INFRASTR, 
         # Artificial reef use
         AR_RELIEF:AR_COMPLEX, 
         AR_MATERIAL_CONCRETE:AR_MATERIAL_TIRE,
         # Artificial reef beliefs
         CONCRETE_ATTRACTFISH:TIRE_RESTORETOOL, 
         # Artificial reef management actions
         FUNDING, AWARE, ACTION_MONITOR:ACTION_REMOVEALL)

# Save the cleaned dataset
#write.csv(anglerdata_final, file = "Data/data_clean.csv", row.names = FALSE)
