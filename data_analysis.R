library(tidyverse)
library(ggalluvial)

# Load data
final_data <- read.csv("Data/data_clean.csv")

anglers <- final_data[,-1]
rownames(anglers) <- final_data[,1]

tac_data <- read.csv("Data/tac_votes.csv")

tac <- tac_data[,-1]
rownames(tac) <- tac_data[,1]


######### * DATA PREPARATION ######### 

### Set factor levels ###

anglers <- anglers %>%
  mutate(LICENSE = factor(LICENSE, levels = c("Saltwater","General","Shoreline","Charter")),
         AGE = factor(AGE, levels = c("18 - 24","25 - 34","35 - 44","45 - 54","55 - 64","65 or older")),
         GENDER = factor(GENDER, levels = c("Male","Female")),
         COUNTY = factor(COUNTY, levels = c("Hillsborough","Manatee","Pasco","Pinellas","Polk","Sarasota","Other")),
         INCOME = factor(INCOME, levels = c("$25,000 or less","$25,001 - $50,000","$50,001 - $75,000","$75,001 - $100,000","$100,000 or more")),
         FREQUENCY = factor(FREQUENCY, levels = c("A few times","About once a month","About once a week","Many times a week")),
         DURATION = factor(DURATION, levels = c("Less than a year","1 - 4 years","5 - 9 years","10 - 19 years","20 years or more")),
         SKILL = factor(SKILL, levels = c("Very poor","Poor","Fair","Good","Excellent")),
         SPECIES_SNOOK = factor(SPECIES_SNOOK, levels = c("Never","Rarely","Sometimes","Often","Always")),
         SPECIES_SEATROUT = factor(SPECIES_SEATROUT, levels = c("Never","Rarely","Sometimes","Often","Always")),
         SPECIES_REDDRUM = factor(SPECIES_REDDRUM, levels = c("Never","Rarely","Sometimes","Often","Always")),
         SPECIES_TARPON = factor(SPECIES_TARPON, levels = c("Never","Rarely","Sometimes","Often","Always")),
         SPECIES_SHEEPSHEAD = factor(SPECIES_SHEEPSHEAD, levels = c("Never","Rarely","Sometimes","Often","Always")),
         SPECIES_SNAPPER = factor(SPECIES_SNAPPER, levels = c("Never","Rarely","Sometimes","Often","Always")),
         SPECIES_GROUPER = factor(SPECIES_GROUPER, levels = c("Never","Rarely","Sometimes","Often","Always")),
         SPECIES_OTHER = factor(SPECIES_OTHER, levels = c("Never","Rarely","Sometimes","Often","Always")),
         HABITAT_SEAGRASS = factor(HABITAT_SEAGRASS, levels = c("Never","Rarely","Sometimes","Often","Always")),
         HABITAT_HARDBTM = factor(HABITAT_HARDBTM, levels = c("Never","Rarely","Sometimes","Often","Always")),
         HABITAT_ARTREEF = factor(HABITAT_ARTREEF, levels = c("Never","Rarely","Sometimes","Often","Always")),
         HABITAT_RIVER = factor(HABITAT_RIVER, levels = c("Never","Rarely","Sometimes","Often","Always")),
         HABITAT_SHORELINE = factor(HABITAT_SHORELINE, levels = c("Never","Rarely","Sometimes","Often","Always")),
         HABITAT_INFRASTR = factor(HABITAT_INFRASTR, levels = c("Never","Rarely","Sometimes","Often","Always")),
         AR_RELIEF = factor(AR_RELIEF, levels = c("Not sure","Low","Moderate","High")),
         AR_SIZE = factor(AR_SIZE, levels = c("Not sure","Small","Medium","Large")),
         AR_DEPTH = factor(AR_DEPTH, levels = c("Not sure","Shallow","Moderate","Deep")),
         AR_COMPLEX = factor(AR_COMPLEX, levels = c("Not sure","Simple","Moderate","Complex")),
         AR_MATERIAL_CONCRETE = factor(AR_MATERIAL_CONCRETE, levels = c("Not sure","Never","Rarely","Sometimes","Often","Always")),
         AR_MATERIAL_PREFAB = factor(AR_MATERIAL_PREFAB, levels = c("Not sure","Never","Rarely","Sometimes","Often","Always")),
         AR_MATERIAL_ROCK = factor(AR_MATERIAL_ROCK, levels = c("Not sure","Never","Rarely","Sometimes","Often","Always")),
         AR_MATERIAL_TIRE = factor(AR_MATERIAL_TIRE, levels = c("Not sure","Never","Rarely","Sometimes","Often","Always")),
         CONCRETE_ATTRACTFISH = factor(CONCRETE_ATTRACTFISH, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         CONCRETE_WORSEWQ = factor(CONCRETE_WORSEWQ, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         CONCRETE_SEDSTABILIZE = factor(CONCRETE_SEDSTABILIZE, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         CONCRETE_DAMAGEHAB = factor(CONCRETE_DAMAGEHAB, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         CONCRETE_RESTORETOOL = factor(CONCRETE_RESTORETOOL, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         TIRE_ATTRACTFISH = factor(TIRE_ATTRACTFISH, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         TIRE_WORSEWQ = factor(TIRE_WORSEWQ, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         TIRE_SEDSTABILIZE = factor(TIRE_SEDSTABILIZE, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         TIRE_DAMAGEHAB = factor(TIRE_DAMAGEHAB, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         TIRE_RESTORETOOL = factor(TIRE_RESTORETOOL, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         FUNDING = factor(FUNDING, levels = c("Opposed","Neutral","Supportive")),
         AWARE = factor(AWARE, levels = c("No","Yes"))) %>%
  # Add distinguisher between anglers and TAC members
  mutate(Sample = "Anglers")

# Determine which anglers use tire reefs
anglers <- anglers %>%
  mutate(AR_USER = ifelse(HABITAT_ARTREEF == "Never" | HABITAT_ARTREEF == "Not sure", "Non-user",
                          ifelse(is.na(AR_MATERIAL_TIRE) | AR_MATERIAL_TIRE == "Never" | AR_MATERIAL_TIRE == "Not sure", "Other Reef User", "Tire Reef User"))) %>%
  mutate(AR_USER = factor(AR_USER, levels = c("Non-user","Other Reef User","Tire Reef User")))


######### * DEMOGRAPHICS ######### 

AGE <- anglers %>%
  drop_na(AGE) %>%
  group_by(AGE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_AGE <- ggplot(AGE, aes(x = "", y = pct, fill = AGE)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Age group", values = c("#96CEF0","#6CBAE9","#3E9CD5","#0070B3","#004D79","#002439")) +
  coord_polar(theta = "y", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AGE.svg', plot=plot_AGE, width=175, height=65, units = "mm", bg = "transparent")
write.csv(AGE, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AGE.csv', row.names = FALSE)


GENDER <- anglers %>%
  drop_na(GENDER) %>% 
  group_by(GENDER) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_GENDER <- ggplot(GENDER, aes(x = "", y = pct, fill = GENDER)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Gender", values = c("#004F7E","#02806E")) +
  coord_polar(theta = "y", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/GENDER.svg', plot=plot_GENDER, width=175, height=65, units = "mm", bg = "transparent")
write.csv(GENDER, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/GENDER.csv', row.names = FALSE)


COUNTY <- anglers %>%
  drop_na(COUNTY) %>% 
  group_by(COUNTY) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_COUNTY <- ggplot(COUNTY, aes(x = "", y = pct, fill = COUNTY)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "County", values = c("#02806E","#004F7E","#5C524F","#DC9E00","#962C14","#653682","#000000")) +
  coord_polar(theta = "y", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/COUNTY.svg', plot=plot_COUNTY, width=175, height=65, units = "mm", bg = "transparent")
write.csv(COUNTY, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/COUNTY.csv', row.names = FALSE)


LICENSE <- anglers %>%
  drop_na(LICENSE) %>% 
  group_by(LICENSE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_LICENSE <- ggplot(LICENSE, aes(x = "", y = pct, fill = LICENSE)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "License Type", values = c("#004F7E","#5C524F","#02806E","#DC9E00")) +
  coord_polar(theta = "y", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/LICENSE.svg', plot=plot_LICENSE, width=175, height=65, units = "mm", bg = "transparent")
write.csv(LICENSE, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/LICENSE.csv', row.names = FALSE)


######### * -- ZIP CODES ######### 

library(sf)
#library(leaflet)
library(RColorBrewer)
library(patchwork)
library(ggspatial)
library(ggtext)


statemap <- st_read("Data/Florida_boundary.shp") 
countymap <- st_read("Data/Florida_counties.shp") 
#countymap <- st_read("Data/tbep_counties.shp") 
#zipmap <- st_read("Data/tbep_zipcodes.shp") 
zipmap <- st_read("Data/Florida_zipcodes.shp") 
water <- st_read("Data/water.shp") 


statemap <- st_read("Data/Florida_boundary.shp") %>%
  st_transform(crs = 4326)
countymap <- st_read("Data/Florida_counties.shp") %>%
  st_transform(crs = 4326)
#countymap <- st_read("Data/tbep_counties.shp") %>%
#  st_transform(crs = 4326)
#zipmap <- st_read("Data/tbep_zipcodes.shp") %>%
#  st_transform(crs = 4326)
zipmap <- st_read("Data/Florida_zipcodes.shp") %>%
  st_transform(crs = 4326)
water <- st_read("Data/water.shp") %>%
  st_transform(crs = 4326)

zipbound <- zipmap |>
  st_union() |>
  st_make_valid()


# Map sample sizes

zipcodes <- anglers %>%
  select(ZIPCODE) %>%
  # remove any non-numeric values in the ZIPCODE field 
  mutate(ZIPCODE = gsub("[^0-9]", "", ZIPCODE)) %>%
  # remove any numbers after the first 5 digits (extended ZIP codes)
  mutate(ZIPCODE = str_sub(ZIPCODE,1,5)) %>%
  drop_na() %>%
  group_by(ZIPCODE) %>%
  summarize(n = n())

zipmap_merged <- zipmap %>%
  inner_join(zipcodes, by = c("ZIP_CODE" = "ZIPCODE"))

# Get zip code extent
bbox <- st_bbox(water)
# Add padding
xpad <- (bbox["xmax"] - bbox["xmin"]) * 0.20
ypad <- (bbox["ymax"] - bbox["ymin"]) * -0.1

zipcode_n <- ggplot() +
  # Base map (water)
  geom_sf(data = water,
          fill = "gray") +
  # Base map (countymap)
  geom_sf(data = countymap,
          fill = "gray90",
          color = "white",
          size = 0.1) +
  # Base map (statemap)
  geom_sf(data = statemap,
          fill = NA,
          color = "black",
          size = 0.1) +
  # Overlay
  geom_sf(data = zipmap_merged,
          aes(fill = n),
          color = "black",
          alpha = 1) +
  coord_sf(
    xlim = c(bbox["xmin"] - xpad, bbox["xmax"] + xpad),
    ylim = c(bbox["ymin"] - ypad, bbox["ymax"] + ypad),
    expand = FALSE) +
  #scale_fill_stepsn(
  #  colours = brewer.pal(9, "Blues"),
  #  breaks = c(1,3,5,7,9,11,13,15,17),
  #  limits = c(1, 17),
  #  name = "Respondents") +
  scale_fill_gradientn(
    colours = brewer.pal(9, "Blues"),
    limits = c(1, 17),
    name = "Respondents") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),   # removes map border
    axis.text = element_blank(),      # removes lat/lon labels
    axis.ticks = element_blank(),     # removes tick marks
    axis.title = element_blank(),     # removes axis titleslegend.position = "right",
    legend.title = element_markdown(),
    legend.background = element_rect(fill = 'transparent', color = NA),
    legend.box.background = element_rect(fill = 'transparent', color = NA))

ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/ZIPCODES.svg', plot=zipcode_n, width=200, height=100, units = "mm", bg = "transparent")



######### * FISHING EXPERIENCE ######### 

FREQUENCY <- anglers %>%
  drop_na(FREQUENCY) %>% 
  group_by(FREQUENCY) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_FREQUENCY <- ggplot(FREQUENCY, aes(x = "", y = pct, fill = FREQUENCY)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Annual Fishing Frequency", values = c("#96CEF0","#3E9CD5","#004D79","#002439")) +
  coord_polar(theta = "y", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/FREQUENCY.svg', plot=plot_FREQUENCY, width=175, height=65, units = "mm", bg = "transparent")
write.csv(FREQUENCY, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/FREQUENCY.csv', row.names = FALSE)


DURATION <- anglers %>%
  drop_na(DURATION) %>%
  group_by(DURATION) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_DURATION <- ggplot(DURATION, aes(x = "", y = pct, fill = DURATION)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Fishing Experience in Tampa Bay", values = c("#96CEF0","#3E9CD5","#0070B3","#004D79","#002439")) +
  coord_polar(theta = "y", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/DURATION.svg', plot=plot_DURATION, width=175, height=65, units = "mm", bg = "transparent")
write.csv(DURATION, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/DURATION.csv', row.names = FALSE)


SKILL <- anglers %>%
  drop_na(SKILL) %>%
  group_by(SKILL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_SKILL <- ggplot(SKILL, aes(x = "", y = pct, fill = SKILL)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Skill at Fishing\n(self-assessed)", values = c("#96CEF0","#3E9CD5","#0070B3","#004D79","#002439")) +
  coord_polar(theta = "y", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/SKILL.svg', plot=plot_SKILL, width=175, height=65, units = "mm", bg = "transparent")
write.csv(SKILL, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/SKILL.csv', row.names = FALSE)


PRESSURES <- anglers %>%
  select(PRESSURE_STORMS:PRESSURE_POLICY) %>%
  pivot_longer(cols = starts_with("PRESSURE_"),
               names_to = "PRESSURE",
               names_prefix = "PRESSURE_",
               values_to = "SELECTION") %>%
  mutate(PRESSURE = factor(PRESSURE, levels = c("FISHPOP","STORMS","HABLOSS","WQUALITY","ANGLERS","DEVELOP","COSTS","POLICY"))) %>%
  group_by(PRESSURE) %>%
  summarise(Count = sum(SELECTION == 1, na.rm = TRUE), 
            .groups = "drop") %>%
  mutate(pct = Count/485*100)
plot_PRESSURES <- ggplot(PRESSURES, aes(x = PRESSURE, y = pct)) +
  geom_bar(stat="identity", fill = "#5C524F") +
  ylim(0,60) + 
  scale_x_discrete(labels = c(
    "FISHPOP" = "Declines in fish\nsightings or\nlandings",
    "STORMS" = "Impacts from\nhurricanes or\ntropical storms",
    "HABLOSS" = "Habitat loss\nor degradation",
    "WQUALITY" = "Declines in\nwater quality",
    "ANGLERS" = "Increases in\nfishing\npressure",
    "DEVELOP" = "Increases in\ncoastal\ndevelopment",
    "COSTS" = "Increases in the\noverall cost of\nfishing",
    "POLICY" = "Changes in \n regulations or\ncatch limits")) +
  labs(x = "Fishing Pressures", y = "Anglers (%)") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/PRESSURES.svg', plot=plot_PRESSURES, width=170, height=100, units = "mm", bg = "transparent")
write.csv(PRESSURES, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/PRESSURES.csv', row.names = FALSE)


######### * FISHING PRACTICES ######### 

REASONS <- anglers %>%
  select(REASON_FUN:REASON_OTHER) %>%
  pivot_longer(cols = starts_with("REASON_"),
               names_to = "REASON",
               names_prefix = "REASON_",
               values_to = "SELECTION") %>%
  mutate(REASON = factor(REASON, levels = c("FUN","FOOD","SELL","CHARTER","OTHER"))) %>%
  group_by(REASON) %>%
  summarise(Count = sum(SELECTION == 1, na.rm = TRUE), 
            .groups = "drop") %>%
  mutate(pct = Count/485*100)
plot_REASONS <- ggplot(REASONS, aes(x = REASON, y = pct)) +
  geom_bar(stat="identity", fill = "#5C524F") +
  ylim(0,100) + 
  scale_x_discrete(labels = c(
    "FUN" = "Fishing for\nfun or sport",
    "FOOD" = "Fishing for\ntheir own food",
    "SELL" = "Fishing for\nfood to sell",
    "CHARTER" = "Providing chartered\nfishing trips",
    "OTHER" = "Some other\nreason")) +
  labs(x = "Typical Reason(s) for Fishing", y = "Anglers (%)") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/REASONS.svg', plot=plot_REASONS, width=170, height=100, units = "mm", bg = "transparent")
write.csv(REASONS, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/REASONS.csv', row.names = FALSE)

REASONS_N <- anglers %>%
  drop_na(REASONS_N) %>%
  group_by(REASONS_N) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
write.csv(REASONS_N, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/REASONS_N.csv', row.names = FALSE)


SPECIES <- anglers %>%
  select(SPECIES_SNOOK:SPECIES_OTHER) %>%
  pivot_longer(cols = starts_with("SPECIES_"),
               names_to = "SPECIES",
               names_prefix = "SPECIES_",
               values_to = "SELECTION") %>%
  mutate(SPECIES = factor(SPECIES, levels = c("GROUPER","REDDRUM","SEATROUT","SHEEPSHEAD","SNAPPER","SNOOK","TARPON","OTHER"))) %>%
  mutate(SELECTION = factor(SELECTION, levels = c("Always","Often","Sometimes","Rarely","Never"))) %>%
  group_by(SPECIES, SELECTION) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count/sum(Count)*100)
plot_SPECIES <- ggplot(SPECIES, aes(x=SPECIES, y=pct, fill=SELECTION)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Frequency of Targeting", values = c("Never" = "#96CEF0",
                                                          "Rarely" = "#3E9CD5",
                                                          "Sometimes" = "#0070B3",
                                                          "Often" = "#004D79",
                                                          "Always" = "#002439")) +
  xlab("Fish Species") +
  ylab("Anglers (%)") +
  #scale_x_discrete(labels = c(
  #  "SEAGRASS" = "Seagrass bed",
  #  "HARDBTM" = "Natural hard bottom\n(ledge, reef, sponge)",
  #  "ARTREEF" = "Artificial reef\n(concrete, rock, reef ball)",
  #  "SHORELINE" = "Natural shoreline\n(beach, mangrove, marsh)",
  #  "RIVER" = "River or creek",
  #  "INFRASTR" = "Exposed infrastructure\n(bridge, dock, pier)")) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/SPECIES.svg', plot=plot_SPECIES, width=170, height=100, units = "mm", bg = "transparent")
write.csv(SPECIES, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/SPECIES.csv', row.names = FALSE)

SPECIES_OFTEN_N <- anglers %>%
  drop_na(SPECIES_OFTEN_N) %>%
  group_by(SPECIES_OFTEN_N) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_SPECIES_OFTEN_N <- ggplot(SPECIES_OFTEN_N, aes(x = SPECIES_OFTEN_N, y = n)) +
  geom_bar(stat="identity", fill = "#5C524F") +
  ylim(0,150) + 
  labs(x = "Number of Species Often Targeted", y = "Anglers") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/SPECIES_OFTEN_N.svg', plot=plot_SPECIES_OFTEN_N, width=170, height=100, units = "mm", bg = "transparent")
write.csv(SPECIES_OFTEN_N, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/SPECIES_OFTEN_N.csv', row.names = FALSE)


SPECIES_OTHER <- anglers %>%
  select(SPECIES_OTHER_TEXT) %>%
  drop_na(SPECIES_OTHER_TEXT) %>%
  group_by(SPECIES_OTHER_TEXT) %>%
  summarise(n = n()) %>%
  arrange(desc(SPECIES_OTHER_TEXT))
write.csv(SPECIES_OTHER, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/SPECIES_OTHER.csv', row.names = FALSE)



LOCATIONS <- anglers %>%
  select(LOCATION_VESSEL:LOCATION_PIER) %>%
  pivot_longer(cols = starts_with("LOCATION_"),
               names_to = "LOCATION",
               names_prefix = "LOCATION_",
               values_to = "SELECTION") %>%
  mutate(LOCATION = factor(LOCATION, levels = c("VESSEL","SHORE","PIER"))) %>%
  group_by(LOCATION) %>%
  summarise(Count = sum(SELECTION == 1, na.rm = TRUE), 
            .groups = "drop") %>%
  mutate(pct = Count/485*100)
plot_LOCATIONS <- ggplot(LOCATIONS, aes(x = LOCATION, y = pct)) +
  geom_bar(stat="identity", fill = "#5C524F") +
  ylim(0,100) + 
  scale_x_discrete(labels = c(
    "VESSEL" = "Boat, kayak, or canoe",
    "SHORE" = "Shoreline or bank of a water body",
    "PIER" = "Fishing pier or boardwalk")) +
  labs(x = "Typical Location(s) for Fishing", y = "Anglers (%)") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/LOCATIONS.svg', plot=plot_LOCATIONS, width=170, height=100, units = "mm", bg = "transparent")
write.csv(LOCATIONS, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/LOCATIONS.csv', row.names = FALSE)


HABITATS <- anglers %>%
  select(HABITAT_SEAGRASS:HABITAT_INFRASTR) %>%
  pivot_longer(cols = starts_with("HABITAT_"),
               names_to = "HABITAT",
               names_prefix = "HABITAT_",
               values_to = "SELECTION") %>%
  mutate(HABITAT = factor(HABITAT, levels = c("SEAGRASS","HARDBTM","ARTREEF","SHORELINE","RIVER","INFRASTR"))) %>%
  mutate(SELECTION = factor(SELECTION, levels = c("Always","Often","Sometimes","Rarely","Never"))) %>%
  group_by(HABITAT, SELECTION) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count/sum(Count)*100)
plot_HABITATS <- ggplot(HABITATS, aes(x=HABITAT, y=pct, fill=SELECTION)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Frequency of Use", values = c("Never" = "#96CEF0",
                                                   "Rarely" = "#3E9CD5",
                                                   "Sometimes" = "#0070B3",
                                                   "Often" = "#004D79",
                                                   "Always" = "#002439")) +
  xlab("Fishing Habitat") +
  ylab("Anglers (%)") +
  scale_x_discrete(labels = c(
    "SEAGRASS" = "Seagrass bed",
    "HARDBTM" = "Natural hard bottom\n(ledge, reef, sponge)",
    "ARTREEF" = "Artificial reef\n(concrete, rock, reef ball)",
    "SHORELINE" = "Natural shoreline\n(beach, mangrove, marsh)",
    "RIVER" = "River or creek",
    "INFRASTR" = "Exposed infrastructure\n(bridge, dock, pier)")) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/HABITATS.svg', plot=plot_HABITATS, width=170, height=100, units = "mm", bg = "transparent")
write.csv(HABITATS, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/HABITATS.csv', row.names = FALSE)

HABITATS_OFTEN_N <- anglers %>%
  drop_na(HABITATS_OFTEN_N) %>%
  group_by(HABITATS_OFTEN_N) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_HABITATS_OFTEN_N <- ggplot(HABITATS_OFTEN_N, aes(x = HABITATS_OFTEN_N, y = n)) +
  geom_bar(stat="identity", fill = "#5C524F") +
  ylim(0,150) + 
  labs(x = "Number of Habitats Often Used", y = "Anglers") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/HABITATS_OFTEN_N.svg', plot=plot_HABITATS_OFTEN_N, width=170, height=100, units = "mm", bg = "transparent")
write.csv(HABITATS_OFTEN_N, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/HABITATS_OFTEN_N_N.csv', row.names = FALSE)



### Species and Habitat Interactions

habitat_cols <- names(anglers) %>%
  keep(~ str_starts(.x, "HABITAT_"))
species_cols <- names(anglers) %>%
  keep(~ str_starts(.x, "SPECIES_")) %>%
  setdiff("SPECIES_OFTEN_N")

# Reshape to one row per respondent, habitat, and species
habitat_species <- anglers %>%
  select(all_of(habitat_cols), all_of(species_cols)) %>%
  pivot_longer(
    cols = all_of(habitat_cols),
    names_to = "habitat",
    values_to = "habitat_frequency") %>%
  pivot_longer(
    cols = all_of(species_cols),
    names_to = "species",
    values_to = "species_frequency") %>%
  # Keep only frequent habitat use AND frequent targeting
  filter(
    habitat_frequency %in% c("Often", "Always"),
    species_frequency %in% c("Often", "Always")) %>%
  # Remove prefixes 
  mutate(
    habitat = str_remove(habitat, "^HABITAT_"),
    species = str_remove(species, "^SPECIES_"))

habitat_species_counts <- habitat_species %>%
  count(habitat, species, name = "n_respondents") %>%
  filter(species != "OTHER") %>%
  arrange(desc(n_respondents))


plot_alluvial <- ggplot(habitat_species_counts, aes(axis1 = habitat,axis2 = species,y = n_respondents)) +
  geom_alluvium(aes(fill = habitat), alpha = 0.7) +
  geom_stratum(width = 0.25, fill = "grey90", color = "grey40") +
  geom_text(stat = "stratum",aes(label = after_stat(stratum)),size = 3) +
  scale_x_discrete(limits = c("Habitat", "Fish species"),expand = c(0.05, 0.05)) +
  scale_fill_manual(
    values = c(
      "ARTREEF" = "#653682",
      "HARDBTM"  = "#DC9E00",
      "INFRASTR"  = "#5C524F",
      "RIVER"  = "#004F7E",
      "SEAGRASS" = "#02806E",
      "SHORELINE"     = "#962C14")) +
  labs(
    x = NULL,
    fill = "Habitat") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/HABITATxSPECIES.svg', plot=plot_alluvial, width=170, height=170, units = "mm", bg = "transparent")


habitat_percentages <- habitat_species_counts %>%
  group_by(habitat) %>%
  mutate(
    percentage = 100 * n_respondents / sum(n_respondents)
  ) %>%
  ungroup() %>%
  arrange(habitat, desc(percentage))
habitat_percentages_wide <- habitat_percentages %>%
  select(habitat, species, percentage) %>%
  pivot_wider(
    names_from = species,
    values_from = percentage,
    values_fill = 0
  )

species_percentages <- habitat_species_counts %>%
  group_by(species) %>%
  mutate(
    percentage = 100 * n_respondents / sum(n_respondents)
  ) %>%
  ungroup() %>%
  arrange(species, desc(percentage))
species_percentages_wide <- species_percentages %>%
  select(species, habitat, percentage) %>%
  pivot_wider(
    names_from = habitat,
    values_from = percentage,
    values_fill = 0
  )

write.csv(habitat_percentages_wide, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/HABITATxSPECIES.csv', row.names = FALSE)



######### * ARTIFICIAL REEF USE ######### 

REEFS <- anglers %>%
  select(AR_MATERIAL_CONCRETE:AR_MATERIAL_TIRE) %>%
  filter(!is.na(AR_MATERIAL_CONCRETE)) %>%
  pivot_longer(cols = starts_with("AR_MATERIAL_"),
               names_to = "AR_MATERIAL",
               names_prefix = "AR_MATERIAL_",
               values_to = "SELECTION") %>%
  mutate(AR_MATERIAL = factor(AR_MATERIAL, levels = c("ROCK","CONCRETE","PREFAB","TIRE"))) %>%
  mutate(SELECTION = factor(SELECTION, levels = c("Always","Often","Sometimes","Rarely","Never","Not sure"))) %>%
  group_by(AR_MATERIAL, SELECTION) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count/sum(Count)*100)
plot_REEFS <- ggplot(REEFS, aes(x=AR_MATERIAL, y=pct, fill=SELECTION)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Frequency of Use", values = c("Not sure" = "#AFAFAF",
                                                          "Never" = "#96CEF0",
                                                          "Rarely" = "#3E9CD5",
                                                          "Sometimes" = "#0070B3",
                                                          "Often" = "#004D79",
                                                          "Always" = "#002439")) +
  xlab("Artificial Reef Material") +
  ylab("Anglers (%)") +
  scale_x_discrete(labels = c(
    "ROCK" = "Rocks",
    "CONCRETE" = "Concrete",
    "PREFAB" = "Prefabricated\n modules",
    "TIRE" = "Tires")) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/REEFS.svg', plot=plot_REEFS, width=170, height=100, units = "mm", bg = "transparent")
write.csv(REEFS, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/REEFS.csv', row.names = FALSE)


AR_USER <- anglers %>%
  drop_na(AR_USER) %>% 
  group_by(AR_USER) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_AR_USER <- ggplot(AR_USER, aes(x = "", y = pct, fill = AR_USER)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Artificial Reef Use", values = c("#5C524F","#004F7E","#02806E")) +
  coord_polar(theta = "y", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_USER.svg', plot=plot_AR_USER, width=175, height=65, units = "mm", bg = "transparent")
write.csv(AR_USER, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_USER.csv', row.names = FALSE)


### Artificial Reef Habitat Profiles

AR_RELIEF <- anglers %>%
  filter(!is.na(AR_RELIEF)) %>%
  group_by(AR_RELIEF) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count/sum(Count)*100)
plot_AR_RELIEF <- ggplot(AR_RELIEF, aes(x=AR_RELIEF, y=pct)) +
  geom_bar(stat="identity", fill = "#5C524F") +
  xlab("Reef Height") +
  ylab("Artificial Reef Users (%)") +
  ylim(0, 50) +
  scale_x_discrete(labels = c(
    "Not sure" = "Not sure",
    "Low" = "Low\n(1 ft. or less)",
    "Moderate" = "Moderate\n(2 - 4 ft.)",
    "High" = "High\n(5 ft. or more)")) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_RELIEF.svg', plot=plot_AR_RELIEF, width=170, height=100, units = "mm", bg = "transparent")
write.csv(AR_RELIEF, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_RELIEF.csv', row.names = FALSE)

AR_SIZE <- anglers %>%
  filter(!is.na(AR_SIZE)) %>%
  group_by(AR_SIZE) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count/sum(Count)*100)
plot_AR_SIZE <- ggplot(AR_SIZE, aes(x=AR_SIZE, y=pct)) +
  geom_bar(stat="identity", fill = "#5C524F") +
  xlab("Reef Size") +
  ylab("Artificial Reef Users (%)") +
  ylim(0, 50) +
  scale_x_discrete(labels = c(
    "Not sure" = "Not sure",
    "Small" = "Small\n(several square yards)",
    "Medium" = "Medium\n(less than 1 acre)",
    "Large" = "Large\n(1 acre or more)")) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_SIZE.svg', plot=plot_AR_SIZE, width=170, height=100, units = "mm", bg = "transparent")
write.csv(AR_SIZE, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_SIZE.csv', row.names = FALSE)

AR_DEPTH <- anglers %>%
  filter(!is.na(AR_DEPTH)) %>%
  group_by(AR_DEPTH) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count/sum(Count)*100)
plot_AR_DEPTH <- ggplot(AR_DEPTH, aes(x=AR_DEPTH, y=pct)) +
  geom_bar(stat="identity", fill = "#5C524F") +
  xlab("Reef Depth") +
  ylab("Artificial Reef Users (%)") +
  ylim(0, 50) +
  scale_x_discrete(labels = c(
    "Not sure" = "Not sure",
    "Shallow" = "Shallow\n(10 ft. or less)",
    "Moderate" = "Moderate\n(10 - 20 ft.)",
    "Deep" = "Deep\n(20 ft. or more)")) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_DEPTH.svg', plot=plot_AR_DEPTH, width=170, height=100, units = "mm", bg = "transparent")
write.csv(AR_DEPTH, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_DEPTH.csv', row.names = FALSE)

AR_COMPLEX <- anglers %>%
  filter(!is.na(AR_COMPLEX)) %>%
  group_by(AR_COMPLEX) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count/sum(Count)*100)
plot_AR_COMPLEX <- ggplot(AR_COMPLEX, aes(x=AR_COMPLEX, y=pct)) +
  geom_bar(stat="identity", fill = "#5C524F") +
  xlab("Reef Complexity") +
  ylab("Artificial Reef Users (%)") +
  ylim(0, 50) +
  scale_x_discrete(labels = c(
    "Not sure" = "Not sure",
    "Simple" = "Simple\n(mostly flat or smooth)",
    "Moderate" = "Moderate\n(some nooks and crannies)",
    "Complex" = "Complex\n(lots of holes, edges,\nand hiding spots)")) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_COMPLEX.svg', plot=plot_AR_COMPLEX, width=170, height=100, units = "mm", bg = "transparent")
write.csv(AR_COMPLEX, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_COMPLEX.csv', row.names = FALSE)


# Create combined profile based on answers
anglers <- anglers %>%
  mutate(AR_PROFILE = str_c(AR_RELIEF, AR_SIZE, AR_DEPTH, AR_COMPLEX, sep = ";"))

AR_PROFILES <- anglers %>%
  drop_na(AR_PROFILE) %>% 
  group_by(AR_PROFILE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

write.csv(AR_PROFILES, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_PROFILES.csv', row.names = FALSE)


### Interactions with targeted species

GROUPER <- anglers %>%
  filter(SPECIES_GROUPER == "Often" | SPECIES_GROUPER == "Always") %>%
  filter(AR_USER != "Non-user") %>%
  # exclude those who are unsure of reef characteristics
  filter(!if_any(c(AR_RELIEF:AR_COMPLEX), ~ .x == "Not sure")) %>%
  # reclass fields so they're all the same
  mutate(AR_SIZE = ifelse(AR_SIZE == "Small", "Low",
                          ifelse(AR_SIZE == "Medium", "Moderate", "High"))) %>%
  mutate(AR_DEPTH = ifelse(AR_DEPTH == "Shallow", "Low",
                          ifelse(AR_DEPTH == "Moderate", "Moderate", "High"))) %>%
  mutate(AR_COMPLEX = ifelse(AR_COMPLEX == "Simple", "Low",
                          ifelse(AR_COMPLEX == "Moderate", "Moderate", "High"))) %>%
  select(AR_RELIEF:AR_COMPLEX) %>%
  pivot_longer(cols = starts_with("AR_"),
               names_to = "ASPECT",
               names_prefix = "AR_",
               values_to = "LEVEL") %>%
  group_by(ASPECT, LEVEL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ASPECT = factor(ASPECT, levels = c("RELIEF","SIZE","DEPTH","COMPLEX")),
         LEVEL = factor(LEVEL, levels = c("High","Moderate","Low"))) %>%
  mutate(SPECIES = "GROUPER") %>%
  relocate(SPECIES, .before = ASPECT)
plot_GROUPER <- ggplot(GROUPER, aes(x=ASPECT, y=pct, fill=LEVEL)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Level", values = c("Low" = "#96CEF0",
                                               "Moderate" = "#3E9CD5",
                                               "High" = "#004D79")) +
  xlab("") +
  ylab("Anglers (%)") +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_GROUPER.svg', plot=plot_GROUPER, width=120, height=100, units = "mm", bg = "transparent")

REDDRUM <- anglers %>%
  filter(SPECIES_REDDRUM == "Often" | SPECIES_REDDRUM == "Always") %>%
  filter(AR_USER != "Non-user") %>%
  # exclude those who are unsure of reef characteristics
  filter(!if_any(c(AR_RELIEF:AR_COMPLEX), ~ .x == "Not sure")) %>%
  # reclass fields so they're all the same
  mutate(AR_SIZE = ifelse(AR_SIZE == "Small", "Low",
                          ifelse(AR_SIZE == "Medium", "Moderate", "High"))) %>%
  mutate(AR_DEPTH = ifelse(AR_DEPTH == "Shallow", "Low",
                           ifelse(AR_DEPTH == "Moderate", "Moderate", "High"))) %>%
  mutate(AR_COMPLEX = ifelse(AR_COMPLEX == "Simple", "Low",
                             ifelse(AR_COMPLEX == "Moderate", "Moderate", "High"))) %>%
  select(AR_RELIEF:AR_COMPLEX) %>%
  pivot_longer(cols = starts_with("AR_"),
               names_to = "ASPECT",
               names_prefix = "AR_",
               values_to = "LEVEL") %>%
  group_by(ASPECT, LEVEL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ASPECT = factor(ASPECT, levels = c("RELIEF","SIZE","DEPTH","COMPLEX")),
         LEVEL = factor(LEVEL, levels = c("High","Moderate","Low"))) %>%
  mutate(SPECIES = "REDDRUM") %>%
  relocate(SPECIES, .before = ASPECT)
plot_REDDRUM <- ggplot(REDDRUM, aes(x=ASPECT, y=pct, fill=LEVEL)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Level", values = c("Low" = "#96CEF0",
                                               "Moderate" = "#3E9CD5",
                                               "High" = "#004D79")) +
  xlab("") +
  ylab("Anglers (%)") +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_REDDRUM.svg', plot=plot_REDDRUM, width=120, height=100, units = "mm", bg = "transparent")

SEATROUT <- anglers %>%
  filter(SPECIES_SEATROUT == "Often" | SPECIES_SEATROUT == "Always") %>%
  filter(AR_USER != "Non-user") %>%
  # exclude those who are unsure of reef characteristics
  filter(!if_any(c(AR_RELIEF:AR_COMPLEX), ~ .x == "Not sure")) %>%
  # reclass fields so they're all the same
  mutate(AR_SIZE = ifelse(AR_SIZE == "Small", "Low",
                          ifelse(AR_SIZE == "Medium", "Moderate", "High"))) %>%
  mutate(AR_DEPTH = ifelse(AR_DEPTH == "Shallow", "Low",
                           ifelse(AR_DEPTH == "Moderate", "Moderate", "High"))) %>%
  mutate(AR_COMPLEX = ifelse(AR_COMPLEX == "Simple", "Low",
                             ifelse(AR_COMPLEX == "Moderate", "Moderate", "High"))) %>%
  select(AR_RELIEF:AR_COMPLEX) %>%
  pivot_longer(cols = starts_with("AR_"),
               names_to = "ASPECT",
               names_prefix = "AR_",
               values_to = "LEVEL") %>%
  group_by(ASPECT, LEVEL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ASPECT = factor(ASPECT, levels = c("RELIEF","SIZE","DEPTH","COMPLEX")),
         LEVEL = factor(LEVEL, levels = c("High","Moderate","Low"))) %>%
  mutate(SPECIES = "SEATROUT") %>%
  relocate(SPECIES, .before = ASPECT)
plot_SEATROUT <- ggplot(SEATROUT, aes(x=ASPECT, y=pct, fill=LEVEL)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Level", values = c("Low" = "#96CEF0",
                                               "Moderate" = "#3E9CD5",
                                               "High" = "#004D79")) +
  xlab("") +
  ylab("Anglers (%)") +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_SEATROUT.svg', plot=plot_SEATROUT, width=120, height=100, units = "mm", bg = "transparent")

SHEEPSHEAD <- anglers %>%
  filter(SPECIES_SHEEPSHEAD == "Often" | SPECIES_SHEEPSHEAD == "Always") %>%
  filter(AR_USER != "Non-user") %>%
  # exclude those who are unsure of reef characteristics
  filter(!if_any(c(AR_RELIEF:AR_COMPLEX), ~ .x == "Not sure")) %>%
  # reclass fields so they're all the same
  mutate(AR_SIZE = ifelse(AR_SIZE == "Small", "Low",
                          ifelse(AR_SIZE == "Medium", "Moderate", "High"))) %>%
  mutate(AR_DEPTH = ifelse(AR_DEPTH == "Shallow", "Low",
                           ifelse(AR_DEPTH == "Moderate", "Moderate", "High"))) %>%
  mutate(AR_COMPLEX = ifelse(AR_COMPLEX == "Simple", "Low",
                             ifelse(AR_COMPLEX == "Moderate", "Moderate", "High"))) %>%
  select(AR_RELIEF:AR_COMPLEX) %>%
  pivot_longer(cols = starts_with("AR_"),
               names_to = "ASPECT",
               names_prefix = "AR_",
               values_to = "LEVEL") %>%
  group_by(ASPECT, LEVEL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ASPECT = factor(ASPECT, levels = c("RELIEF","SIZE","DEPTH","COMPLEX")),
         LEVEL = factor(LEVEL, levels = c("High","Moderate","Low"))) %>%
  mutate(SPECIES = "SHEEPSHEAD") %>%
  relocate(SPECIES, .before = ASPECT)
plot_SHEEPSHEAD <- ggplot(SHEEPSHEAD, aes(x=ASPECT, y=pct, fill=LEVEL)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Level", values = c("Low" = "#96CEF0",
                                               "Moderate" = "#3E9CD5",
                                               "High" = "#004D79")) +
  xlab("") +
  ylab("Anglers (%)") +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_SHEEPSHEAD.svg', plot=plot_SHEEPSHEAD, width=120, height=100, units = "mm", bg = "transparent")

SNAPPER <- anglers %>%
  filter(SPECIES_SNAPPER == "Often" | SPECIES_SNAPPER == "Always") %>%
  filter(AR_USER != "Non-user") %>%
  # exclude those who are unsure of reef characteristics
  filter(!if_any(c(AR_RELIEF:AR_COMPLEX), ~ .x == "Not sure")) %>%
  # reclass fields so they're all the same
  mutate(AR_SIZE = ifelse(AR_SIZE == "Small", "Low",
                          ifelse(AR_SIZE == "Medium", "Moderate", "High"))) %>%
  mutate(AR_DEPTH = ifelse(AR_DEPTH == "Shallow", "Low",
                           ifelse(AR_DEPTH == "Moderate", "Moderate", "High"))) %>%
  mutate(AR_COMPLEX = ifelse(AR_COMPLEX == "Simple", "Low",
                             ifelse(AR_COMPLEX == "Moderate", "Moderate", "High"))) %>%
  select(AR_RELIEF:AR_COMPLEX) %>%
  pivot_longer(cols = starts_with("AR_"),
               names_to = "ASPECT",
               names_prefix = "AR_",
               values_to = "LEVEL") %>%
  group_by(ASPECT, LEVEL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ASPECT = factor(ASPECT, levels = c("RELIEF","SIZE","DEPTH","COMPLEX")),
         LEVEL = factor(LEVEL, levels = c("High","Moderate","Low"))) %>%
  mutate(SPECIES = "SNAPPER") %>%
  relocate(SPECIES, .before = ASPECT)
plot_SNAPPER <- ggplot(SNAPPER, aes(x=ASPECT, y=pct, fill=LEVEL)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Level", values = c("Low" = "#96CEF0",
                                               "Moderate" = "#3E9CD5",
                                               "High" = "#004D79")) +
  xlab("") +
  ylab("Anglers (%)") +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_SNAPPER.svg', plot=plot_SNAPPER, width=120, height=100, units = "mm", bg = "transparent")

SNOOK <- anglers %>%
  filter(SPECIES_SNOOK == "Often" | SPECIES_SNOOK == "Always") %>%
  filter(AR_USER != "Non-user") %>%
  # exclude those who are unsure of reef characteristics
  filter(!if_any(c(AR_RELIEF:AR_COMPLEX), ~ .x == "Not sure")) %>%
  # reclass fields so they're all the same
  mutate(AR_SIZE = ifelse(AR_SIZE == "Small", "Low",
                          ifelse(AR_SIZE == "Medium", "Moderate", "High"))) %>%
  mutate(AR_DEPTH = ifelse(AR_DEPTH == "Shallow", "Low",
                           ifelse(AR_DEPTH == "Moderate", "Moderate", "High"))) %>%
  mutate(AR_COMPLEX = ifelse(AR_COMPLEX == "Simple", "Low",
                             ifelse(AR_COMPLEX == "Moderate", "Moderate", "High"))) %>%
  select(AR_RELIEF:AR_COMPLEX) %>%
  pivot_longer(cols = starts_with("AR_"),
               names_to = "ASPECT",
               names_prefix = "AR_",
               values_to = "LEVEL") %>%
  group_by(ASPECT, LEVEL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ASPECT = factor(ASPECT, levels = c("RELIEF","SIZE","DEPTH","COMPLEX")),
         LEVEL = factor(LEVEL, levels = c("High","Moderate","Low"))) %>%
  mutate(SPECIES = "SNOOK") %>%
  relocate(SPECIES, .before = ASPECT)
plot_SNOOK <- ggplot(SNOOK, aes(x=ASPECT, y=pct, fill=LEVEL)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Level", values = c("Low" = "#96CEF0",
                                               "Moderate" = "#3E9CD5",
                                               "High" = "#004D79")) +
  xlab("") +
  ylab("Anglers (%)") +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_SNOOK.svg', plot=plot_SNOOK, width=120, height=100, units = "mm", bg = "transparent")

TARPON <- anglers %>%
  filter(SPECIES_TARPON == "Often" | SPECIES_TARPON == "Always") %>%
  filter(AR_USER != "Non-user") %>%
  # exclude those who are unsure of reef characteristics
  filter(!if_any(c(AR_RELIEF:AR_COMPLEX), ~ .x == "Not sure")) %>%
  # reclass fields so they're all the same
  mutate(AR_SIZE = ifelse(AR_SIZE == "Small", "Low",
                          ifelse(AR_SIZE == "Medium", "Moderate", "High"))) %>%
  mutate(AR_DEPTH = ifelse(AR_DEPTH == "Shallow", "Low",
                           ifelse(AR_DEPTH == "Moderate", "Moderate", "High"))) %>%
  mutate(AR_COMPLEX = ifelse(AR_COMPLEX == "Simple", "Low",
                             ifelse(AR_COMPLEX == "Moderate", "Moderate", "High"))) %>%
  select(AR_RELIEF:AR_COMPLEX) %>%
  pivot_longer(cols = starts_with("AR_"),
               names_to = "ASPECT",
               names_prefix = "AR_",
               values_to = "LEVEL") %>%
  group_by(ASPECT, LEVEL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ASPECT = factor(ASPECT, levels = c("RELIEF","SIZE","DEPTH","COMPLEX")),
         LEVEL = factor(LEVEL, levels = c("High","Moderate","Low"))) %>%
  mutate(SPECIES = "TARPON") %>%
  relocate(SPECIES, .before = ASPECT)
plot_TARPON <- ggplot(TARPON, aes(x=ASPECT, y=pct, fill=LEVEL)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Level", values = c("Low" = "#96CEF0",
                                               "Moderate" = "#3E9CD5",
                                               "High" = "#004D79")) +
  xlab("") +
  ylab("Anglers (%)") +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_TARPON.svg', plot=plot_TARPON, width=120, height=100, units = "mm", bg = "transparent")

species_merged <- bind_rows(GROUPER,REDDRUM,SEATROUT,SHEEPSHEAD,SNAPPER,SNOOK,TARPON)
write.csv(species_merged, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AR_SPECIES.csv', row.names = FALSE)




######### * ARTIFICIAL REEF MANAGEMENT ######### 

AWARE <- anglers %>%
  drop_na(AWARE) %>% 
  group_by(AWARE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_AWARE <- ggplot(AWARE, aes(x = "", y = pct, fill = AWARE)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Awareness of Planned Tire Reef\nRemoval by Pinellas County", values = c("#004F7E","#02806E")) +
  coord_polar(theta = "y", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AWARE.svg', plot=plot_AWARE, width=175, height=65, units = "mm", bg = "transparent")
write.csv(AWARE, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/AWARE.csv', row.names = FALSE)

FUNDING <- anglers %>%
  drop_na(FUNDING) %>% 
  group_by(FUNDING) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_FUNDING <- ggplot(FUNDING, aes(x = "", y = pct, fill = FUNDING)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Using Public Funds for\nArtificial Reef Management", values = c("#962C14","#DC9E00","#004F7E")) +
  coord_polar(theta = "y", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/FUNDING.svg', plot=plot_FUNDING, width=175, height=65, units = "mm", bg = "transparent")
write.csv(FUNDING, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/FUNDING.csv', row.names = FALSE)




######### * PERCEPTIONS OF TIRE REEFS ######### 

PERCEPTIONS_TIRES <- anglers %>%
  select(TIRE_ATTRACTFISH:TIRE_RESTORETOOL) %>%
  pivot_longer(cols = starts_with("TIRE_"),
               names_to = "ASPECT",
               names_prefix = "TIRE_",
               values_to = "AGREEMENT") %>%
  group_by(ASPECT, AGREEMENT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ASPECT = factor(ASPECT, levels = c("WORSEWQ","DAMAGEHAB","ATTRACTFISH","SEDSTABILIZE","RESTORETOOL")),
         AGREEMENT = factor(AGREEMENT, levels = c("Strongly agree","Agree","Somewhat agree","Somewhat disagree","Disagree","Strongly disagree")))

plot_PERCEPTIONS_TIRES <- ggplot(PERCEPTIONS_TIRES, aes(x=ASPECT, y=pct, fill=AGREEMENT)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Agreement", values = c("Strongly disagree" = "#983E12",
                                                   "Disagree" = "#D7632B",
                                                   "Somewhat disagree" = "#FFA67C",
                                                   "Somewhat agree" = "#7CACFF",
                                                   "Agree" = "#466EB4",
                                                   "Strongly agree" = "#244276")) +
  xlab("Perception of Tire Reefs") +
  ylab("Anglers (%)") +
  scale_x_discrete(labels = c(
    "RESTORETOOL" = "Are useful for\nrestoring habitats\n(Positive)",
    "SEDSTABILIZE" = "Stabilize sediments,\nprevent erosion\n(Positive)",
    "ATTRACTFISH" = "Attract a lot of\ndesirable fish\n(Positive)",
    "DAMAGEHAB" = "Damage natural\nhabitats\n(Negative)",
    "WORSEWQ" = "Worsen water\nquality\n(Negative)")) +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/PERCEPTIONS_TIRES.svg', plot=plot_PERCEPTIONS_TIRES, width=175, height=75, units = "mm", bg = "transparent")
write.csv(PERCEPTIONS_TIRES, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/PERCEPTIONS_TIRES.csv', row.names = FALSE)

PERCEPTIONS_CONCRETE <- anglers %>%
  select(CONCRETE_ATTRACTFISH:CONCRETE_RESTORETOOL) %>%
  pivot_longer(cols = starts_with("CONCRETE_"),
               names_to = "ASPECT",
               names_prefix = "CONCRETE_",
               values_to = "AGREEMENT") %>%
  group_by(ASPECT, AGREEMENT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ASPECT = factor(ASPECT, levels = c("WORSEWQ","DAMAGEHAB","ATTRACTFISH","SEDSTABILIZE","RESTORETOOL")),
         AGREEMENT = factor(AGREEMENT, levels = c("Strongly agree","Agree","Somewhat agree","Somewhat disagree","Disagree","Strongly disagree")))

plot_PERCEPTIONS_CONCRETE <- ggplot(PERCEPTIONS_CONCRETE, aes(x=ASPECT, y=pct, fill=AGREEMENT)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(name = "Agreement", values = c("Strongly disagree" = "#983E12",
                                                   "Disagree" = "#D7632B",
                                                   "Somewhat disagree" = "#FFA67C",
                                                   "Somewhat agree" = "#7CACFF",
                                                   "Agree" = "#466EB4",
                                                   "Strongly agree" = "#244276")) +
  xlab("Perception of Concrete Reefs") +
  ylab("Anglers (%)") +
  scale_x_discrete(labels = c(
    "RESTORETOOL" = "Are useful for\nrestoring habitats\n(Positive)",
    "SEDSTABILIZE" = "Stabilize sediments,\nprevent erosion\n(Positive)",
    "ATTRACTFISH" = "Attract a lot of\ndesirable fish\n(Positive)",
    "DAMAGEHAB" = "Damage natural\nhabitats\n(Negative)",
    "WORSEWQ" = "Worsen water\nquality\n(Negative)")) +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/PERCEPTIONS_CONCRETE.svg', plot=plot_PERCEPTIONS_CONCRETE, width=175, height=75, units = "mm", bg = "transparent")
write.csv(PERCEPTIONS_CONCRETE, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/PERCEPTIONS_CONCRETE.csv', row.names = FALSE)


######### * TIRE REEF MANAGEMENT ACTIONS ######### 

# Reshape
anglers_long <- anglers %>%
  pivot_longer(cols = starts_with("ACTION_"),
               names_to = "ACTION",
               names_prefix = "ACTION_",
               values_to = "SUPPORT") %>%
  mutate(ACTION = factor(ACTION, levels = c("MONITOR","STABILIZE","COVER","REMOVEPART","REMOVEALL")))

action_means_all <- anglers_long %>%
  group_by(ACTION) %>%
  summarise(
    mean = mean(SUPPORT, na.rm = TRUE),
    sd = sd(SUPPORT, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(USER = "Anglers")
action_means_users <- anglers_long %>%
  group_by(AR_USER, ACTION) %>%
  summarise(
    mean = mean(SUPPORT, na.rm = TRUE),
    sd = sd(SUPPORT, na.rm = TRUE),
    .groups = "drop") %>%
  rename(USER = AR_USER) %>%
  relocate(USER, .after = last_col())


tac_long <- tac %>%
  pivot_longer(cols = starts_with("ACTION_"),
               names_to = "ACTION",
               names_prefix = "ACTION_",
               values_to = "SUPPORT") %>%
  mutate(ACTION = factor(ACTION, levels = c("MONITOR","STABILIZE","COVER","REMOVEPART","REMOVEALL")))

action_means_tac <- tac_long %>%
  group_by(ACTION) %>%
  summarise(
    mean = mean(SUPPORT, na.rm = TRUE),
    sd = sd(SUPPORT, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(USER = "TAC")


merged_long <- bind_rows(anglers_long %>% select(Sample, ACTION, SUPPORT), tac_long %>% select(Sample, ACTION, SUPPORT))

action_means_merged <- bind_rows(action_means_all, action_means_users, action_means_tac)

write.csv(action_means_merged, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/ACTION_MEANS.csv', row.names = FALSE)


# Boxplots (All)
ggplot(anglers_long, aes(x = ACTION, y = SUPPORT)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4, ymax = 6),inherit.aes = FALSE,fill = "grey90",alpha = 0.1) +
  geom_boxplot(width = 0.7, outlier.shape = NA, fill = "#5C524F", color = "#5C524F", alpha = 0.6) +  # hide outliers for cleaner look
  geom_hline(yintercept = 5, linetype = "solid", color = "black") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_discrete(labels = c(
    "MONITOR" = "No action,\njust monitor",
    "STABILIZE" = "Stabilize\nperimeter",
    "COVER" = "Cover with\nrock or concrete",
    "REMOVEPART" = "Remove some\nof the tires",
    "REMOVEALL" = "Remove all\nof the tires")) +
  labs(x = "Proposed Management Action", y = "Level of Support") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
# Boxplots (TAC)
ggplot(tac_long, aes(x = ACTION, y = SUPPORT)) +
  #geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 6.5),inherit.aes = FALSE,fill = "grey90",alpha = 0.1) +
  geom_boxplot(width = 0.7, outlier.shape = NA, fill = "#5C524F", color = "#5C524F", alpha = 0.6) +  # hide outliers for cleaner look
  #geom_hline(yintercept = 5.5, linetype = "solid", color = "black") +
  scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 1)) +
  scale_x_discrete(labels = c(
    "MONITOR" = "No action,\njust monitor",
    "STABILIZE" = "Stabilize\nperimeter",
    "COVER" = "Cover with\nrock or concrete",
    "REMOVEPART" = "Remove some\nof the tires",
    "REMOVEALL" = "Remove all\nof the tires")) +
  labs(x = "Proposed Management Action", y = "Level of Support") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
# Boxplots (By Sample)
plot_ACTIONS_all <- ggplot(merged_long, aes(x = ACTION, y = SUPPORT, fill = Sample, color = Sample)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4, ymax = 6),inherit.aes = FALSE,fill = "grey90",alpha = 0.1) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.6) +  # hide outliers for cleaner look
  geom_hline(yintercept = 5, linetype = "solid", color = "black") +
  scale_fill_manual(name = "Sample", values = c("#004F7E", "#5C524F")) +
  scale_color_manual(name = "Sample", values = c("#004F7E", "#5C524F")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_discrete(labels = c(
    "MONITOR" = "No action,\njust monitor",
    "STABILIZE" = "Stabilize\nperimeter",
    "COVER" = "Cover with\nrock or concrete",
    "REMOVEPART" = "Remove some\nof the tires",
    "REMOVEALL" = "Remove all\nof the tires")) +
  labs(x = "Proposed Management Action", y = "Level of Support") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/ACTIONS_all.svg', plot=plot_ACTIONS_all, width=175, height=65, units = "mm", bg = "transparent")

# Boxplots (By Users)
plot_ACTIONS_user <- ggplot(anglers_long, aes(x = ACTION, y = SUPPORT, fill = AR_USER, color = AR_USER)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4, ymax = 6),inherit.aes = FALSE,fill = "grey90",alpha = 0.1) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.6) +  # hide outliers for cleaner look
  geom_hline(yintercept = 5, linetype = "solid", color = "black") +
  scale_fill_manual(name = "Artificial Reef Use", values = c("#5C524F", "#004F7E", "#00806E")) +
  scale_color_manual(name = "Artificial Reef Use", values = c("#5C524F", "#004F7E", "#00806E")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_discrete(labels = c(
    "MONITOR" = "No action,\njust monitor",
    "STABILIZE" = "Stabilize\nperimeter",
    "COVER" = "Cover with\nrock or concrete",
    "REMOVEPART" = "Remove some\nof the tires",
    "REMOVEALL" = "Remove all\nof the tires")) +
  labs(x = "Proposed Management Action", y = "Level of Support") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/ACTIONS_user.svg', plot=plot_ACTIONS_user, width=175, height=65, units = "mm", bg = "transparent")

# Boxplots (By Frequency)
plot_ACTIONS_frequency <- ggplot(anglers_long, aes(x = ACTION, y = SUPPORT, fill = FREQUENCY, color = FREQUENCY)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4, ymax = 6),inherit.aes = FALSE,fill = "grey90",alpha = 0.1) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.6) +  # hide outliers for cleaner look
  geom_hline(yintercept = 5, linetype = "solid", color = "black") +
  scale_fill_manual(name = "Annual Fishing Frequency", values = c("#96CEF0", "#3E9CD5", "#004D79","#002439")) +
  scale_color_manual(name = "Annual Fishing Frequency", values = c("#96CEF0", "#3E9CD5", "#004D79","#002439")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_discrete(labels = c(
    "MONITOR" = "No action,\njust monitor",
    "STABILIZE" = "Stabilize\nperimeter",
    "COVER" = "Cover with\nrock or concrete",
    "REMOVEPART" = "Remove some\nof the tires",
    "REMOVEALL" = "Remove all\nof the tires")) +
  labs(x = "Proposed Management Action", y = "Level of Support") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/ACTIONS_frequency.svg', plot=plot_ACTIONS_frequency, width=175, height=65, units = "mm", bg = "transparent")

# Boxplots (By Duration)
plot_ACTIONS_duration <- ggplot(anglers_long %>%
         # Reclassify groups
         mutate(DURATION = as.character(DURATION)) %>%
         mutate(DURATIONred = ifelse(is.na(DURATION), NA,
                                     ifelse(DURATION == "Less than a year", "Less than 5 years",
                                            ifelse(DURATION == "1 - 4 years", "Less than 5 years", DURATION)))) %>%
         mutate(DURATIONred = factor(DURATIONred, levels = c("Less than 5 years","5 - 9 years","10 - 19 years","20 years or more"))), 
       aes(x = ACTION, y = SUPPORT, fill = DURATIONred, color = DURATIONred)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4, ymax = 6),inherit.aes = FALSE,fill = "grey90",alpha = 0.1) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.6) +  # hide outliers for cleaner look
  geom_hline(yintercept = 5, linetype = "solid", color = "black") +
  scale_fill_manual(name = "Fishing Experience", values = c("#96CEF0", "#3E9CD5", "#004D79","#002439")) +
  scale_color_manual(name = "Fishing Experience", values = c("#96CEF0", "#3E9CD5", "#004D79","#002439")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_discrete(labels = c(
    "MONITOR" = "No action,\njust monitor",
    "STABILIZE" = "Stabilize\nperimeter",
    "COVER" = "Cover with\nrock or concrete",
    "REMOVEPART" = "Remove some\nof the tires",
    "REMOVEALL" = "Remove all\nof the tires")) +
  labs(x = "Proposed Management Action", y = "Level of Support") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/ACTIONS_duration.svg', plot=plot_ACTIONS_duration, width=175, height=65, units = "mm", bg = "transparent")


######### * SPATIAL DISTRIBUTION ######### 

### MAP ANGLER PRESENCE ###

# Reshape
zones_long <- anglers %>%
  pivot_longer(cols = starts_with("ZONE_"),
               names_to = "ZONE",
               names_prefix = "ZONE_",
               values_to = "PRESENCE") %>%
  mutate(ZONE = as.integer(ZONE))

# All anglers (n = 485)
zones <- zones_long %>%
  group_by(ZONE) %>%
  summarise(
    anglers = sum(PRESENCE)) %>%
  mutate(pct = round(anglers/485*100, digits = 0))

# Seagrass anglers (n = 308 - Often or Always)
zones_SEAGRASS <- zones_long %>%
  filter(HABITAT_SEAGRASS == "Often" | HABITAT_SEAGRASS == "Always") %>%
  group_by(ZONE) %>%
  summarise(
    SEAGRASS_anglers = sum(PRESENCE)) %>%
  mutate(SEAGRASS_pct = round(SEAGRASS_anglers/308*100, digits = 0))

# HARDBTM anglers (n = 177 - Often or Always)
zones_HARDBTM <- zones_long %>%
  filter(HABITAT_HARDBTM == "Often" | HABITAT_HARDBTM == "Always") %>%
  group_by(ZONE) %>%
  summarise(
    HARDBTM_anglers = sum(PRESENCE)) %>%
  mutate(HARDBTM_pct = round(HARDBTM_anglers/177*100, digits = 0))

# ARTREEF anglers (n = 122 - Often or Always)
zones_ARTREEF <- zones_long %>%
  filter(HABITAT_ARTREEF == "Often" | HABITAT_ARTREEF == "Always") %>%
  group_by(ZONE) %>%
  summarise(
    ARTREEF_anglers = sum(PRESENCE)) %>%
  mutate(ARTREEF_pct = round(ARTREEF_anglers/122*100, digits = 0))

# RIVER anglers (n = 111 - Often or Always)
zones_RIVER <- zones_long %>%
  filter(HABITAT_RIVER == "Often" | HABITAT_RIVER == "Always") %>%
  group_by(ZONE) %>%
  summarise(
    RIVER_anglers = sum(PRESENCE)) %>%
  mutate(RIVER_pct = round(RIVER_anglers/111*100, digits = 0))

# SHORELINE anglers (n = 374 - Often or Always)
zones_SHORELINE <- zones_long %>%
  filter(HABITAT_SHORELINE == "Often" | HABITAT_SHORELINE == "Always") %>%
  group_by(ZONE) %>%
  summarise(
    SHORELINE_anglers = sum(PRESENCE)) %>%
  mutate(SHORELINE_pct = round(SHORELINE_anglers/374*100, digits = 0))

# INFRASTR anglers (n = 281 - Often or Always)
zones_INFRASTR <- zones_long %>%
  filter(HABITAT_INFRASTR == "Often" | HABITAT_INFRASTR == "Always") %>%
  group_by(ZONE) %>%
  summarise(
    INFRASTR_anglers = sum(PRESENCE)) %>%
  mutate(INFRASTR_pct = round(INFRASTR_anglers/281*100, digits = 0))

# Tire reef anglers (n = 71)
zones_tire <- zones_long %>%
  filter(AR_USER == "Tire Reef User") %>%
  group_by(ZONE) %>%
  summarise(
    tire_anglers = sum(PRESENCE)) %>%
  mutate(tire_pct = round(tire_anglers/71*100, digits = 0))



# Combine
zones <- zones %>%
  left_join(zones_SEAGRASS, by = "ZONE") %>%
  left_join(zones_HARDBTM, by = "ZONE") %>%
  left_join(zones_ARTREEF, by = "ZONE") %>%
  left_join(zones_RIVER, by = "ZONE") %>%
  left_join(zones_SHORELINE, by = "ZONE") %>%
  left_join(zones_INFRASTR, by = "ZONE") %>%
  left_join(zones_tire, by = "ZONE") %>%
  mutate(pct_group = ifelse(pct < 10, 1,
                            ifelse(pct >= 10 & pct < 20, 2,
                                   ifelse(pct >= 20 & pct < 30, 3,
                                          ifelse(pct >= 30 & pct < 40, 4,
                                                 ifelse(pct >= 40 & pct < 50, 5,
                                                        ifelse(pct >= 50 & pct < 60, 6, 7)))))),
         SEAGRASS_pct_group = ifelse(SEAGRASS_pct < 10, 1,
                                     ifelse(SEAGRASS_pct >= 10 & SEAGRASS_pct < 20, 2,
                                            ifelse(SEAGRASS_pct >= 20 & SEAGRASS_pct < 30, 3,
                                                   ifelse(SEAGRASS_pct >= 30 & SEAGRASS_pct < 40, 4,
                                                          ifelse(SEAGRASS_pct >= 40 & SEAGRASS_pct < 50, 5,
                                                                 ifelse(SEAGRASS_pct >= 50 & SEAGRASS_pct < 60, 6, 7)))))),
         HARDBTM_pct_group = ifelse(HARDBTM_pct < 10, 1,
                                     ifelse(HARDBTM_pct >= 10 & HARDBTM_pct < 20, 2,
                                            ifelse(HARDBTM_pct >= 20 & HARDBTM_pct < 30, 3,
                                                   ifelse(HARDBTM_pct >= 30 & HARDBTM_pct < 40, 4,
                                                          ifelse(HARDBTM_pct >= 40 & HARDBTM_pct < 50, 5,
                                                                 ifelse(HARDBTM_pct >= 50 & HARDBTM_pct < 60, 6, 7)))))),
         ARTREEF_pct_group = ifelse(ARTREEF_pct < 10, 1,
                                     ifelse(ARTREEF_pct >= 10 & ARTREEF_pct < 20, 2,
                                            ifelse(ARTREEF_pct >= 20 & ARTREEF_pct < 30, 3,
                                                   ifelse(ARTREEF_pct >= 30 & ARTREEF_pct < 40, 4,
                                                          ifelse(ARTREEF_pct >= 40 & ARTREEF_pct < 50, 5,
                                                                 ifelse(ARTREEF_pct >= 50 & ARTREEF_pct < 60, 6, 7)))))),
         RIVER_pct_group = ifelse(RIVER_pct < 10, 1,
                                     ifelse(RIVER_pct >= 10 & RIVER_pct < 20, 2,
                                            ifelse(RIVER_pct >= 20 & RIVER_pct < 30, 3,
                                                   ifelse(RIVER_pct >= 30 & RIVER_pct < 40, 4,
                                                          ifelse(RIVER_pct >= 40 & RIVER_pct < 50, 5,
                                                                 ifelse(RIVER_pct >= 50 & RIVER_pct < 60, 6, 7)))))),
         SHORELINE_pct_group = ifelse(SHORELINE_pct < 10, 1,
                                     ifelse(SHORELINE_pct >= 10 & SHORELINE_pct < 20, 2,
                                            ifelse(SHORELINE_pct >= 20 & SHORELINE_pct < 30, 3,
                                                   ifelse(SHORELINE_pct >= 30 & SHORELINE_pct < 40, 4,
                                                          ifelse(SHORELINE_pct >= 40 & SHORELINE_pct < 50, 5,
                                                                 ifelse(SHORELINE_pct >= 50 & SHORELINE_pct < 60, 6, 7)))))),
         INFRASTR_pct_group = ifelse(INFRASTR_pct < 10, 1,
                                     ifelse(INFRASTR_pct >= 10 & INFRASTR_pct < 20, 2,
                                            ifelse(INFRASTR_pct >= 20 & INFRASTR_pct < 30, 3,
                                                   ifelse(INFRASTR_pct >= 30 & INFRASTR_pct < 40, 4,
                                                          ifelse(INFRASTR_pct >= 40 & INFRASTR_pct < 50, 5,
                                                                 ifelse(INFRASTR_pct >= 50 & INFRASTR_pct < 60, 6, 7)))))),
         tire_pct_group = ifelse(tire_pct < 10, 1,
                            ifelse(tire_pct >= 10 & tire_pct < 20, 2,
                                   ifelse(tire_pct >= 20 & tire_pct < 30, 3,
                                          ifelse(tire_pct >= 30 & tire_pct < 40, 4,
                                                 ifelse(tire_pct >= 40 & tire_pct < 50, 5,
                                                        ifelse(tire_pct >= 50 & tire_pct < 60, 6, 7)))))))
         
write.csv(zones, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/zones.csv', row.names = FALSE)


