library(tidyverse)

# Load data
final_data <- read.csv("Data/data_clean.csv")

anglers <- final_data[,-1]
rownames(anglers) <- final_data[,1]

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
         AWARE = factor(AWARE, levels = c("No","Yes")))

# Determine which anglers use tire reefs
anglers <- anglers %>%
  mutate(AR_USER = ifelse(HABITAT_ARTREEF == "Never" | HABITAT_ARTREEF == "Not sure", "Non-user",
                          ifelse(is.na(AR_MATERIAL_TIRE) | AR_MATERIAL_TIRE == "Never" | AR_MATERIAL_TIRE == "Not sure", "Other Reef User", "Tire Reef User"))) %>%
  mutate(AR_USER = factor(AR_USER, levels = c("Non-user","Other Reef User","Tire Reef User")))

         
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
    .groups = "drop")
action_means_users <- anglers_long %>%
  group_by(AR_USER, ACTION) %>%
  summarise(
    mean = mean(SUPPORT, na.rm = TRUE),
    sd = sd(SUPPORT, na.rm = TRUE),
    .groups = "drop")

#write.csv(action_means, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/action_means.csv', row.names = FALSE)

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
# Boxplots (By Users)
ggplot(anglers_long, aes(x = ACTION, y = SUPPORT, fill = AR_USER, color = AR_USER)) +
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
# Boxplots (By Funding Support)
ggplot(anglers_long, aes(x = ACTION, y = SUPPORT, fill = FUNDING, color = FUNDING)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4, ymax = 6),inherit.aes = FALSE,fill = "grey90",alpha = 0.1) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.6) +  # hide outliers for cleaner look
  geom_hline(yintercept = 5, linetype = "solid", color = "black") +
  scale_fill_manual(name = "Use of Public Funds for\nArtificial Reef Management", values = c("#962C14", "#DC9E00", "#004F7E")) +
  scale_color_manual(name = "Use of Public Funds for\nArtificial Reef Management", values = c("#962C14", "#DC9E00", "#004F7E")) +
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
# Boxplots (By Frequency)
ggplot(anglers_long, aes(x = ACTION, y = SUPPORT, fill = FREQUENCY, color = FREQUENCY)) +
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
#ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/SHORE_DESIRE.svg', plot=plot_SHORE_DESIRE, width=200, height=125, units = "mm", bg = "transparent")


######### * SPATIAL DISTRIBUTION ######### 

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
  mutate(pct = anglers/485*100)

# Artificial reef anglers (n = 402)
zones_AR <- zones_long %>%
  filter(AR_USER != "Non-user") %>%
  group_by(ZONE) %>%
  summarise(
    AR_anglers = sum(PRESENCE)) %>%
  mutate(AR_pct = AR_anglers/402*100)

# Tire reef anglers (n = 71)
zones_tire <- zones_long %>%
  filter(AR_USER == "Tire Reef User") %>%
  group_by(ZONE) %>%
  summarise(
    tire_anglers = sum(PRESENCE)) %>%
  mutate(tire_pct = tire_anglers/71*100)

# Combine
zones <- cbind(zones, zones_AR, zones_tire)


#write.csv(action_means, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/action_means.csv', row.names = FALSE)


# Create table of zone hotspots
zones <- anglers %>%
  group_by()

