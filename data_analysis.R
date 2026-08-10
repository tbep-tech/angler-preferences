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


######### * SUMMARIES ######### 

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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/AGE.svg', plot=plot_AGE, width=175, height=65, units = "mm", bg = "transparent")
write.csv(AGE, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/AGE.csv', row.names = FALSE)


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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/GENDER.svg', plot=plot_GENDER, width=175, height=65, units = "mm", bg = "transparent")
write.csv(GENDER, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/GENDER.csv', row.names = FALSE)


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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/COUNTY.svg', plot=plot_COUNTY, width=175, height=65, units = "mm", bg = "transparent")
write.csv(COUNTY, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/COUNTY.csv', row.names = FALSE)


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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/FREQUENCY.svg', plot=plot_SEASONALITY, width=175, height=65, units = "mm", bg = "transparent")
write.csv(FREQUENCY, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/FREQUENCY.csv', row.names = FALSE)


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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/DURATION.svg', plot=plot_DURATION, width=175, height=65, units = "mm", bg = "transparent")
write.csv(DURATION, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/DURATION.csv', row.names = FALSE)


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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/SKILL.svg', plot=plot_SKILL, width=175, height=65, units = "mm", bg = "transparent")
write.csv(SKILL, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/SKILL.csv', row.names = FALSE)


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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/AWARE.svg', plot=plot_AWARE, width=175, height=65, units = "mm", bg = "transparent")
write.csv(AWARE, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/AWARE.csv', row.names = FALSE)

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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/FUNDING.svg', plot=plot_FUNDING, width=175, height=65, units = "mm", bg = "transparent")
write.csv(FUNDING, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/FUNDING.csv', row.names = FALSE)



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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/REASONS.svg', plot=plot_REASONS, width=170, height=100, units = "mm", bg = "transparent")
write.csv(REASONS, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/REASONS.csv', row.names = FALSE)


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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/REASONS.svg', plot=plot_REASONS, width=170, height=100, units = "mm", bg = "transparent")
write.csv(REASONS, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/REASONS.csv', row.names = FALSE)

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
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/AR_USER.svg', plot=plot_AR_USER, width=175, height=65, units = "mm", bg = "transparent")
write.csv(AR_USER, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/AR_USER.csv', row.names = FALSE)



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
# Boxplots (By Duration)
ggplot(anglers_long %>%
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
#ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/SHORE_DESIRE.svg', plot=plot_SHORE_DESIRE, width=200, height=125, units = "mm", bg = "transparent")


######### * SPATIAL DISTRIBUTION ######### 

### MAP ANGLER PRESENCE ###

# Identify Support/Opposition of Complete Tire Removal
anglers <- anglers %>%
  mutate(REMOVEALL_CAT = ifelse(ACTION_REMOVEALL <= 4, "Oppose",
                                 ifelse(ACTION_REMOVEALL >= 6, "Support", "Neutral")))

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

# Artificial reef anglers (n = 402)
zones_AR <- zones_long %>%
  filter(AR_USER != "Non-user") %>%
  group_by(ZONE) %>%
  summarise(
    AR_anglers = sum(PRESENCE)) %>%
  mutate(AR_pct = round(AR_anglers/402*100, digits = 0))

# Tire reef anglers (n = 71)
zones_tire <- zones_long %>%
  filter(AR_USER == "Tire Reef User") %>%
  group_by(ZONE) %>%
  summarise(
    tire_anglers = sum(PRESENCE)) %>%
  mutate(tire_pct = round(tire_anglers/71*100, digits = 0))

# Complete Removal Opposition (n = 130)
zones_oppose <- zones_long %>%
  filter(REMOVEALL_CAT == "Oppose") %>%
  group_by(ZONE) %>%
  summarise(
    oppose_anglers = sum(PRESENCE)) %>%
  mutate(oppose_pct = round(oppose_anglers/130*100, digits = 0))

# Complete Removal Support (n = 263)
zones_support <- zones_long %>%
  filter(REMOVEALL_CAT == "Support") %>%
  group_by(ZONE) %>%
  summarise(
    support_anglers = sum(PRESENCE)) %>%
  mutate(support_pct = round(support_anglers/263*100, digits = 0))

# Combine
zones <- zones %>%
  left_join(zones_AR, by = "ZONE") %>%
  left_join(zones_tire, by = "ZONE") %>%
  left_join(zones_support, by = "ZONE") %>%
  left_join(zones_oppose, by = "ZONE") %>%
  mutate(pct_group = ifelse(pct < 10, 1,
                            ifelse(pct >= 10 & pct < 20, 2,
                                   ifelse(pct >= 20 & pct < 30, 3,
                                          ifelse(pct >= 30 & pct < 40, 4,
                                                 ifelse(pct >= 40 & pct < 50, 5,
                                                        ifelse(pct >= 50 & pct < 60, 6, 7)))))),
         tire_pct_group = ifelse(tire_pct < 10, 1,
                            ifelse(tire_pct >= 10 & tire_pct < 20, 2,
                                   ifelse(tire_pct >= 20 & tire_pct < 30, 3,
                                          ifelse(tire_pct >= 30 & tire_pct < 40, 4,
                                                 ifelse(tire_pct >= 40 & tire_pct < 50, 5,
                                                        ifelse(tire_pct >= 50 & tire_pct < 60, 6, 7)))))),
         support_pct_group = ifelse(support_pct < 10, 1,
                                 ifelse(support_pct >= 10 & support_pct < 20, 2,
                                        ifelse(support_pct >= 20 & support_pct < 30, 3,
                                               ifelse(support_pct >= 30 & support_pct < 40, 4,
                                                      ifelse(support_pct >= 40 & support_pct < 50, 5,
                                                             ifelse(support_pct >= 50 & support_pct < 60, 6, 7)))))),
         oppose_pct_group = ifelse(oppose_pct < 10, 1,
                                 ifelse(oppose_pct >= 10 & oppose_pct < 20, 2,
                                        ifelse(oppose_pct >= 20 & oppose_pct < 30, 3,
                                               ifelse(oppose_pct >= 30 & oppose_pct < 40, 4,
                                                      ifelse(oppose_pct >= 40 & oppose_pct < 50, 5,
                                                             ifelse(oppose_pct >= 50 & oppose_pct < 60, 6, 7)))))))
         
write.csv(zones, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/zones.csv', row.names = FALSE)


### MAP SUPPORT FOR COMPLETE REMOVAL ###

# Keep only zones that a person selected
zones_present <- zones_long %>%
  filter(PRESENCE == 1)

# Calculate average support for complete removal by zone
zone_means_all <- zones_present %>%
  group_by(ZONE) %>%
  summarise(
    median = median(ACTION_REMOVEALL, na.rm = TRUE),
    mean = mean(ACTION_REMOVEALL, na.rm = TRUE),
    sd = sd(ACTION_REMOVEALL, na.rm = TRUE),
    .groups = "drop")

write.csv(zone_means_all, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/Angler Preferences/Results/zones_medians.csv', row.names = FALSE)


zone_means_allpart <- zones_present %>%
  group_by(ZONE) %>%
  summarise(
    median = median(ACTION_REMOVEPART, na.rm = TRUE),
    mean = mean(ACTION_REMOVEPART, na.rm = TRUE),
    sd = sd(ACTION_REMOVEPART, na.rm = TRUE),
    .groups = "drop")


zone_means_allstabilize <- zones_present %>%
  group_by(ZONE) %>%
  summarise(
    median = median(ACTION_STABILIZE, na.rm = TRUE),
    mean = mean(ACTION_STABILIZE, na.rm = TRUE),
    sd = sd(ACTION_STABILIZE, na.rm = TRUE),
    .groups = "drop")



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
    "RESTORETOOL" = "... are useful for\nrestoring habitats\n(Positive)",
    "SEDSTABILIZE" = "... stabilize sediments,\nprevent erosion\n(Positive)",
    "ATTRACTFISH" = "... attract a lot of\ndesirable fish\n(Positive)",
    "DAMAGEHAB" = "... damage natural\nhabitats\n(Negative)",
    "WORSEWQ" = "... worsen water\nquality\n(Negative)")) +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/PERCEPTIONS_TIRES.svg', plot=plot_PERCEPTIONS_TIRES, width=175, height=75, units = "mm", bg = "transparent")
write.csv(PERCEPTIONS_TIRES, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/PERCEPTIONS_TIRES.csv', row.names = FALSE)

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
    "RESTORETOOL" = "... are useful for\nrestoring habitats\n(Positive)",
    "SEDSTABILIZE" = "... stabilize sediments,\nprevent erosion\n(Positive)",
    "ATTRACTFISH" = "... attract a lot of\ndesirable fish\n(Positive)",
    "DAMAGEHAB" = "... damage natural\nhabitats\n(Negative)",
    "WORSEWQ" = "... worsen water\nquality\n(Negative)")) +
  #coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.title.y = element_text(margin = margin(r = 9)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
ggsave(file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/PERCEPTIONS_CONCRETE.svg', plot=plot_PERCEPTIONS_CONCRETE, width=175, height=75, units = "mm", bg = "transparent")
write.csv(PERCEPTIONS_CONCRETE, file='C:/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/BSS/RESULTS/PERCEPTIONS_CONCRETE.csv', row.names = FALSE)










