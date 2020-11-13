### CDC/SVI data visualizations


# Geographic categories for covid data based on Wikipedia
Midwest.states <- c("North Dakota","South Dakota","Nebraska","Kansas","Indiana","Missouri","Iowa","Ohio","Wisconsin","Michigan","Minnesota","Illinois")
Northeast.states <- c("Pennsylvania","New Hampshire","Maine","Connecticut","New Jersey","Rhode Island","New York","Vermont","Massachusetts")
South.states <- c("West Virginia","Oklahoma","Kentucky","Alabama","Arkansas","Tennessee","Louisiana","Mississippi","South Carolina","Texas","Georgia","North Carolina","Florida","Virginia","Delaware","Maryland","District of Columbia")
West.states <- c("Wyoming","Idaho","Montana","Utah","Alaska","Arizona","Nevada","Colorado","New Mexico","Oregon","Washington","California","Hawaii")
South.states.1 <- c("West Virginia","Oklahoma","Kentucky","Alabama","Arkansas","Tennessee","Louisiana","Mississippi","South Carolina","Texas","Georgia","North Carolina","Florida","Virginia","Delaware","Maryland","District Of Columbia")
# made adjustment for DC spelling difference


svi.state_Midwest <- filter(svi.state, STATE %in% Midwest.states)
svi.state_Northeast <- filter(svi.state, STATE %in% Northeast.states)
svi.state_South <- filter(svi.state, STATE %in% South.states.1)
svi.state_West <- filter(svi.state, STATE %in% West.states)


# Plotting poverty rate
svi.pov.mw <- svi.state_Midwest %>%
  ggplot(aes(x = reorder(STATE, poverty.rate), y = poverty.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.21) +
  xlab("State") +
  ylab("Poverty Rate") + 
  ggtitle("Poverty Rate in Midwest US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.pov.ne <- svi.state_Northeast %>%
  ggplot(aes(x = reorder(STATE, poverty.rate), y = poverty.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.21) +
  xlab("State") +
  ylab("Poverty Rate") + 
  ggtitle("Poverty Rate in Northeast US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.pov.s <- svi.state_South %>%
  ggplot(aes(x = reorder(STATE, poverty.rate), y = poverty.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.21) +
  xlab("State") +
  ylab("Poverty Rate") + 
  ggtitle("Poverty Rate in South US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.pov.w <- svi.state_West %>%
  ggplot(aes(x = reorder(STATE, poverty.rate), y = poverty.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  geom_text(x=13, y =0.042, label = "New Mexico data missing",
            size = 5, color = "black")+
  ylim(0, 0.21) +
  xlab("State") +
  ylab("Poverty Rate") + 
  ggtitle("Poverty Rate in South US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style
### New Mexico data missing


# Plotting uninsured rate
svi.uninsured.mw <- svi.state_Midwest %>%
  ggplot(aes(x = reorder(STATE, uninsured.rate), y = uninsured.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.175) +
  xlab("State") +
  ylab("Uninsured  Rate") + 
  ggtitle("Uninsured Rate in Midwest US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.uninsured.ne <- svi.state_Northeast %>%
  ggplot(aes(x = reorder(STATE, uninsured.rate), y = uninsured.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.175) +
  xlab("State") +
  ylab("Uninsured  Rate") + 
  ggtitle("Uninsured Rate in Northeast US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.uninsured.s <- svi.state_South %>%
  ggplot(aes(x = reorder(STATE, uninsured.rate), y = uninsured.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.175) +
  xlab("State") +
  ylab("Uninsured  Rate") + 
  ggtitle("Uninsured Rate in South US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.uninsured.w <- svi.state_West %>%
  ggplot(aes(x = reorder(STATE, uninsured.rate), y = uninsured.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.175) +
  xlab("State") +
  ylab("Uninsured  Rate") + 
  ggtitle("Uninsured Rate in West US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style


# Plotting mobile home rate
svi.mobile.mw <- svi.state_Midwest %>%
  ggplot(aes(x = reorder(STATE, mobile.home.rate), y = mobile.home.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.08) +
  xlab("State") +
  ylab("Mobile Home Rate") + 
  ggtitle("Mobile Home Rate in Midwest US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.mobile.ne <- svi.state_Northeast %>%
  ggplot(aes(x = reorder(STATE, mobile.home.rate), y = mobile.home.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.08) +
  xlab("State") +
  ylab("Mobile Home Rate") + 
  ggtitle("Mobile Home Rate in Northeast US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.mobile.s <- svi.state_South %>%
  ggplot(aes(x = reorder(STATE, mobile.home.rate), y = mobile.home.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.08) +
  xlab("State") +
  ylab("Mobile Home Rate") + 
  ggtitle("Mobile Home Rate in South US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.mobile.w <- svi.state_West %>%
  ggplot(aes(x = reorder(STATE, mobile.home.rate), y = mobile.home.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.08) +
  xlab("State") +
  ylab("Mobile Home Rate") + 
  ggtitle("Mobile Home Rate in West US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style


# Plotting minority rate
svi.minority.mw <- svi.state_Midwest %>%
  ggplot(aes(x = reorder(STATE, minority.rate), y = minority.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.8) +
  xlab("State") +
  ylab("Minority Rate") + 
  ggtitle("Minority Rate in Midwest US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.minority.ne <- svi.state_Northeast %>%
  ggplot(aes(x = reorder(STATE, minority.rate), y = minority.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.8) +
  xlab("State") +
  ylab("Minority Rate") + 
  ggtitle("Minority Rate in Northeast US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.minority.s <- svi.state_South %>%
  ggplot(aes(x = reorder(STATE, minority.rate), y = minority.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.8) +
  xlab("State") +
  ylab("Minority Rate") + 
  ggtitle("Minority Rate in South US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.minority.w <- svi.state_West %>%
  ggplot(aes(x = reorder(STATE, minority.rate), y = minority.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.8) +
  xlab("State") +
  ylab("Minority Rate") + 
  ggtitle("Minority Rate in West US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style


# Plotting disabled rate
svi.disabled.mw <- svi.state_Midwest %>%
  ggplot(aes(x = reorder(STATE, disabled.rate), y = disabled.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.2) +
  xlab("State") +
  ylab("Disabled Rate") + 
  ggtitle("Disabled Rate in Midwest US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.disabled.ne <- svi.state_Northeast %>%
  ggplot(aes(x = reorder(STATE, disabled.rate), y = disabled.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.2) +
  xlab("State") +
  ylab("Disabled Rate") + 
  ggtitle("Disabled Rate in Northeast US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.disabled.s <- svi.state_South %>%
  ggplot(aes(x = reorder(STATE, disabled.rate), y = disabled.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.2) +
  xlab("State") +
  ylab("Disabled Rate") + 
  ggtitle("Disabled Rate in South US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.disabled.w <- svi.state_West %>%
  ggplot(aes(x = reorder(STATE, disabled.rate), y = disabled.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.2) +
  xlab("State") +
  ylab("Disabled Rate") + 
  ggtitle("Disabled Rate in West US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style


# Plotting elderly rate
svi.old.mw <- svi.state_Midwest %>%
  ggplot(aes(x = reorder(STATE, age65.rate), y = age65.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.2) +
  xlab("State") +
  ylab("Age over 65 Rate") + 
  ggtitle("Age over 65 Rate in Midwest US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.old.ne <- svi.state_Northeast %>%
  ggplot(aes(x = reorder(STATE, age65.rate), y = age65.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.2) +
  xlab("State") +
  ylab("Age over 65 Rate") + 
  ggtitle("Age over 65 Rate in Northeast US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.old.s <- svi.state_South %>%
  ggplot(aes(x = reorder(STATE, age65.rate), y = age65.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.2) +
  xlab("State") +
  ylab("Age over 65 Rate") + 
  ggtitle("Age over 65 Rate in South US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.old.w <- svi.state_West %>%
  ggplot(aes(x = reorder(STATE, age65.rate), y = age65.rate, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 0.2) +
  xlab("State") +
  ylab("Age over 65 Rate") + 
  ggtitle("Age over 65 Rate in West US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style


# Plotting per capita income
svi.pci.mw <- svi.state_Midwest %>%
  ggplot(aes(x = reorder(STATE, per.capita), y = per.capita, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 55000) +
  xlab("State") +
  ylab("Per Capita Income") + 
  ggtitle("Per Capita Income in Midwest US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.pci.ne <- svi.state_Northeast %>%
  ggplot(aes(x = reorder(STATE, per.capita), y = per.capita, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 55000) +
  xlab("State") +
  ylab("Per Capita Income") + 
  ggtitle("Per Capita Income in Northeast US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.pci.s <- svi.state_South %>%
  ggplot(aes(x = reorder(STATE, per.capita), y = per.capita, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  ylim(0, 55000) +
  xlab("State") +
  ylab("Per Capita Income") + 
  ggtitle("Per Capita Income in South US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

svi.pci.w <- svi.state_West %>%
  ggplot(aes(x = reorder(STATE, per.capita), y = per.capita, color = STATE, fill = STATE)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  geom_text(x=13, y =10800, label = "New Mexico data missing",
            size = 5, color = "black")+
  ylim(0, 55000) +
  xlab("State") +
  ylab("Per Capita Income") + 
  ggtitle("Per Capita Income in West US") + 
  theme_minimal() +
  theme(legend.position = 'none', # remove legend,
        axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style