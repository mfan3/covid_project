### unemployment data visualization 

# Geographic categories for covid data based on Wikipedia
Midwest.states <- c("North Dakota","South Dakota","Nebraska","Kansas","Indiana","Missouri","Iowa","Ohio","Wisconsin","Michigan","Minnesota","Illinois")
Northeast.states <- c("Pennsylvania","New Hampshire","Maine","Connecticut","New Jersey","Rhode Island","New York","Vermont","Massachusetts")
South.states <- c("West Virginia","Oklahoma","Kentucky","Alabama","Arkansas","Tennessee","Louisiana","Mississippi","South Carolina","Texas","Georgia","North Carolina","Florida","Virginia","Delaware","Maryland","District of Columbia")
West.states <- c("Wyoming","Idaho","Montana","Utah","Alaska","Arizona","Nevada","Colorado","New Mexico","Oregon","Washington","California","Hawaii")



unemployment_Midwest <- filter(unemployment, state %in% Midwest.states)
unemployment_Northeast <- filter(unemployment, state %in% Northeast.states)
unemployment_South <- filter(unemployment, state %in% South.states)
unemployment_West <- filter(unemployment, state %in% West.states)


# Plotting 2020 unemployment

unemploy.mw <- unemployment_Midwest %>%
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(x = month, y = unemployment.rate.2020, color = state)) +
  geom_line() +
  geom_point() +
  scale_color_hue(name='Midwest States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,5), limits=c(0,30),
                     labels=as.character(seq(-60,60,5)))+
  ylim(0, 30)+
  ylab("Unemployment Rate") +
  xlab("Month") + 
  ggtitle("2020 Unemployment Rate in Midwest US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

unemploy.ne <- unemployment_Northeast %>%
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(x = month, y = unemployment.rate.2020, color = state)) + 
  geom_line() +
  geom_point() +
  scale_color_hue(name='Northeast States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,5), limits=c(0,30),
                     labels=as.character(seq(-60,60,5)))+
  ylim(0, 30)+
  ylab("Unemployment Rate") +
  xlab("Month") + 
  ggtitle("2020 Unemployment Rate in Northeast US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

unemploy.s <- unemployment_South %>%
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(x = month, y = unemployment.rate.2020, color = state)) + 
  geom_line() +
  geom_point() +
  scale_color_hue(name='South States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,5), limits=c(0,30),
                     labels=as.character(seq(-60,60,5)))+
  ylim(0, 30)+
  ylab("Unemployment Rate") +
  xlab("Month") + 
  ggtitle("2020 Unemployment Rate in South US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

unemploy.w <- unemployment_West %>%
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(x = month, y = unemployment.rate.2020, color = state)) + 
  geom_line() +
  geom_point() +
  scale_color_hue(name='West States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,5), limits=c(0,30),
                     labels=as.character(seq(-60,60,5)))+
  ylab("Unemployment Rate") +
  xlab("Month") + 
  ggtitle("2020 Unemployment Rate in West US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

