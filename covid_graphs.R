### covid data visualization 

# Geographic categories for covid data based on Wikipedia
Midwest.states <- c("North Dakota","South Dakota","Nebraska","Kansas","Indiana","Missouri","Iowa","Ohio","Wisconsin","Michigan","Minnesota","Illinois")
Northeast.states <- c("Pennsylvania","New Hampshire","Maine","Connecticut","New Jersey","Rhode Island","New York","Vermont","Massachusetts")
South.states <- c("West Virginia","Oklahoma","Kentucky","Alabama","Arkansas","Tennessee","Louisiana","Mississippi","South Carolina","Texas","Georgia","North Carolina","Florida","Virginia","Delaware","Maryland","District of Columbia")
West.states <- c("Wyoming","Idaho","Montana","Utah","Alaska","Arizona","Nevada","Colorado","New Mexico","Oregon","Washington","California","Hawaii")


# covid death graphs
covid.death.midwest <- covid.state.midwest %>%
  ggplot(aes(x = date, y = cumulative.death.cases, color = state)) + 
  geom_line() +
  scale_color_hue(name='Midwest States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(0,40000,5000), limits=c(0,40000),
                     labels=as.character(seq(0,40000,5000)))+
  ylab("Cumulative Death Cases") +
  xlab("Date") + 
  ggtitle("Cumulative Death Cases in Midwest US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.death.northeast <- covid.state.northeast %>%
  ggplot(aes(x = date, y = cumulative.death.cases, color = state)) + 
  geom_line() +
  scale_color_hue(name='Northeast States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(0,40000,5000), limits=c(0,40000),
                     labels=as.character(seq(0,40000,5000)))+
  ylab("Cumulative Death Cases") +
  xlab("Date") + 
  ggtitle("Cumulative Death Cases in Northeast US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.death.south<- covid.state.south %>%
  ggplot(aes(x = date, y = cumulative.death.cases, color = state)) + 
  geom_line() +
  scale_color_hue(name='South States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(0,40000,5000), limits=c(0,40000),
                     labels=as.character(seq(0,40000,5000)))+
  ylab("Cumulative Death Cases") +
  xlab("Date") + 
  ggtitle("Cumulative Death Cases in South US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.death.west <- covid.state.west %>%
  ggplot(aes(x = date, y = cumulative.death.cases, color = state)) + 
  geom_line() +
  scale_color_hue(name='West States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(0,40000,5000), limits=c(0,40000),
                     labels=as.character(seq(0,40000,5000)))+
  ylab("Cumulative Death Cases") +
  xlab("Date") + 
  ggtitle("Cumulative Death Cases in West US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style


# covid confirmed cases graphs
covid.confirm.midwest <- covid.state.midwest %>%
  ggplot(aes(x = date, y = cumulative.confirmed.cases, color = state)) + 
  geom_line() +
  scale_color_hue(name='Midwest States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 1000000)+
  ylab("Cumulative Confirmed Cases") +
  xlab("Date") + 
  ggtitle("Cumulative Confirmed Cases in Midwest US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.confirm.northeast <- covid.state.northeast %>%
  ggplot(aes(x = date, y = cumulative.confirmed.cases, color = state)) + 
  geom_line() +
  scale_color_hue(name='Northeast States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 1000000)+
  ylab("Cumulative Confirmed Cases") +
  xlab("Date") + 
  ggtitle("Cumulative Confirmed Cases in Northeast US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.confirm.south <- covid.state.south %>%
  ggplot(aes(x = date, y = cumulative.confirmed.cases, color = state)) + 
  geom_line() +
  scale_color_hue(name='South States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 1000000)+
  ylab("Cumulative Confirmed Cases") +
  xlab("Date") + 
  ggtitle("Cumulative Confirmed Cases in South US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.confirm.west <- covid.state.west %>%
  ggplot(aes(x = date, y = cumulative.confirmed.cases, color = state)) + 
  geom_line() +
  scale_color_hue(name='West States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 1000000)+
  ylab("Cumulative Confirmed Cases") +
  xlab("Date") + 
  ggtitle("Cumulative Confirmed Cases in West US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style


# covid testing rate graphs 
# Only starting from April 12th because we only have data dtarting from then
covid.test.rate.midwest <- covid.state.midwest %>%
  filter(date > as.Date("2020-04-11")) %>% 
  ggplot(aes(x = date, y = testing.rate, color = state)) + 
  geom_line() +
  scale_color_hue(name='Midwest States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(0,120,20), limits=c(0,105),
                     labels=as.character(seq(0,120,20)))+
  ylab("Testing Rate") +
  xlab("Date") + 
  ggtitle("Testing Rate in Midwest US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.test.rate.northeast <- covid.state.northeast %>%
  filter(date > as.Date("2020-04-11")) %>% 
  ggplot(aes(x = date, y = testing.rate, color = state)) + 
  geom_line() +
  scale_color_hue(name='Northeast States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(0,120,20), limits=c(0,105),
                     labels=as.character(seq(0,120,20)))+
  ylab("Testing Rate") +
  xlab("Date") + 
  ggtitle("Testing Rate in Northeast US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.test.rate.south <- covid.state.south %>%
  filter(date > as.Date("2020-04-11")) %>% 
  ggplot(aes(x = date, y = testing.rate, color = state)) + 
  geom_line() +
  scale_color_hue(name='South States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(0,120,20), limits=c(0,105),
                     labels=as.character(seq(0,120,20)))+
  ylab("Testing Rate") +
  xlab("Date") + 
  ggtitle("Testing Rate in South US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.test.rate.west <- covid.state.west %>%
  filter(date > as.Date("2020-04-11")) %>% 
  ggplot(aes(x = date, y = testing.rate, color = state)) + 
  geom_line() +
  scale_color_hue(name='West States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(0,120,20), limits=c(0,105),
                     labels=as.character(seq(0,120,20)))+
  ylab("Testing Rate") +
  xlab("Date") + 
  ggtitle("Testing Rate in West US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style


# covid testing postive rate graphs 
# Only starting from April 12th because we only have data starting from then
covid.incident.rate.midwest <- covid.state.midwest %>%
  filter(date > as.Date("2020-04-11")) %>% 
  ggplot(aes(x = date, y = incident.rate, color = state)) + 
  geom_line() +
  scale_color_hue(name='Midwest States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 5)+
  ylab("Incident Rate") +
  xlab("Date") + 
  ggtitle("Incident Rate in Midwest US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.incident.rate.northeast <- covid.state.northeast %>%
  filter(date > as.Date("2020-04-11")) %>% 
  ggplot(aes(x = date, y = incident.rate, color = state)) + 
  geom_line() +
  scale_color_hue(name='Northeast States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 5)+
  ylab("Incident Rate") +
  xlab("Date") + 
  ggtitle("Incident Rate in Northeast US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.incident.rate.south <- covid.state.south %>%
  filter(date > as.Date("2020-04-11")) %>% 
  ggplot(aes(x = date, y = incident.rate, color = state)) + 
  geom_line() +
  scale_color_hue(name='South States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 5)+
  ylab("Incident Rate") +
  xlab("Date") + 
  ggtitle("Incident Rate in South US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

covid.incident.rate.west <- covid.state.west %>%
  filter(date > as.Date("2020-04-11")) %>% 
  ggplot(aes(x = date, y = incident.rate, color = state)) + 
  geom_line() +
  scale_color_hue(name='West States')+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 5)+
  ylab("Incident Rate") +
  xlab("Date") + 
  ggtitle("Incident Rate in West US") + 
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

















