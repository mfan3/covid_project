### Google mobility data visualizations 


# Geographic categories for covid data based on Wikipedia
Midwest.states <- c("North Dakota","South Dakota","Nebraska","Kansas","Indiana","Missouri","Iowa","Ohio","Wisconsin","Michigan","Minnesota","Illinois")
Northeast.states <- c("Pennsylvania","New Hampshire","Maine","Connecticut","New Jersey","Rhode Island","New York","Vermont","Massachusetts")
South.states <- c("West Virginia","Oklahoma","Kentucky","Alabama","Arkansas","Tennessee","Louisiana","Mississippi","South Carolina","Texas","Georgia","North Carolina","Florida","Virginia","Delaware","Maryland","District of Columbia")
West.states <- c("Wyoming","Idaho","Montana","Utah","Alaska","Arizona","Nevada","Colorado","New Mexico","Oregon","Washington","California","Hawaii")

StateMonthMobility_Midwest <- filter(StateMonthMobility2,sub_region_1 %in% Midwest.states)
StateMonthMobility_Northeast <- filter(StateMonthMobility2,sub_region_1 %in% Northeast.states)
StateMonthMobility_South <- filter(StateMonthMobility2,sub_region_1 %in% South.states)
StateMonthMobility_West <- filter(StateMonthMobility2,sub_region_1 %in% West.states)


# Plotting for Mean Percent Change of Retail+Recreation
mobility.r.midwest <- StateMonthMobility_Midwest %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_retial_and_recreation_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Retail and Recreation in Midwest US")+
  ylab("Mean Percent Change of Retail and Recreation") + 
  xlab("Month") + 
  labs(color="Midwest States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-65,20),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.r.northeast <- StateMonthMobility_Northeast %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_retial_and_recreation_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Retail and Recreation in Northeast US")+
  ylab("Mean Percent Change of Retail and Recreation") + 
  xlab("Month") + 
  labs(color="Northeast States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-65,20),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.r.south <- StateMonthMobility_South %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_retial_and_recreation_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Retail and Recreation in South US")+
  ylab("Mean Percent Change of Retail and Recreation") + 
  xlab("Month") + 
  labs(color="South States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-65,20),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style


mobility.r.west <- StateMonthMobility_West %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_retial_and_recreation_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Retail and Recreation in West US")+
  ylab("Mean Percent Change of Retail and Recreation") + 
  xlab("Month") + 
  labs(color="West States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-65,20),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

# grid.arrange(p1,p2,p3,p4,nrow=2,top="Relationship between Mean Percent Change of Retail+Recreation and Month by States")


# Plotting for Mean Percent Change of Grocery+Pharmacy 
mobility.gp.midwest <- StateMonthMobility_Midwest %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_grocery_and_pharmacy_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Grocery and Pharmacy in Midwest US")+
  ylab("Mean Percent Change of Grocery and Pharmacy") + 
  xlab("Month") + 
  labs(color="Midwest States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-35,35),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.gp.northeast <- StateMonthMobility_Northeast %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_grocery_and_pharmacy_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Grocery and Pharmacy in Northeast US")+
  ylab("Mean Percent Change of Grocery and Pharmacy") + 
  xlab("Month") + 
  labs(color="Northeast States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-35,35),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.gp.south <- StateMonthMobility_South %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_grocery_and_pharmacy_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Grocery and Pharmacy in South US")+
  ylab("Mean Percent Change of Grocery and Pharmacy") + 
  xlab("Month") + 
  labs(color="South States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-35,35),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.gp.west <- StateMonthMobility_West %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_grocery_and_pharmacy_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Grocery and Pharmacy in West US")+
  ylab("Mean Percent Change of Grocery and Pharmacy") + 
  xlab("Month") + 
  labs(color="West States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-35,35),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

# grid.arrange(q1,q2,q3,q4,nrow=2,top="Relationship between Mean Percent Change of Grocery+Pharmacy and Month by States")


# Plotting for Mean Percent Change of Parks 
mobility.parks.midwest <- StateMonthMobility_Midwest %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_parks_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Parks in Midwest US")+
  ylab("Mean Percent Change of Parks") + 
  xlab("Month") + 
  labs(color="Midwest States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-100,400,50), limits=c(-75,400),
                     labels=as.character(seq(-100,400,50)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.parks.northeast <- StateMonthMobility_Northeast %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_parks_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Parks in Northeast US")+
  ylab("Mean Percent Change of Parks") + 
  xlab("Month") + 
  labs(color="Northeast States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-100,400,50), limits=c(-75,400),
                     labels=as.character(seq(-100,400,50)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.parks.south <- StateMonthMobility_South %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_parks_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Parks in South US")+
  ylab("Mean Percent Change of Parks") + 
  xlab("Month") + 
  labs(color="South States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-100,400,50), limits=c(-75,400),
                     labels=as.character(seq(-100,400,50)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.parks.west <- StateMonthMobility_West %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_parks_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Parks in West US")+
  ylab("Mean Percent Change of Parks") + 
  xlab("Month") + 
  labs(color="West States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-100,400,50), limits=c(-75,400),
                     labels=as.character(seq(-100,400,50)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

# grid.arrange(r1,r2,r3,r4,nrow=2,top="Relationship between Mean Percent Change of Parks and Month by States")


# Plotting for Mean Percent Change of Transit Stations 
mobility.transit.midwest <- StateMonthMobility_Midwest %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_transit_stations_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Transit Stations in Midwest US")+
  ylab("Mean Percent Change of Transit Stations") + 
  xlab("Month") + 
  labs(color="Midwest States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-100,100,10), limits=c(-70,45),
                     labels=as.character(seq(-100,100,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.transit.northeast <- StateMonthMobility_Northeast %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_transit_stations_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Transit Stations in Northeast US")+
  ylab("Mean Percent Change of Transit Stations") + 
  xlab("Month") + 
  labs(color="Northeast States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-100,100,10), limits=c(-70,45),
                     labels=as.character(seq(-100,100,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.transit.south <- StateMonthMobility_South %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_transit_stations_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Transit Stations in South US")+
  ylab("Mean Percent Change of Transit Stations") + 
  xlab("Month") + 
  labs(color="South States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-100,100,10), limits=c(-70,45),
                     labels=as.character(seq(-100,100,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.transit.west <- StateMonthMobility_West %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_transit_stations_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Transit Stations in West US")+
  ylab("Mean Percent Change of Transit Stations") + 
  xlab("Month") + 
  labs(color="West States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-100,100,10), limits=c(-70,45),
                     labels=as.character(seq(-100,100,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

# grid.arrange(s1,s2,s3,s4,nrow=2,top="Relationship between Mean Percent Change of Transit Stations and Month by States")


# Plotting for Mean Percent Change of Workplaces 
mobility.work.midwest <- StateMonthMobility_Midwest %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_workplaces_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Workplaces in Midwest US")+
  ylab("Mean Percent Change of Workplaces") + 
  xlab("Month") + 
  labs(color="Midwest States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-65,10),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.work.northeast <- StateMonthMobility_Northeast %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_workplaces_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Workplaces in Northeast US")+
  ylab("Mean Percent Change of Workplaces") + 
  xlab("Month") + 
  labs(color="Northeast States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-65,10),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.work.south <- StateMonthMobility_South %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_workplaces_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Workplaces in South US")+
  ylab("Mean Percent Change of Workplaces") + 
  xlab("Month") + 
  labs(color="South States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-65,10),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.work.west <- StateMonthMobility_West %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_workplaces_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Workplaces in West US")+
  ylab("Mean Percent Change of Workplaces") + 
  xlab("Month") + 
  labs(color="West States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,10), limits=c(-65,10),
                     labels=as.character(seq(-60,60,10)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

# grid.arrange(t1,t2,t3,t4,nrow=2,top="Relationship between Mean Percent Change of Workplaces and Month by States")


# Plotting Mean Percent Change of Residential (%change of people staying at home)
mobility.res.midwest <- StateMonthMobility_Midwest %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_residential_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Residential in Midwest US")+
  ylab("Mean Percent Change of Residential") + 
  xlab("Month") + 
  labs(color="Midwest States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,5), limits=c(-5,20),
                     labels=as.character(seq(-60,60,5)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.res.northeast <- StateMonthMobility_Northeast %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_residential_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Residential in Northeast US")+
  ylab("Mean Percent Change of Residential") + 
  xlab("Month") + 
  labs(color="Northeast States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,5), limits=c(-5,20),
                     labels=as.character(seq(-60,60,5)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.res.south <- StateMonthMobility_South %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_residential_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Residential in South US")+
  ylab("Mean Percent Change of Residential") + 
  xlab("Month") + 
  labs(color="South States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,5), limits=c(-5,20),
                     labels=as.character(seq(-60,60,5)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

mobility.res.west <- StateMonthMobility_West %>% 
  mutate(month = as.Date(paste("2020-", month,"-01", sep=""))) %>% 
  ggplot(aes(y=mean_residential_percent_change, 
             x=month, color=sub_region_1)) +
  geom_point() + 
  geom_line() + 
  geom_path()+
  ggtitle("Mean Percent Change of Residential in West US")+
  ylab("Mean Percent Change of Residential") + 
  xlab("Month") + 
  labs(color="West States") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(expand=c(0,1), breaks=seq(-60,60,5), limits=c(-5,20),
                     labels=as.character(seq(-60,60,5)))+
  theme_minimal() +
  theme(axis.title = element_text(size=12), # change axis style
        legend.title = element_text(size=12), # change legend style
        plot.title = element_text(size=14)) # change title style

# grid.arrange(u1,u2,u3,u4,nrow=2,top="Relationship between Mean Percent Change of Residential and Month by States")




