### Google mobility data cleaning 

# Importing Google mobility dataset
US_Region_2020_mobility <- read.csv( "2020_US_Region_Mobility_Report.csv", header=T, na.strings=c("","NA"))

# Cleaning mobility dataset
US_State_2020_mobility<- US_Region_2020_mobility %>% filter(sub_region_1 != "Null")
US_State_2020_mobility_without_county <- US_State_2020_mobility %>% filter(iso_3166_2_code != "Null")

unique(US_State_2020_mobility_without_county$sub_region_1)
US_State_2020_mobility_without_county$date <- as.Date(US_State_2020_mobility_without_county$date, "%m/%d/%y")


month <- as.numeric(format(US_State_2020_mobility_without_county$date, "%m"))

StateMonthMobility <- US_State_2020_mobility_without_county %>% mutate(month = month)


StateMonthmobility <- StateMonthMobility %>% filter(retail_and_recreation_percent_change_from_baseline != "NA")%>% 
  filter(grocery_and_pharmacy_percent_change_from_baseline != "NA")%>%
  filter(parks_percent_change_from_baseline != "NA")%>%
  filter(transit_stations_percent_change_from_baseline!= "NA")%>% 
  filter(workplaces_percent_change_from_baseline!= "NA")%>% 
  filter(residential_percent_change_from_baseline!= "NA")

StateMonthMobility2 <- StateMonthMobility %>% 
  group_by(month,sub_region_1) %>% 
  summarise(mean_retial_and_recreation_percent_change = 
              mean(retail_and_recreation_percent_change_from_baseline),
            mean_grocery_and_pharmacy_percent_change = 
              mean(grocery_and_pharmacy_percent_change_from_baseline), 
            mean_parks_percent_change = mean(parks_percent_change_from_baseline),
            mean_transit_stations_percent_change = 
              mean(transit_stations_percent_change_from_baseline),
            mean_workplaces_percent_change = 
              mean(workplaces_percent_change_from_baseline),
            mean_residential_percent_change= 
              mean(residential_percent_change_from_baseline))
