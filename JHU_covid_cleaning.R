### JHU covid data cleaning 

# Exclude these states
# American Samoa, Diamond Princess, Grand Princess, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands
covid.death <- covid.death.raw %>% 
  select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Country_Region, Lat, Long_, Combined_Key)) %>%
  rename(state = Province_State, population = Population) %>% 
  group_by(state) %>% # Specify group indicator
  summarise(across(everything(), sum)) %>% # Specify column
  pivot_longer(-c(state, population),
               names_to = "date",
               values_to = "cumulative.death.cases") %>% 
  filter(state %in% c(state.name, "District of Columbia")) %>% 
  group_by(state) %>% 
  mutate(death.cases = cumulative.death.cases - lag(cumulative.death.cases),
         date = as.Date(date, format = "%m/%d/%y"), 
         month = match(months(date), month.name),
         death.cases = replace_na(death.cases, 0))

covid.deaths <- covid.death[c(1, 2, 3, 6, 4, 5)]


# loading JHU covid confirmed historical dateset

covid.confirmed.raw <- read_csv("./csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
# glimpse(covid.confirmed.raw)

# cleaning JHU covid confirmed historical dateset
covid.confirmed <- covid.confirmed.raw %>% 
  select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Country_Region, Lat, Long_, Combined_Key)) %>%
  rename(state = Province_State) %>% 
  group_by(state) %>% # Specify group indicator
  summarise(across(everything(), sum)) %>% # Specify column
  pivot_longer(-c(state),
               names_to = "date",
               values_to = "cumulative.confirmed.cases") %>% 
  filter(state %in% c(state.name, "District of Columbia")) %>% 
  group_by(state) %>% 
  mutate(confirmed.cases = cumulative.confirmed.cases - lag(cumulative.confirmed.cases),
         date = as.Date(date, format = "%m/%d/%y"), 
         confirmed.cases = replace_na(confirmed.cases, 0))


# combining death and confirmed into one dataset
covid <- covid.deaths %>% inner_join(covid.confirmed, by = c("state","date"))

dates <- format(seq(from = as.Date("2020/04/13"), to = as.Date("2020/10/24"), by = "day"), "%m-%d-%Y")
date = "04-12-2020"
reports.raw <- read_csv(paste("./csse_covid_19_daily_reports_us/", date, ".csv", sep = ""))
report <- reports.raw %>%
  select(-c(Country_Region, Last_Update, Lat, Long_, Confirmed, Deaths, FIPS, UID, ISO3)) %>% 
  rename(state = Province_State,
         recovered.cases = Recovered,
         active.cases = Active,
         incident.rate = Incident_Rate, # positive results per 100,000 persons.
         people.tested = People_Tested,
         people.hospitalized = People_Hospitalized,
         mortality.rate = Mortality_Rate,
         testing.rate = Testing_Rate,
         hospitalization.rate = Hospitalization_Rate) %>% 
  filter(state %in% c(state.name, "District of Columbia")) %>% 
  mutate(date = as.Date(date, format = "%m-%d-%y"),
         # month = match(months(date), month.name),
         incident.rate = incident.rate/1000,
         # based on documentation (100k population)
         testing.rate = testing.rate/1000) %>%
  select(-c(people.hospitalized, hospitalization.rate, recovered.cases))



for (date in dates){
  reports.raw <- read_csv(paste("./csse_covid_19_daily_reports_us/", date, ".csv", sep = ""))
  rpt <- reports.raw %>%
    select(-c(Country_Region, Last_Update, Lat, Long_, Confirmed, Deaths, FIPS, UID, ISO3)) %>% 
    rename(state = Province_State,
           recovered.cases = Recovered,
           active.cases = Active,
           incident.rate = Incident_Rate,
           people.tested = People_Tested,
           people.hospitalized = People_Hospitalized,
           mortality.rate = Mortality_Rate,
           testing.rate = Testing_Rate,
           hospitalization.rate = Hospitalization_Rate) %>% 
    filter(state %in% c(state.name, "District of Columbia")) %>% 
    mutate(date = as.Date(date, format = "%m-%d-%y"),
           # month = match(months(date), month.name),
           incident.rate = incident.rate/1000,
           testing.rate = testing.rate/1000) %>%
    select(-c(people.hospitalized, hospitalization.rate, recovered.cases))
  ### North Dekota has > 100% testing rate 
  report <- rbind(report, rpt)
}

reports <- report[c(1, 6, 7, 2, 3, 4, 5)][order(report$state, report$date),]

covid.time.series <- covid %>% full_join(reports, by = c("state","date"))

# covid.time.series <- reports
# View(covid.time.series)
#write.csv(covid.time.series, "E:/UCSD/2020-2021 Senior/PSYC 201 Project/CJHU Databse/covid time series version 1.csv")


# Looping through dates
