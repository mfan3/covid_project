### SVI and unemployment data cleaning

library(tidyverse)
library(dplyr) 

# selecting relevant indices (refer to codebook)
svi.raw <- read_csv("SVI2018_US_COUNTY.csv")
svi.raw %>% 
  select(STATE,
         COUNTY,
         FIPS,
         AREA_SQMI,
         E_TOTPOP,
         E_HU,
         E_HH,
         E_POV,
         E_UNEMP,
         E_PCI,
         E_NOHSDP,
         E_AGE65,
         E_DISABL,
         E_MINRTY,
         E_LIMENG,
         E_MOBILE,
         E_CROWD,
         E_NOVEH,
         E_GROUPQ,
         E_UNINSUR) -> svi.data

# -999 means unavailable data so replaced with 0
svi.selected <- svi.data
svi.selected[svi.selected == "-999"] <- NA
# only New Mexico has NA responses for est. PCI, poverty, and unemployment

# further cleaning svi data and calculating total amount of variables
vi.selected %>% 
  # PCI is per capita income 
  # calculating total income to derive statewide per capita income later 
  mutate(income = E_TOTPOP * E_PCI) %>% 
  group_by(STATE) %>% 
  summarise(area = sum(AREA_SQMI),
            # aggregating to state level
            total.population = sum(E_TOTPOP),
            total.housing.units = sum(E_HU),
            total.household = sum(E_HH),
            total.poverty = sum(E_POV),
            total.unemployed = sum(E_UNEMP),
            total.income = sum(income),
            # dividing into statewide per capita
            total.capita = total.income/total.population,
            total.no.HS = sum(E_NOHSDP),
            total.age65 = sum(E_AGE65),
            total.disabled = sum(E_DISABL),
            total.minority = sum(E_MINRTY),
            total.eng.proficiency = sum(E_LIMENG),
            total.mobile = sum(E_MOBILE),
            total.crowd = sum(E_CROWD),
            total.no.vehicle = sum(E_NOVEH),
            total.institutionalized = sum(E_GROUPQ),
            total.uninsured = sum(E_UNINSUR)) -> svi.state


# further cleaning svi data and calculating percentages of variables
svi.state %>%
  mutate(population.density = total.population/area,
         poverty.rate = total.poverty/total.population,
         unemployed.rate = total.unemployed/total.population,
         per.capita = total.capita,
         no.HS.rate = total.no.HS/total.population,
         age65.rate = total.age65/total.population,
         disabled.rate = total.disabled/total.population,
         minority.rate = total.minority/total.population,
         poor.English.rate = total.eng.proficiency/total.population,
         mobile.home.rate = total.mobile/total.population,
         household.crowd.rate = total.crowd/total.population,
         no.vehicle.rate = total.no.vehicle/total.population,
         institutionalized.rate = total.institutionalized/total.population,
         uninsured.rate = total.uninsured/total.population) -> svi.state


# Data cleaning for unemployment data
unemployment.2020.raw <- read_csv("unemployment2020.csv")
unemployment.2020.data <- 
  unemployment.2020.raw[,colSums(is.na(unemployment.2020.raw))<nrow(unemployment.2020.raw)]
unemployment.2020 <- na.omit(unemployment.2020.data)
svi.state %>%
  select(STATE, unemployed.rate) -> unemployment.2018

svi.state %>%
  select(-unemployed.rate) -> svi.state

svi.state$STATE <- str_to_title(svi.state$STATE[1:51])


# combining unemployment data
unemployment <- cbind(unemployment.2018, unemployment.2020) %>% 
  mutate(unemployed.rate = round(unemployed.rate * 100, digit = 1)) %>% 
  rename(unemployment.rate.2018 = unemployed.rate) %>% 
  select(-c(STATE)) %>% # select everything but 
  rename(state = State,
         Jan = Jan.,
         Feb = Feb.,
         Mar = March,
         Apr = April,
         May = May,
         Jun = June,
         Jul = July,
         Aug = Aug.,
         Sep = Sept.) %>%
  pivot_longer(-c(state, unemployment.rate.2018),
               names_to = "month",
               values_to = "unemployment.rate.2020") %>% 
  mutate(month = match(month, month.abb))

unemployments <- unemployment[c(2, 3, 1, 4)]
