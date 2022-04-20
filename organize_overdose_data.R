# This script will load and clean the CDC wonder data on overdose death rates
# by county and merge with it county level ACS data.


# Load libraries ----------------------------------------------------------

library(here)
library(readr)
library(tidyverse)
library(ggplot2)

# CDC Wonder Data ---------------------------------------------------------

# The constructed file from CDC Wonder is a mess. Its tab-delimited with 
# a bunch of non-machine readable notes at the bottom. Thankfully, with the 
# power of a readr, we should be good.

overdose <- read_tsv(here("input","cdc_wonder", 
                          "overdose_deaths_cdc_wonder_county_1720.txt"),
                     n_max=3721, na = "Unreliable") %>%
  # rename for sanity and just keep the age adjusted rate
  rename(county=County, county_code=`County Code`, year=Year, pop=Population,
         death_rate=`Age Adjusted Rate`) %>%
  # drop them if missing the death rate
  filter(!is.na(death_rate)) %>%
  # Lets take 2020  because it has more observations
  filter(year==2020) %>%
  select(county, county_code, death_rate)

# death rates are per 100,000 population


# ACS County Data ---------------------------------------------------------

acs <- read_csv(here("input","social_explorer","R13107381_SL050.csv"), 
                skip=1) %>%
  rename(county_code=Geo_FIPS, pop_density=SE_A00002_002) %>%
  mutate(pct_nonwhite=100*(1-SE_B04001_003/SE_B04001_001),
         dropout_rate=100*(SE_A12003_002/SE_A12003_001),
         unemp_rate=100*(SE_A17005_003/SE_A17005_001),
         pct_frgn_born=100*(SE_A06001_003/SE_A06001_001),
         poverty_rate=100*(SE_A13005_002/SE_A13005_001)) %>%
  select(county_code, pop_density, pct_nonwhite, dropout_rate, unemp_rate,
         pct_frgn_born, poverty_rate)

# Combine them ------------------------------------------------------------

overdose <- left_join(overdose, acs)

## TODO: add categorical variables for metro status of county and region of
## the country
overdose <- read_csv(here("input","labor_stats",
                      "qcew-county-msa-csa-crosswalk-csv.csv")) %>%
  rename(county_code=`County Code`,msa_type=`MSA Type`) %>%
  mutate(metro=factor(ifelse(is.na(msa_type) | msa_type=="Micro",
                             "Non-Metropolitan","Metropolitan"),
                      levels=c( "Non-Metropolitan","Metropolitan"))) %>%
  select(county_code, metro) %>%
  right_join(overdose) %>%
  select(county, county_code, death_rate, metro, pop_density, pct_nonwhite, 
         dropout_rate, unemp_rate, pct_frgn_born, poverty_rate)

save(overdose, file=here("output","overdose.RData"))

# Some test analysis ------------------------------------------------------

ggplot(overdose, aes(x=poverty_rate, y=death_rate, color=metro))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()

model1 <- lm(death_rate~poverty_rate+metro, data=overdose)
summary(model1)
model2 <- lm(death_rate~poverty_rate*metro, data=overdose)
summary(model1)
model3 <- lm(death_rate~pop_density+pct_nonwhite+dropout_rate+unemp_rate+
              pct_frgn_born+poverty_rate*metro, data=overdose)
summary(model2)
