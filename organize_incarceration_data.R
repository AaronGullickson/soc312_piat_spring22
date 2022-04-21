

# Load libraries ----------------------------------------------------------

library(readr)
library(tidyverse)
library(here)
library(ggplot2)
library(rvest)
library(stringr)
library(tabulizer)


# Read incarceration and homeless data ------------------------------------

incarceration <- read_csv(here("input","misc","incarceration_rates.csv"))
homeless <- read_csv(here("input","misc","homeless_pop.csv"))
homicide <- read_csv(here("input","misc","homicide_rate.csv")) %>%
  rename(homicide_rate=homicideRate2017) %>%
  select(State, homicide_rate)
  

full_data <- full_join(incarceration, homeless) %>%
  full_join(homicide) %>%
  mutate(incarceration_rate=prisonRate*100000, 
         homeless_rate=100000*totalHomeless/Pop) %>%
  rename(state=State) %>%
  select(state, incarceration_rate, homeless_rate, homicide_rate)

# ACS Data ----------------------------------------------------------------

acs <- read_csv(here("input","social_explorer","R13107721_SL040.csv"), 
                skip=1) %>%
  rename(state=Geo_NAME, gini=SE_A14028_001) %>%
  mutate(pct_black=100*(SE_A04001_004/SE_A04001_001),
         pct_college=100*(SE_B12001_004/SE_B12001_001),
         unemp_rate=100*(SE_A17005_003/SE_A17005_001),
         poverty_rate=100*(SE_A13005_002/SE_A13005_001),
         gini=100*gini) %>%
  select(state, pct_black, unemp_rate, pct_college, poverty_rate, gini)

full_data <- full_data %>%
  left_join(acs)

# Urbanization data -------------------------------------------------------

# read it from 2010 data on wikipedia

page = read_html("https://en.wikipedia.org/wiki/Urbanization_in_the_United_States")
my.table = html_node(page, ".wikitable")
urbanicity = html_table(my.table, fill = TRUE)[-1,1:2]
colnames(urbanicity) <- c("state","urbanization")
urbanicity$urbanization <- as.numeric(str_remove(urbanicity$urbanization, "%"))
urbanicity$state <- str_remove(urbanicity$state, "\\[[^\\]]*\\]")

full_data <- full_data %>%
  left_join(urbanicity)

# Party in power data -----------------------------------------------------

state_control <- extract_tables(here("input","misc","Legis_Control_2-2021.pdf"), 
                                pages = 1)[[1]][3:52,c(1,13)]
state_control <- as.data.frame(state_control)
colnames(state_control) <- c("state","party_control")
state_control <- tibble(state_control) %>%
  mutate(
    # Nebraska is missing because the legislature is not partisan, but Gov
    # is republican so put it in the R column
    party_control=ifelse(party_control=="N/A", "Rep", party_control),
    party_control=factor(party_control,
                              levels=c("Dem","Rep","Divided"),
                              labels=c("Democratic","Republican","Split")))

full_data <- full_data %>%
  left_join(state_control)

save(full_data, file=here("output","state_incarceration.RData"))


# Test analysis -----------------------------------------------------------

ggplot(full_data, aes(x=poverty_rate, y=incarceration_rate))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)

ggplot(full_data, aes(x=poverty_rate, y=incarceration_rate, color=party_control))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)


model1 <- lm(incarceration_rate~poverty_rate+party_control, data=full_data)
summary(model1)
model2 <- lm(incarceration_rate~poverty_rate+party_control+homicide_rate, 
             data=full_data)
summary(model2)
model3 <- lm(incarceration_rate~poverty_rate*party_control+homicide_rate, 
             data=full_data)
summary(model3)
