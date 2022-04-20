# This script will load the CCES 2020 data and extract the data we need
# for the PIAT project

# Load libraries ----------------------------------------------------------

library(here)
library(readr)
library(tidyverse)
library(ggplot2)
library(psych)


# CCES Data ---------------------------------------------------------------

cces <- read_csv(here("input","cces","CES20_Common_OUTPUT_vv.csv.gz"))

cces <- cces %>%
  mutate(conservative=ifelse(CC20_340a==8, NA, CC20_340a),
         party_affil=factor(case_when(
           pid3==1 ~ "Democrat",
           pid3==2 ~ "Republican",
           pid3==3 ~ "Independent",
           pid3==4 ~ "Other",
           TRUE ~ NA_character_),
           levels=c("Democrat","Republican","Independent","Other")),
         relig=factor(case_when(
           religpew==1 & pew_bornagain==1 ~ "Evangelical Protestant",
           religpew==1 & pew_bornagain!=1 ~ "Mainline Protestant",
           religpew==2 ~ "Catholic",
           religpew==3 ~ "Mormon",
           religpew==4 ~ "Eastern Orthodox",
           religpew==5 ~ "Jewish",
           religpew==6 ~ "Muslim",
           religpew==9 | religpew==10 ~ "Atheist/Agnostic",
           religpew==11 ~ "Nothing in particular",
           TRUE ~ "Other Religion"),
           levels=c("Mainline Protestant","Evangelical Protestant","Mormon",
                    "Catholic","Eastern Orthodox","Jewish","Muslim",
                    "Other Religion","Nothing in particular",
                    "Atheist/Agnostic")),
         relig_import=5-pew_religimp,
         relig_attend=ifelse(pew_churatd==7, NA, 7-pew_churatd),
         relig_pray=ifelse(pew_prayer==8, NA, 8-pew_prayer),
         gender=factor(gender, levels=1:2, labels=c("Male","Female")),
         education=factor(educ, 1:6, 
                          labels=c("No HS diploma","HS diploma","Some college",
                                   "AA degree", "Bachelors degree",
                                   "Grad degree")),
         race=factor(race, levels=1:8, 
                     labels=c("White","Black","Latino","Asian",
                              "American Indian","Middle Eastern","Multiracial",
                              "Other")),
         age=2020-birthyr,
         family_income=case_when(
           faminc_new==1 ~ 5,
           faminc_new==2 ~ 15,
           faminc_new==3 ~ 25,
           faminc_new==4 ~ 35,
           faminc_new==5 ~ 45,
           faminc_new==6 ~ 55,
           faminc_new==7 ~ 65,
           faminc_new==8 ~ 75, 
           faminc_new==9 ~ 90, 
           faminc_new==10 ~ 110,
           faminc_new==11 ~ 135,
           faminc_new==12 ~ 175,
           faminc_new==13 ~ 225,
           faminc_new==14 ~ 300,
           faminc_new==15 ~ 425,
           faminc_new==16 ~ 500
         )) %>%
  select(conservative, party_affil, relig, gender, education, race, age,
         family_income, relig_import, relig_attend, relig_pray) %>%
  na.omit()

# test Cronbach's alpha  
alpha(cor(cces[,c("relig_import", "relig_attend", "relig_pray")]))

# that looks pretty good so lets create a summated scale
cces <- cces %>%
  mutate(religiosity=scale(scale(relig_import)+scale(relig_attend)+
                             scale(relig_pray))[,1]) %>%
  select(conservative, party_affil, relig, gender, education, race, age,
         family_income, religiosity)

save(cces, file=here("output","cces.RData"))

# Test analysis -----------------------------------------------------------

ggplot(cces, aes(x=religiosity, y=conservative, color=relig))+
  geom_jitter(alpha=0.05)+
  geom_smooth(method="lm", se=FALSE)

ggplot(cces, aes(x=factor(conservative), y=religiosity))+
  geom_boxplot()+
  facet_wrap(~relig)

model1 <- lm(conservative~religiosity, data=cces)
summary(model1)
model2 <- lm(conservative~religiosity+relig, data=cces)
summary(model2)
model3 <- lm(conservative~religiosity*relig, data=cces)
summary(model3)
model4 <- lm(conservative~religiosity*relig+age+race+gender+education+
               family_income, data=cces)
summary(model4)

model5 <- lm(conservative~religiosity*education+relig, data=cces)
summary(model5)
