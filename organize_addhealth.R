# This script will read in the public wave 4 add health data and extract
# the variables that we need


# Load libraries ----------------------------------------------------------

library(haven)
library(tidyverse)
library(here)
library(ggplot2)

# Add health data ---------------------------------------------------------



addhealth <- read_sav(here("input","addhealth_w4","w4inhome_dvn.sav"))

addhealth <- addhealth %>%
  mutate(bmi=ifelse(H4BMI>197, NA, as.numeric(H4BMI)),
         fast_food=ifelse(H4GH8>=100, NA, H4GH8),
         sweet_drinks=ifelse(H4GH9>=100, NA, H4GH9),
         hh_income=factor(H4EC1, levels=1:12,
                          labels=c("Less than $5000","$5000-9999",
                                   "$10,000-14,999","$15,000-19,999",
                                   "$20,000-24,999","$25,000-29,999",
                                   "$30,000-39,999","$40,000-49,999",
                                   "$50,000-74,999","$75,000-99,999",
                                   "$100,000-149,999","$150,000 or more")),
         education=factor(case_when(
           H4ED2<3 ~ "No HS diploma",
           H4ED2<6 ~ "HS diploma",
           H4ED2==6 ~ "Some college",
           H4ED2<9 ~ "4 yr college degree",
           H4ED2<98 ~ "Grad degree"), 
           levels=c("No HS diploma","HS diploma","Some college",
                    "4 yr college degree","Grad degree")),
         gender=factor(BIO_SEX4, levels=1:2, labels=c("Male","Female")),
         age=2008-H4OD1Y) %>%
  select(bmi, fast_food, sweet_drinks, hh_income, education, gender, age) %>%
  na.omit()

save(addhealth, file=here("output","addhealth.RData"))


# test analysis -----------------------------------------------------------


ggplot(addhealth, aes(x=hh_income, y=bmi))+
  geom_boxplot()+
  coord_flip()

ggplot(addhealth, aes(x=hh_income, y=bmi))+
  geom_boxplot()+
  facet_wrap(~gender)+
  coord_flip()


ggplot(addhealth, aes(x=fast_food, y=bmi))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=FALSE)

ggplot(addhealth, aes(x=sweet_drinks, y=bmi))+
  geom_jitter(alpha=0.1)+
  geom_smooth(method="lm", se=FALSE)

ggplot(addhealth, aes(x=age, y=bmi))+
  geom_jitter(alpha=0.1)+
  geom_smooth(method="lm", se=FALSE)

model1 <- lm(bmi~hh_income+gender+age+education, data=addhealth)
summary(model1)
model2 <- lm(bmi~hh_income+gender+age+education+fast_food+sweet_drinks, 
             data=addhealth)
summary(model2)
model3 <- lm(bmi~hh_income*gender+age+education+fast_food+sweet_drinks, 
             data=addhealth)
summary(model3)
