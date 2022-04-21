# This script will read in and organize the GSS data exctract


# Load libraries ----------------------------------------------------------

library(here)
library(readr)
library(tidyverse)
library(ggplot2)


# Read GSS Data -----------------------------------------------------------

gss <- read_fwf(here("input","gss","GSS.dat"),
                col_positions = 
                  fwf_widths(widths = c(7,7,7,7,7,7,7,7,7,7,7),
                                col_names = c("sex_orient","age","educ",
                                              "paeduc","maeduc","gender",
                                              "reg16","family16","famdif16",
                                              "relig16","fund16")),
                col_types = cols(.default = "i"))

gss <- gss %>%
  mutate(sex_orient=factor(case_when(
    sex_orient==3 ~ "Heterosexual",
    sex_orient==1 | sex_orient==2 ~ "Gay/Lesbian/Bisexual"),
    levels=c("Heterosexual","Gay/Lesbian/Bisexual")),
    age=ifelse(age<18, NA, age),
    noparents16=as.numeric((family16==0 | family16==8) & famdif16==5),
    gender=factor(gender, levels=1:2, labels=c("Male","Female")),
    reg16=factor(case_when(
      reg16==1 | reg16==2 ~ "Northeast",
      reg16==3 | reg16==4 ~ "Midwest",
      reg16>=5 & reg16<=7 ~ "South",
      reg16>=8 ~ "West")),
    parent_educ = max(maeduc, paeduc),
    parent_educ = ifelse(parent_educ<0, NA, parent_educ),
    raised_fund = factor(case_when(
      fund16==1 ~ "Yes",
      fund16==2 | fund16==3 ~ "No"),
      levels=c("No","Yes"))
  ) %>%
  # no dat on family sit for 2021
  filter(family16>=0) %>%
  select(noparents16, sex_orient, age, gender, reg16, parent_educ, raised_fund) %>%
  na.omit()

save(gss, file=here("output","gss.RData"))

# Test analysis -----------------------------------------------------------

model1 <- lm(noparents16~sex_orient+raised_fund, data=gss)  
summary(model1)
model2 <- lm(noparents16~sex_orient*raised_fund, data=gss)  
summary(model2)
model3 <- lm(noparents16~sex_orient+reg16, data=gss)  
summary(model3)
model4 <- lm(noparents16~sex_orient*reg16, data=gss)  
summary(model4)
