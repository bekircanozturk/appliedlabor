install.packages("lubridate")
library(haven)
library(dplyr)
library(purrr)
library(stringr)
setwd("C:/Users/Public/Documents/bekircan gonzalo/firmyear")


entre2010 <- read_sas("entre2010_sub.sas7bdat") 

entre2011 <- read_sas("entre2011_sub.sas7bdat") 

entre2012 <- read_sas("entre2012_sub.sas7bdat")

entre2013 <- read_sas("entre2013_sub.sas7bdat") 
entre2014 <- read_sas("entre2014_sub.sas7bdat")
entre2015 <- read_sas("entre2015_sub.sas7bdat")

firmage <- bind_rows(entre2010, entre2011, entre2012, entre2013, entre2014, entre2015)
names(firmage) <- tolower(names(firmage))

firmage <- firmage %>%
  arrange(siren, desc(!is.na(dat_crea))) %>%
  distinct(siren, .keep_all = TRUE) %>%
  mutate(creation=year(ymd(dat_crea))) %>%
  select(-dat_crea, -year)
saveRDS(firmage, "firmage.rds")
