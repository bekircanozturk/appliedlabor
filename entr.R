library(haven)
library(dplyr)

library(purrr)
install.packages("stringr")
library(stringr)
setwd("C:/Users/Public/Documents/bekircan gonzalo/entreprises")

ent2008 <- read_sas("ent2008_sub.sas7bdat") %>%
  mutate(year = 2008)
ent2008<- ent2008 %>%
  rename_with(~ str_replace(., "NBPNA_SEXECSR", "NBPNA_SEXECS"),
              .cols = contains("NBPNA_SEXECSR"))
ent2009 <- read_sas("ent2009_sub.sas7bdat") %>%
  mutate(year = 2009)
ent2009<- ent2009 %>%
  rename_with(~ str_replace(., "NBPNA_SEXECSR", "NBPNA_SEXECS"),
              .cols = contains("NBPNA_SEXECSR"))
ent2010 <- read_sas("ent2010_sub.sas7bdat") %>%
  mutate(year = 2010)
ent2010 <- ent2010 %>%
    rename_with(~ str_replace(., "NBPNA_SEXECSR", "NBPNA_SEXECS"),
                .cols = contains("NBPNA_SEXECSR"))
ent2011 <- read_sas("ent2011_sub.sas7bdat") %>%
  mutate(year = 2011)
ent2011 <- ent2011 %>%
  rename_with(~ str_replace(., "NBPNA_SEXECSR", "NBPNA_SEXECS"),
              .cols = contains("NBPNA_SEXECSR"))
ent2012 <- read_sas("ent2012_sub.sas7bdat") %>%
  mutate(year = 2012)
ent2012 <- ent2012 %>%
  rename_with(~ str_replace(., "NBPNA_SEXECSR", "NBPNA_SEXECS"),
              .cols = contains("NBPNA_SEXECSR"))
ent2013 <- read_sas("ent2013_sub.sas7bdat") %>%
  mutate(year = 2013)
ent2014 <- read_sas("ent2014_sub.sas7bdat") %>%
  mutate(year = 2014)
ent2015 <- read_sas("ent2015_sub.sas7bdat") %>%
  mutate(year = 2015)

clean_entr <- bind_rows(ent2008,ent2009, ent2010, ent2011, ent2012, ent2013, ent2014, ent2015)
names(clean_entr) <- tolower(names(clean_entr))
saveRDS(clean_entr, "clean_entr.rds")