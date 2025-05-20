setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven)
library(dplyr)
library(janitor)
library(readr)
pts0821 <-read_sas(("ptsclean.sas7bdat"))



longtenure <- pts0821 %>%
  filter(ANCSIR > 2) %>%
  group_by(SIR) %>%
  summarise(
  totalgt2 = n(),
  totalge4 = sum(ANCSIR >= 4),
  longtenure = totalge4/totalgt2,
  .groups = "drop"
)

write.csv(longtenure, "longtenure.csv")
