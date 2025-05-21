# ─────────────────────────────────────────────────────────────────────────────
# 1.  Set working directory and load libraries
#     • haven   : read/write SAS & Stata files
#     • dplyr   : data-manipulation verbs
#     • stringr : string helpers (str_pad, substr, etc.)
#     • readr   : fast CSV I/O
#     • tidyr   : pivot / reshape tools
# ─────────────────────────────────────────────────────────────────────────────

setwd("C:/Users/Public/Documents/bekircan gonzalo/eciraz")
library(haven)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# ─────────────────────────────────────────────────────────────────────────────
# 2.  Read all quarterly ECI extracts (*.sas7bdat) into one big tibble
#     • list.files() grabs paths
#     • lapply(read_sas) imports each file
#     • bind_rows()     stacks them
#     • distinct()      drops exact duplicates
#     • filter(type==1) removes observations for products
# ─────────────────────────────────────────────────────────────────────────────

folder <- "C:/Users/Public/Documents/bekircan gonzalo/eciraz"
files <- list.files(folder, pattern = "\\.sas7bdat$", full.names = TRUE)

alldata <- files %>%
  lapply(read_sas) %>%
  bind_rows()

cleandata <- alldata %>%
  distinct() %>%
  filter(type==1)
# ─────────────────────────────────────────────────────────────────────────────
# 4.  Parse date field ‘datenq’
#     • Convert yyyymm to integer, then derive ‘year’, ‘month’, ‘quarter’
#     • Drop raw ‘datenq’ & ‘month’; relocate year/quarter next to siret
# ─────────────────────────────────────────────────────────────────────────────
cleandata <- cleandata %>%
  mutate(datenq=as.integer(datenq),
         year=datenq %/% 100,
         month = datenq %%100,
         quarter = ((month-1)%/% 3) + 1L) %>%
  select(-datenq,-month)

cleandata <- cleandata %>%
  relocate(year, quarter, .after=siret)
# ─────────────────────────────────────────────────────────────────────────────
# 5.  Re-code binary survey variable ‘direc’
#     1 / "1" → 1  (difficulty reported)
#     2 / "2" → 0  (no difficulty)
#     9 / blanks → 8 (special “missing / don’t know” code)
# ─────────────────────────────────────────────────────────────────────────────

binarycols <- c("direc")

numcols <- c("ca", "eff","tuc")

cleandata <- cleandata %>% 
  mutate(across(all_of(binarycols),
                ~case_when(
                  . %in% c(1, "1") ~ 1L,
                  . %in% c(2, "2") ~ 0L,
                  . %in% c(9, "9", "", " ", NA) ~ 8L,   
                )))
# ─────────────────────────────────────────────────────────────────────────────
# 6.  Collapse quarterly data to annual (siret, year)
#     • ‘direc’ becomes 1 if any quarter flagged a difficulty
#     • Numeric flows (ca, eff, tuc) averaged over quarters, only eff is used in final analysis
# ─────────────────────────────────────────────────────────────────────────────

cleandatayear <- cleandata %>%
  group_by(siret, year) %>%
  summarise(
    across(all_of(binarycols), ~as.integer(any(.x==1, na.rm =TRUE)), .names = "{.col}"),
    across(all_of(numcols), ~mean(.x, na.rm =TRUE), .names = "{.col}"),
    .groups = "drop")

cleandatayear <- cleandatayear %>%
  mutate(siret=as.character(siret), siret = str_pad(siret, 14, pad ="0"), siren = str_sub(siret, 1, 9))
# ─────────────────────────────────────────────────────────────────────────────
# 7.  Merge firm-level long-tenure ratio (≥4 yr / >2 yr) from CSV
# ─────────────────────────────────────────────────────────────────────────────

longtenure <- read_csv("C:/Users/Public/Documents/bekircan gonzalo/tenure/longtenure.csv", 
                       col_types = cols(x = col_skip(), totalgt2 = col_skip(), 
                                        totalge4 = col_skip()))
cleandatayear <- cleandatayear %>%
  left_join(longtenure, by = "siren")

cleandatayear <- cleandatayear %>%
  mutate(siret=as.character(siret), siret = str_pad(siret, 14, pad ="0"), siren = str_sub(siret, 1, 9)) %>%
  select(-siret)

# ─────────────────────────────────────────────────────────────────────────────
# 8.  Mark enterprises observed in ECI (eci = 1) and save checkpoint
# ─────────────────────────────────────────────────────────────────────────────

cleandatayear <- cleandatayear %>%
  mutate(eci=1) %>%
  select(siren, everything())
saveRDS(cleandatayear, file="mergedeci.rds")

# ─────────────────────────────────────────────────────────────────────────────
# 9.  Bring in BTS-Entreprises panel (‘clean_entr’) and keep rows that
#     match an ECI observation → table ‘eciwithetab’
# ─────────────────────────────────────────────────────────────────────────────

clean_entr <- readRDS("C:/Users/Public/Documents/bekircan gonzalo/entreprises/clean_entr.rds")
clean_entr <- clean_entr %>%
  mutate(siren=substr(siret,1,9))

eciwithetab <- clean_entr %>%
  left_join(cleandatayear, by = c("siren", "year")) %>%
  filter(!is.na(eci))
# ─────────────────────────────────────────────────────────────────────────────
# 10. Build lagged difficulty dummies within establishments
#     • direc1 = previous year, direc2 = two years prior
#     • Missing lags coded as 8  ➜ later filtered out
# ─────────────────────────────────────────────────────────────────────────────

eciwithetab <- eciwithetab %>%
  select(siren,siret,year,direc,everything()) %>%
  arrange(siret,year) %>%
  group_by(siret) %>%
  mutate(direc1=lag(direc,1),
         direc2=lag(direc,2),
         direc1=ifelse(is.na(direc1),8,direc1),
         direc2=ifelse(is.na(direc2),8,direc2)) %>%
  ungroup()
# ─────────────────────────────────────────────────────────────────────────────
# 11. Worker-composition shares
#     nexec  = managers (CSC 13 + 23) 13+23 because bts etab. includes genders
#     ninter = technicians (14 + 24)
#     nworker= blue-collar (16 + 26)
#     p*     = share of group in total employment (eff_moy_et)
# ─────────────────────────────────────────────────────────────────────────────
eciwithetab <- eciwithetab %>%
  mutate(nexec=nbpna_sexecs_13+nbpna_sexecs_23,
         ninter=nbpna_sexecs_14+nbpna_sexecs_24,
         nworker=nbpna_sexecs_16+nbpna_sexecs_26,
         pexec=nexec/eff_moy_et,
         pinter=ninter/eff_moy_et,
         pworker=nworker/eff_moy_et) %>%
  select(siren,siret,year,direc, direc1, direc2,everything()) %>%
  select(-starts_with("nbpna"))

# ─────────────────────────────────────────────────────────────────────────────
# 12. Build local-labour-market (LMA × industry × year) metrics
#     • l_t, l_tm1 : employment stock and its lag
#     • acc / sep  : accessions & separations (gross flows)
#     • netgrowth and loglt as in Fabling & Maré (2016), excessturnover is dropped
# ─────────────────────────────────────────────────────────────────────────────

lmaemp <- clean_entr %>%
  filter(!is.na(zempt), zempt!="") %>%
  mutate(industry=substr(apet,1,2),
         l_t=eff_moy_et,
         lma=zempt) %>%
  group_by(lma, industry, year)%>%
  summarise(l_t=sum(l_t, na.rm = TRUE),
            .groups = "drop")  

lmaemp <- lmaemp %>%
  arrange(industry, lma,year) %>%
  group_by(industry,lma) %>%
  mutate(l_tm1=lag(l_t)) %>%
  ungroup() %>%
  filter(!is.na(l_tm1))

lmaemp <- lmaemp %>%
  mutate(acc= pmax(l_t-l_tm1,0),
         sep=pmax(l_tm1-l_t,0),
         excessturnover=2*pmin(acc,sep)/(l_tm1+l_t),
         netgrowth=2*(acc-sep)/(l_tm1+l_t),
         loglt=log((l_tm1+l_t)/2)) %>%
  select(industry, lma, year, excessturnover, netgrowth, loglt)
# ─────────────────────────────────────────────────────────────────────────────
# 13. Join LMA metrics back to establishment panel, change ordering in dataset, then drop observations without eff_moy_et
# ─────────────────────────────────────────────────────────────────────────────

eciwithetab <- eciwithetab %>%
  mutate(industry=substr(apet,1,2),
         lma=zempt) %>%
  left_join(lmaemp, by=c("industry", "lma", "year"))

eciwithetab <- eciwithetab %>%
  select(siren,siret,year,industry, direc, direc1, direc2,eff_moy_et, excessturnover, netgrowth, loglt, everything()) %>%
  filter(!is.na(eff_moy_et))
# ─────────────────────────────────────────────────────────────────────────────
# 14. Add firm-age (current year – creation year) and prune:
#     • drop negative ages, 2008-09 observations, and rows missing key
#       covariates (longtenure, excessturnover, netgrowth, loglt)
#     • loglabor = log(1 + eff_moy_et)  (firm size)
# ─────────────────────────────────────────────────────────────────────────────

firmage <- readRDS("C:/Users/Public/Documents/bekircan gonzalo/firmyear/firmage.rds")
eciwithetab <- eciwithetab %>%
  left_join(firmage, by = "siren")

eciwithetab <- eciwithetab %>%
  mutate(firmage=year-creation) %>%
  filter(firmage >= 0) %>%
  filter(!year %in% c(2008,2009)) %>%
  filter(!is.na(longtenure), !is.na(excessturnover),!is.na(loglt),!is.na(netgrowth)) %>%
  mutate(loglabor =log1p(eff_moy_et)) %>%
  select(-eci,-creation)

saveRDS(eciwithetab, file="eciwithetab.rds")

eciwithetab <- eciwithetab%>%
  filter(!(direc1==8 | direc2==8)) 
# ─────────────────────────────────────────────────────────────────────────────
# 15. Wage premium
#     • avgwage      = wage bill / employment
#     • groupavgwage = mean within (year, industry, LMA)
#     • wageprem     = log(avgwage) – log(groupavgwage)
# ─────────────────────────────────────────────────────────────────────────────
firmavgwage <- eciwithetab %>%
  mutate(avgwage= s_brut/eff_moy_et) %>%
  group_by(year, industry, lma) %>%
  mutate(groupavgwage=mean(avgwage, na.rm=TRUE),
         wageprem=log(avgwage)-log(groupavgwage)) %>%
  ungroup() %>%
  select(siret,year,wageprem)
# ─────────────────────────────────────────────────────────────────────────────
# 16. Final export
#     • Save RDS snapshots
#     • Write Stata .dta file ‘finaldata.dta’ for probit estimation
# ─────────────────────────────────────────────────────────────────────────────
eciwithetab <- eciwithetab %>%
  left_join(firmavgwage, by=c("siret", "year"))
saveRDS(eciwithetab, file="eciwithetab1.rds")
write_dta(eciwithetab, "finaldata.dta")


