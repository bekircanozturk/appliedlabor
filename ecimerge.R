setwd("C:/Users/Public/Documents/bekircan gonzalo/eciraz")
library(haven)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
folder <- "C:/Users/Public/Documents/bekircan gonzalo/eciraz"
files <- list.files(folder, pattern = "\\.sas7bdat$", full.names = TRUE)

alldata <- files %>%
  lapply(read_sas) %>%
  bind_rows()

cleandata <- alldata %>%
  distinct()


colstomode <- c("cap", "capx", "tppa", "tppre", "oscd", "tdepa", "tdepre", "oscde", "tdl", "tsk", "ossk","tpxpa", "vpxpa", "tpxpre", "vpxpre", "tpepa","vpepa","compnat", "compue", "compext")
cleandata <-cleandata %>%
  mutate(across(all_of(colstomode),
                ~ na_if(trimws(.x), "")))

mode1 <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

modes <- cleandata %>%
  filter(type==2) %>%
  group_by(siret, datenq) %>%
  summarise(across(all_of(colstomode), mode1, .names = "{.col}_mode"),.groups="drop")

cleandata <- cleandata %>%
  left_join(modes, by = c("siret", "datenq")) %>%
  mutate(across(all_of(colstomode),
                ~ if_else(type==1 & is.na(.x),
                          get(paste0(cur_column(), "_mode")),
                          .x)))%>%
  select(-ends_with("_mode")) %>%
  filter(type==1)

cleandata <- cleandata %>%
  mutate(datenq=as.integer(datenq),
         year=datenq %/% 100,
         month = datenq %%100,
         quarter = ((month-1)%/% 3) + 1L) %>%
  select(-datenq,-month)
cleandata <- cleandata %>%
  relocate(year, quarter, .after=siret)

charcols <- c("naf2_unq", "pgp", "pge", "pgpx", "pgsal", "gtcom", "gtmo", "gtfi", "gtap",
        "gtau", "gtso", "poproda", "ppdaps", "capro", "ditre", "dios", "dictm", "dicad","tre", "trdt", "txsal",
        "tppa", "tppre", "tdpa", "oscd", "tdepa", "gte", "tdepre", "oscde", "tdl", "tsk", "ossk", "tpxpa",
        "vpxpa", "tpxpre", "vpxpre", "tpepa", "vpepa", "compnat", "compue", "compext")

binarycols <- c("direc")

numcols <- c("ca", "eff","tuc")

cleandata <- cleandata %>% 
  mutate(across(all_of(binarycols),
                ~case_when(
                  . %in% c(1, "1") ~ 1L,
                  . %in% c(2, "2") ~ 0L,
                 . %in% c(9, "9", "", " ", NA) ~ 8L,   
                 )))

cleandatayear <- cleandata %>%
  group_by(siret, year) %>%
  summarise(
    across(all_of(charcols), mode1, .names = "{.col}"),
    across(all_of(binarycols), ~as.integer(any(.x==1, na.rm =TRUE)), .names = "{.col}"),
    across(all_of(numcols), ~mean(.x, na.rm =TRUE), .names = "{.col}"),
    .groups = "drop")

cleandatayear <- cleandatayear %>%
  mutate(siret=as.character(siret), siret = str_pad(siret, 14, pad ="0"), siren = str_sub(siret, 1, 9))

longtenure <- read_csv("C:/Users/Public/Documents/bekircan gonzalo/tenure/longtenure.csv", 
                       col_types = cols(x = col_skip(), totalgt2 = col_skip(), 
                                        totalge4 = col_skip()))
cleandatayear <- cleandatayear %>%
  left_join(longtenure, by = "siren")

cleandatayear <- cleandatayear %>%
  mutate(siret=as.character(siret), siret = str_pad(siret, 14, pad ="0"), siren = str_sub(siret, 1, 9)) %>%
  select(-siret)
cleandatayear <- cleandatayear %>%
  mutate(eci=1) %>%
  select(siren, everything())
saveRDS(cleandatayear, file="mergedeci.rds")
  
clean_entr <- readRDS("C:/Users/Public/Documents/bekircan gonzalo/entreprises/clean_entr.rds")
clean_entr <- clean_entr %>%
  mutate(siren=substr(siret,1,9))

eciwithetab <- clean_entr %>%
  left_join(cleandatayear, by = c("siren", "year")) %>%
  filter(!is.na(eci))

eciwithetab <- eciwithetab %>%
  select(siren,siret,year,direc,everything()) %>%
  arrange(siret,year) %>%
  group_by(siret) %>%
  mutate(direc1=lag(direc,1),
         direc2=lag(direc,2),
         direc1=ifelse(is.na(direc1),8,direc1),
         direc2=ifelse(is.na(direc2),8,direc2)) %>%
  ungroup()

eciwithetab <- eciwithetab %>%
  mutate(nexec=nbpna_sexecs_13+nbpna_sexecs_23,
         ninter=nbpna_sexecs_14+nbpna_sexecs_24,
         nworker=nbpna_sexecs_16+nbpna_sexecs_26,
         pexec=nexec/eff_moy_et,
         pinter=ninter/eff_moy_et,
         pworker=nworker/eff_moy_et) %>%
  select(siren,siret,year,direc, direc1, direc2,everything()) %>%
  select(-starts_with("nbpna"))

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

eciwithetab <- eciwithetab %>%
  mutate(industry=substr(apet,1,2),
         lma=zempt) %>%
  left_join(lmaemp, by=c("industry", "lma", "year"))

eciwithetab <- eciwithetab %>%
  select(siren,siret,year,industry, direc, direc1, direc2,eff_moy_et, excessturnover, netgrowth, loglt, everything()) %>%
  filter(!is.na(eff_moy_et))

rm(alldata, cleandata,modes, longtenure, clean_entr)

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

sum(is.na(eciwithetab$netgrowth))
vars <- c("naf2_unq", "pgp", "pge", "pgpx", "pgsal", "gtcom", "gtmo", "gtfi", "gtap",
              "gtau", "gtso", "poproda", "ppdaps", "capro", "ditre", "dios", "dictm", "dicad","tre", "trdt", "txsal",
              "tppa", "tppre", "tdpa", "oscd", "tdepa", "gte", "tdepre", "oscde", "tdl", "tsk", "ossk", "tpxpa",
              "vpxpa", "tpxpre", "vpxpre", "tpepa", "vpepa", "compnat", "compue", "compext" )
ntotal<- nrow(eciwithetab)
usablecounts <- eciwithetab %>%
  summarise(across(all_of(vars), ~ sum(!(is.na(.) | . %in% c("", " ", "9", 9))))) %>%
  pivot_longer(everything(), names_to="variable", values_to = "n_usable") %>%
  mutate(percentage= round(100*n_usable/ntotal,1))

varstodrop <- strsplit("gtso, vpxpa, vpxpre, tdpa, gtau, gtmo, gte, gtap, dios, gtfi, dictm, dicad", ",\\s*") [[1]]
varstodrop2 <- c("vpepa")
 
eciwithetab <- eciwithetab%>%
  select(-all_of(varstodrop)) %>%
  select(-all_of(varstodrop2)) %>%
  filter(!(direc1==8 | direc2==8)) 

firmavgwage <- eciwithetab %>%
  mutate(avgwage= s_brut/eff_moy_et) %>%
  group_by(year, industry, lma) %>%
  mutate(groupavgwage=mean(avgwage, na.rm=TRUE),
         wageprem=log(avgwage)-log(groupavgwage)) %>%
  ungroup() %>%
  select(siret,year,wageprem)
eciwithetab <- eciwithetab %>%
  left_join(firmavgwage, by=c("siret", "year"))
saveRDS(eciwithetab, file="eciwithetab1.rds")

firmskeep <- eciwithetab %>%
  filter(year %in% 2010:2015) %>%
  group_by(siret) %>%
  summarise(n_years=n_distinct(year)) %>%
  filter(n_years==6)%>%
  pull(siret)
eciwithetab2 <- eciwithetab %>%
  filter(siret %in% firmskeep, year %in% 2010:2015)
saveRDS(eciwithetab2, file="eciwithetab2.rds")

modeldata <-eciwithetab %>%
  drop_na(direc,wageprem,longtenure,loglabor)
write_dta(eciwithetab, "finaldata.dta")
write_dta(modeldata, "finaldata2.dta")
write_dta(eciwithetab2, "finaldata3.dta")

model <- glm(direc ~ wageprem + longtenure+ loglabor, data=modeldata, family=binomial(link=probit), na.action = na.omit)


