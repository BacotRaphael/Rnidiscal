rm(list = ls())
library(tidyverse)
library(readxl)
library(writexl)
library(magrittr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read_excel("REACH_SOM2101_Final-Dataset_JMCNA_Somalia_01112021.xlsx", "Clean_Data")
survey <- read_excel("REACH_SOM2101_Final-Dataset_JMCNA_Somalia_01112021.xlsx", "survey")
choices <- read_excel("REACH_SOM2101_Final-Dataset_JMCNA_Somalia_01112021.xlsx", "choices")

## shelter indicators
## Issue with columns between interveted
# shelter type // CANNOT RETRIEVE ORIGINAL QUESTION FROM DATA, only the other shelter type.
data$any_other_shelter_types_yes %>% str_split(" ") %>% unlist %>% unique
choice.shelter.type <- choices %>% filter(list_name=="shelter_types") %>% pull(name)

col.shelter.type <- colnames(data)[unlist(lapply(data, function(x) choice.shelter.type[1] %in% unique(x)))]
ncol.shelter.type <- which(colnames(data)==col.shelter.type[1])
data %>% filter(!is.na(aid_barriers)) %>% select((ncol.shelter.type-4):(ncol.shelter.type+20)) %>% str
data$aid_barriers %>% table(useNA="always") %>% sort(decreasing = T)

# enclosure issue
data$shelter_enclosure_issue %>% str_split(" ") %>% unlist %>% unique
which(colnames(data)=="shelter_enclosure_issue")
data %>% select(685:715) %>% str
colnames(data)[unlist(lapply(data, function(x) "lack_insulation_cold" %in% unique(x)))]
which(colnames(data)=="aid_denail_yes")

## rename the two wrongly named columns
data <- data %>%
  rename(shelter_enclosure_issue_new = aid_denail_yes, aid_denail_yes = shelter_enclosure_issue) %>% 
  rename(shelter_enclosure_issue = shelter_enclosure_issue_new) %>%
  relocate(shelter_enclosure_issue, .after=706) %>% relocate(aid_denail_yes, .after=643)

data %>% select(706:719) %>% str
data$shelter_enclosure_issue %>% str_split(" ") %>% unlist %>% unique

# shelter damage
which(colnames(data)=="shelter_damage")
data$shelter_damage %>% str_split(" ") %>% unlist %>% unique
colnames(data)[unlist(lapply(data, function(x) "damage_roof" %in% unique(x)))]

which(colnames(data)=="what_do_covid")
data$what_do_covid %>% str_split(" ") %>% unlist %>% unique

# shelter_damage and what_do_covid have been interveted - rename the two wrongly named columns
data <- data %>%
  rename(what_do_covid_new = shelter_damage, shelter_damage = what_do_covid) %>%
  rename(what_do_covid = what_do_covid_new) %>%
  relocate(what_do_covid, .after=1311) %>% relocate(shelter_damage, .after=722)

data %>% select(723:730) %>% str
data$shelter_damage %>% str_split(" ") %>% unlist %>% unique
