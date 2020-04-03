library(openxlsx)
library(tidyverse)
library(lubridate)
library(digest)
library(forecast)
library(data.table)
rm(list = ls())
source("Rfunctions.R")

# PARAMS
files = list()
files$bloodAnalysis = "data/template - blood analysis.xlsx"
files$treatments = "data/template - treatment.xlsx"
files$clinicalObservations = "data/template - clinical_observartions.xlsx"


numberOfPatients = 50
maxNumberOfDays = 30


# TABLE BLOOD ANALYSIS
tableTemplateValues = read.xlsx(files$bloodAnalysis, skipEmptyRows = TRUE) %>%
  as_tibble() %>%
  mutate(value = as.numeric(value)) %>%
  mutate(normal_min = str_extract(range_normal, pattern = "(\\([[:digit:]]+(\\.[[:digit:]])*)|(Inf)|(-Inf)") %>% str_remove("(\\()") %>% as.numeric()) %>%
  mutate(normal_max = str_extract(range_normal, pattern = "[[:digit:]]+(\\.[[:digit:]])*\\)|(Inf)|(-Inf)") %>% str_remove("(\\))") %>% as.numeric()) %>%
  mutate(patientID = NA, datetime = Sys.time(), parameter, value, unit, normal_min, normal_max) %>%
  select(patientID, datetime, parameter, value, unit, normal_min, normal_max)


# Create dummy data blood analysis
tablePatientsAndDates = createTablePatientsAndDates(numberOfPatients = numberOfPatients,
                                                    maxNumberOfDays = maxNumberOfDays,
                                                    tableTemplateValues = tableTemplateValues)
