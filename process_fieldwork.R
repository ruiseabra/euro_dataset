## this script does all the post-fieldwork processing of robolimpet-related files
#########################-
## checks for unprocessed robolimpet files, moves the originals into a subfolder and a processed copy is left in the original folder (processed files all have the same date and temperature format, making it easier to import later down the pipeline)
## collects from the "robolimpets@work" all info related to which loggers were deployed where, and when (start, end, and microhabitat info)
## identifies if calibration should be applied, and if so what run of calibration
## updates the robolimpet servicing dates file (for future fieldwork planning)

rm(list=ls())
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(xts))
Sys.setenv(TZ = "UTC")

## id the user and set the appropriate paths ####
path <- getwd() %>% dirname
paths <- list(files = str_c(path, "/robolimpet_reports"))
paths$calibReadings     <- str_c(path, "/calibration/calibration_data") %>% dir(pattern = "robolimpet.readings", full.names = TRUE) %>% last
paths$calibTemperatures <- str_c(path, "/calibration/calibration_data") %>% dir(pattern = "reference.temperatures", full.names = TRUE) %>% last
paths$atWork            <- str_c(path, "/logger_info") %>% dir(pattern = "robolimpets@work", full.names = TRUE) %>% last
paths$servDates <- str_c(path, "/logger_info/robolimpet_servicing_dates.RData")
paths$roboInfo  <- str_c(path, "/logger_info/robolimpet_info.RData")

## standardize robolimpet files ####
source("1_standardize_robolimpet_files.R")

## grab info from the "robolimpets@work" file ####
## and update the robolimpet-servicing-dates file
source("2_grab_logger_info.R")
