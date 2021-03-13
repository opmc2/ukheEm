## -----------------------------------------------------------------------------
## mcs.R
##
## Project: UKHeEm (+UKHeExp)
## Purpose: Clean and save data from Millennium Cohort Study (MCS)
## Author: Oliver Cassagneau-Francis
## Date: Tue Jan 19 12:29:13 2021
## -----------------------------------------------------------------------------
## Notes:

# ---- Setup ----

# required packages
library(data.table)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(foreign)
library(here)

# ---- Import data ----

# data directory
dataDir <- "data-raw/MCS"

filePathsMcs7 <- list.files(
  here(dataDir),
  pattern = "mcs7",
  recursive = TRUE, full.names = TRUE
)

listMcs7 <- list()

for (i in seq_along(filePathsMcs7)) {
  listMcs7[[i]] <- read.spss(
    filePathsMcs7[[i]],
    to.data.frame = TRUE, use.missings = FALSE,
    add.undeclared.levels = "no",
    duplicated.value.labels = "condense",
    trim.factor.names = TRUE
  )
}

# name datafiles in MCS7
names(listMcs7) <- str_extract(filePathsMcs7, "(?<=mcs7_)[A-Za-z_]*(?=.sav)")

# extract variable labels

listAttributes <- list()
listVarLabels <- list()

listAttributes <- lapply(listMcs7, attributes)

for (i in seq_along(listMcs7)) {
  listVarLabels[[i]] <- data.table(
    varNames = listAttributes[[i]]$names,
    varLabels = listAttributes[[i]]$variable.labels
  )
}

names(listAttributes) <- names(listMcs7)

# save mcs7 data
mcs7 <- lapply(listMcs7, as.data.table)
usethis::use_data(mcs7, overwrite = TRUE)

mcs7Labels <- lapply(listVarLabels, as.data.table)
usethis::use_data(mcs7Labels, overwrite = TRUE)
