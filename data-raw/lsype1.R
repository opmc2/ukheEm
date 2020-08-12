# code to import and clean the next steps (LSYPE1) data

# Notes: the young person data files cover waves 1-6, and the parental attitudes
# files cover waves 1-4. There are also family background files that cover waves
# 1-5. There are then two files which refer to wave 7 named:
# - `lyspe_w7_nov2011_suppressed.sav`
# - `lyspe_w4-7_nov2011_suppressed.sav`
# The survey begain in 2004 and was annual until wave 7, when there was a 4 year
# break until 2015 when the students were re-interviewed (at age 25) for wave 8.

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
dataDir <- "data-raw/Lsype1/"

# ------------------------------- #
# ---- Young person data files ----
# ------------------------------- #

filePathsYP <- list.files(
  here(dataDir),
  pattern = "young_person", recursive = TRUE, full.names = TRUE
)

listYP <- list()

for (i in seq_along(filePathsYP)) {
  listYP[[i]] <- read.spss(
    filePathsYP[[i]],
    to.data.frame = TRUE, add.undeclared.levels = "no",
    duplicated.value.labels = "condense",
    trim.factor.names = TRUE
  )
}

# ---- Wave 7 ----
# November 2011

listYP[[7]] <- read.spss(
  here(dataDir ,"lsype_w7_nov2011_suppressed.sav"),
  to.data.frame = TRUE, add.undeclared.levels = "no",
  duplicated.value.labels = "condense",
  trim.factor.names = TRUE
)

# extract variable labels

listAttributes <- list()
listVarLabels <- list()

listAttributes <- lapply(listYP, attributes)

for (i in seq_along(listYP)) {
  listVarLabels[[i]] <- data.table(
    varNames = listAttributes[[i]]$names,
    varLabels = listAttributes[[i]]$variable.labels
  )
}

# ---- Wave 8 data: 2015 (age 25) ----

wave8Main <- read.spss(
  here(dataDir, "ns8_2015_main_interview.sav"),
  to.data.frame = TRUE, add.undeclared.levels = "no",
  duplicated.value.labels = "condense",
  trim.factor.names = TRUE
)

wave8DV <- read.spss(
  here(dataDir, "ns8_2015_derived.sav"),
  to.data.frame = TRUE, add.undeclared.levels = "no",
  duplicated.value.labels = "condense",
  trim.factor.names = TRUE
)

# extract variable labels and add to list

listVarLabels[[8]] <- rbindlist(list(
  data.table(
    varNames = attributes(wave8Main)$names,
    varLabels = attributes(wave8Main)$variable.labels
  ),
  data.table(
    varNames = attributes(wave8DV)$names,
    varLabels = attributes(wave8DV)$variable.labels
  )
))

listYP[[8]] <- left_join(wave8Main, wave8DV, by = "NSID")

# save YP files in /data
lsype1YP <- listYP
use_data(lsype1YP)

lsype1YPlabels <- listVarLabels
use_data(lsype1YPlabels)

# --------------------------------- #
# ---- Family background files ----
# --------------------------------- #
filePathsFB <- list.files(
  here(dataDir),
  pattern = "family_background", recursive = TRUE, full.names = TRUE
)

listFB <- list()

for (i in seq_along(filePathsFB)) {
  listFB[[i]] <- read.spss(
    filePathsFB[[i]],
    to.data.frame = TRUE, add.undeclared.levels = "no",
    duplicated.value.labels = "condense",
    trim.factor.names = TRUE
  )
}

# extract variable labels
listAttributesFB <- list()
listVarLabelsFB <- list()

listAttributesFB <- lapply(listFB, attributes)

for (i in seq_along(listFB)) {
  listVarLabelsFB[[i]] <- data.table(
    varNames = listAttributesFB[[i]]$names,
    varLabels = listAttributesFB[[i]]$variable.labels
  )
}

# save in /data
lsype1FB <- listFB
use_data(lsype1FB)
lsype1FBlabels <- listVarLabelsFB
use_data(lsype1FBlabels)

