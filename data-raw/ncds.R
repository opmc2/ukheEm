# code to prepare `ncds` dataset goes here

# Notes: this files contains the code to import sweeps 0-5 of the NCDS, which is
# available to download from
# "https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000032".

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
dataDir <- "data-raw/NCDS"

# get list of files
filePaths <- list.files(
  here(dataDir), full.names = TRUE
)

listNcds <- list()

for (i in seq_along(filePaths)) {
  listNcds[[i]] <- read.spss(
    filePaths[[i]],
    to.data.frame = TRUE, add.undeclared.levels = "no",
    duplicated.value.labels = "condense",
    trim.factor.names = TRUE
  )
}

# make all variable names lowercase
for (i in seq_along(listNcds)) {
  names(listNcds[[i]]) <- tolower(names(listNcds[[i]]))
}

# make into dts
listNcds <- lapply(listNcds, as.data.table)

# name dts in listNcds
fileNames <- str_match(filePaths, pattern = "NCDS/\\s*(.*?)\\s*[.]sav")[, 2]

names(listNcds) <- fileNames

for (i in seq_along(listNcds)) {
  listNcds[[i]][, fileName := fileNames[[i]]]
}

# make one big data.table
ncds <- rbindlist(listNcds, fill = T)



usethis::use_data(ncds, overwrite = TRUE)
