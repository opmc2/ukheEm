# code to import and clean the BCS1970 data

# ---- load packages ----
library(here)
library(data.table)
library(foreign)

# list files
bcsFilePaths <- as.list(list.files(
  path = here("data-raw/Bcs70"),
  pattern = ".sav", full.names = TRUE, recursive = TRUE
))

# extra line in case not all files should be read
files2read <- bcsFilePaths

bcsListDt <- lapply(
  files2read,
  read.spss, to.data.frame = TRUE
)

# extract lfs wavenames
bcsWaves <- stringr::str_extract(files2read, pattern = "[[:alnum:]-_]*.sav")
bcsWaves <- stringr::str_sub(bcsWaves, end = -5L)

# names items in list by waves
names(bcsListDt) <- bcsWaves

bcsListDt <- lapply(bcsListDt, as.data.table)

# extract variable names
bcsListLabels <- lapply(
  bcsListDt,
  function(dt) {
    data.table(varNames = attributes(dt)$names,
               varLabels = attributes(dt)$variable.labels)
  }
)

names(bcsListLabels) <- bcsWaves

# add variable for wave
for (i in seq_along(bcsListDt)) {
  bcsListDt[[i]][, file := bcsWaves[[i]]]
}

saveRDS(bcsListLabels, file = here("Rdata/bcs70labels.rds"))

# save as RDS
saveRDS(bcsListDt, file = here("Rdata/bcs70.rds"))

# save in /data
bcs70 <- bcsListDt
use_data(bcs70, overwrite = T)
bcs70labels <- bcsListLabels
use_data(bcs70labels, overwrite = T)
