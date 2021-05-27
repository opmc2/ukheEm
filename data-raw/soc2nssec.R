## --------------------------------------------------------------------------- #
## soc2nssec.R
##
## Project: UK HE EM
## Purpose: Create table mapping SOC to NSSEC codes
## Author: Oliver Cassagneau-Francis
## Date: Tue May 11 11:56:59 2021
## --------------------------------------------------------------------------- #
## Notes:

library(data.table)

soc2nssec <- fread("data-raw/soc2nssec2010.csv")

setnames(soc2nssec,
         old = names(soc2nssec),
         new = c("SOC2010", "socName", "NSSEC"))

usethis::use_data(soc2nssec, overwrite = TRUE)
