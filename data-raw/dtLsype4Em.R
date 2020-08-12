# clean data in lsype1 for use in EM algorithm

# load packages
library(here)
library(data.table)

# load data
load(here("data/lsype1YP.rda"))
load(here("data/lsype1FB.rda"))

# convert to data.tables
lsype1FB <- lapply(lsype1FB, as.data.table)
lsype1YP <- lapply(lsype1YP, as.data.table)

# select variables and merge waves FB1&4 and YP1&4
dtLsypeWv4Wv8 <- merge(
  merge(
    merge(
      lsype1FB[[1]][, .(NSID,
                            annualIncomeMP_w1 = W1inc1estMP,
                            annualIncomeMP_topBand_w1 = W1inc2estMP,
                            annualIncomeHH_w1 = W1inc1est,
                            contAnnIncHH_w1 = W1GrssyrHH)],
      lsype1FB[[4]][, .(NSID, annualIncomeHH_w4 = W4Inc1EstMP)],
      by = c("NSID"), all = TRUE
    ),
    lsype1YP[[4]][, .(NSID, att2Debt16 = W4debtattYP, att2Schl16 = W4schatYP,
                          infoStudFin16 = W4SupConfYP, sex = W4SexYP,
                          likeY11 = W4YelevenYP,
                          mainAct16 = W4MainActYP)],
    by = c("NSID"), all = TRUE
  ),
  lsype1YP[[8]][, .(NSID, grssWkPay25 = W8GROW, degree25 = W8DDEGP,
                        mainAct25 = W8DACTIVITY,
                        mainAct25_backcoded = W8DACTIVITYC)],
  by = c("NSID"), all = TRUE
)

# drops observations with missing data
dtLsypeNoMissing <- dtLsypeWv4Wv8[
  !(is.na(degree25) | is.na(grssWkPay25) | is.na(likeY11) |
      is.na(mainAct16) | is.na(mainAct25) | is.na(contAnnIncHH_w1))
]

# rename and log outcome variables y1 and y2
dtLsypeNoMissing[, c("y1", "y2") := .(log(contAnnIncHH_w1), log(grssWkPay25))]

# only keep those with non-missing and finite outcomes
dtLsype4Em <- dtLsypeNoMissing[!is.na(y1) & !is.na(y2) & is.finite(y1) &
                                 is.finite(y2)]

# save to /data
use_data(dtLsype4Em)
