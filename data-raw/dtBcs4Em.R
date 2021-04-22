# clean data in bcs70 to use in EM algorithm

# load packages
library(here)
library(data.table)

# load ukheEm (including bcs70 data)
# library(ukheEm)

# load data
load(here("data/bcs70.rda"))
load(here("data/bcs70labels.rda"))

# list of variables to be possible instruments
workVars <- bcs70labels$bcs1986x[varNames %like% "^c5a", varNames]
names(workVars) <- bcs70labels$bcs1986x[varNames %like% "^c5a", varLabels]
jobVars <- bcs70labels$bcs1986x[varNames %like% "^c5d", varNames]
names(jobVars) <- bcs70labels$bcs1986x[varNames %like% "^c5d", varLabels]
adultLifeVars <- bcs70labels$bcs1986x[varNames %like% "^c5e", varNames]
names(adultLifeVars) <- bcs70labels$bcs1986x[varNames %like% "^c5e", varLabels]

# combine YP 1 and 8 and FB 1
dtBcs <- merge(
  merge(
    merge(
      bcs70$bcs1986x[, .(bcsid, attSchl = q46.1, parInc = oe2,
                         .SD), .SDcols = c(
                           workVars, jobVars, adultLifeVars
                         )],
      bcs70$bcs1986derived[, .(bcsid = BCSID,
                               readScore = as.numeric(BD4RREAD)
                               )],
      by = "bcsid"
      ),
    bcs70$bcs1986_arithmetic_data[, .(bcsid,
                                      mathScore = as.numeric(mathscore))],
    by = "bcsid", all = TRUE
    ),
  bcs70$bcs1996x[, .(bcsid, degree = fcase(b960219 == "23", T,
                                           default = F),
                     wkPay = wklypay)],
  by = "bcsid", all = TRUE
)

setnames(dtBcs, function(x) stringr::str_remove(x, ".SD."))

# drop observations with missing data
dtBcsNoNA <- dtBcs[!(is.na(degree) | is.na(wkPay)), ]

# define log pay after university
dtBcsNoNA[, logWkPay := log(wkPay)]

# keep only individuals with wages between 1st and 99th percentiles
wageLims <- dtBcsNoNA[, quantile(wkPay, probs = c(.01, .99), na.rm = TRUE)]
dtBcs4Em <- dtBcsNoNA[wkPay %between% wageLims]

# using binned parental income data
# needs to be formatted to calculate likelihood directly
# each observation has left and right values corresponding to lower and upper
# bounds of relevant bin

# lower bound
dtBcs4Em[, left := fcase(
  parInc == "<50    pw/Under 2600 pa", 10,
  parInc == "50-99  pw/2600-5199  pa", 50,
  parInc == "100-149pw/5200-7799  pa", 100,
  parInc == "150-199pw/7800-10399 pa", 150,
  parInc == "200-249pw/10400-12999pa", 200,
  parInc == "250-299pw/13000-15599pa", 250,
  parInc == "300-349pw/15600-18199pa", 300,
  parInc == "350-399pw/18200-20799pa", 350,
  parInc == "400-449pw/20800-23399pa", 400,
  parInc == "450-499pw/23400-25999pa", 450,
  parInc == "500&over /26000&over", 500
)]

# upper bound
dtBcs4Em[, right := fcase(
  parInc == "<50    pw/Under 2600 pa", 50,
  parInc == "50-99  pw/2600-5199  pa", 99,
  parInc == "100-149pw/5200-7799  pa", 149,
  parInc == "150-199pw/7800-10399 pa", 199,
  parInc == "200-249pw/10400-12999pa", 249,
  parInc == "250-299pw/13000-15599pa", 299,
  parInc == "300-349pw/15600-18199pa", 349,
  parInc == "350-399pw/18200-20799pa", 399,
  parInc == "400-449pw/20800-23399pa", 449,
  parInc == "450-499pw/23400-25999pa", 499,
  parInc == "500&over /26000&over", 600
)]

# take the log of the bounds
dtBcs4Em[, c("left", "right") := .(log(left), log(right))]

# combine maths and reading score
dtBcs4Em[, combnScore := fcase(
  !is.na(mathScore) & !is.na(readScore), mathScore + readScore / 2,
  !is.na(readScore) & is.na(mathScore), readScore,
  !is.na(mathScore) & is.na(readScore), mathScore
)]


# save to \data
use_data(dtBcs4Em, overwrite = TRUE)

