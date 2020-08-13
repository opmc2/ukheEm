# clean data in bcs70 to use in EM algorithm

# load packages
library(here)
library(data.table)

# load data
load(file = here("data/bcs70.rda"))

# combine YP 1 and 8 and FB 1
dtBcs <- merge(
  bcs70$bcs1986x[, .(bcsid, attSchl = q46.1, parInc = oe2)],
  bcs70$bcs1996x[, .(bcsid, degree = fcase(b960219 == "23", T,
                                           default = F), wklypay)],
  by = "bcsid"
)

# drop observations with missing data
dtBcsNoNA <- dtBcs[
  !(is.na(degree) | is.na(wklypay) | is.na(attSchl) |
      is.na(parInc) | parInc %in% c("Uncertain", "Refuse to Answer"))
]

# define y2 (wage at 25/26)
dtBcsNoNA[, y2 := log(wklypay)]

# keep only individuals with wages between 1st and 99th percentiles
wageLims <- bcs70$bcs1996x[, quantile(wklypay,
                                      probs = c(.01, .99), na.rm = TRUE)]
dtBcs4Em <- dtBcsNoNA[wklypay %between% wageLims]

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

# save to \data
use_data(dtBcs4Em, overwrite = TRUE)

# save as .rds to \data to use in examples and functions
saveRDS(dtBcs4Em, file = here("data/dtBcs4Em.rds"))
