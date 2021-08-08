## --------------------------------------------------------------------------- #
## dtLsype4Em.R
##
## Project: UkheEm
## Purpose: Prepares Lsype data on which to run `progUkheEm()`
## Author: Oliver Cassagneau-Francis
## Date: Tue Apr 20 19:07:36 2021
## --------------------------------------------------------------------------- #
## Notes:

# load packages
library(here)
library(data.table)
library(magrittr)

# load data
load(here("data/lsype1YP.rda"))
load(here("data/lsype1FB.rda"))

# convert to data.tables
lsype1FB <- lapply(lsype1FB, as.data.table)
lsype1YP <- lapply(lsype1YP, as.data.table)

# noncognitive variables
locVarsP <- paste0("W2Fat", c(1, 5, 8), "YP")
locVarsN <- "W2Fat7YP"

vars2keep <- c("NSID", locVarsP, locVarsN, "W2yschat1", "W2ghq12scr")

dtNonCog <- lsype1YP[[2]][, ..vars2keep] %>%
  .[, locScoreP := rowSums((sapply(.SD, as.numeric)*-1) + 4), .SDcols = locVarsP] %>%
  .[, locScoreN := rowSums(sapply(.SD, as.numeric) - 1), .SDcols = locVarsN] %>%
  .[, locScore := locScoreP + locScoreN]

setnames(dtNonCog, old = c("W2yschat1", "W2ghq12scr"), new = c("att2schlScr", "ghqScr"))

stdise <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# normalise and combine noncog scores
dtNonCog[, paste0(c("locScore", "att2schlScr", "ghqScr"), "_std") := lapply(.SD, stdise),
         .SDcols = c("locScore", "att2schlScr", "ghqScr")]
dtNonCog[, noncogScore := (stdise(locScore) + stdise(att2schlScr) - stdise(ghqScr)) / 3]

# cognitive measure
# while waiting to get hold of the NPD I will use as measures of cognitive ability:
# - self-reported "good marks" [W1-3YYS12YP]
# - whether they are "good" at maths, english, science and ICT [W1g<subj>YP]
dtCog <- lsype1YP[[1]][, .(
  NSID,
  gdMath = W1gmathYP,
  gdMath_num = fcase(
    W1gmathYP == "No good at all", 0,
    W1gmathYP == "Not very good", 1,
    W1gmathYP == "Fairly good", 2,
    W1gmathYP == "Very good", 3
  ),
  gdEngl = W1gengYP,
  gdEngl_num = fcase(
    W1gengYP == "No good at all", 0,
    W1gengYP == "Not very good", 1,
    W1gengYP == "Fairly good", 2,
    W1gengYP == "Very good", 3
  ),
  gdSci = W1gsciYP,
  gdSci_num = fcase(
    W1gsciYP == "No good at all", 0,
    W1gsciYP == "Not very good", 1,
    W1gsciYP == "Fairly good", 2,
    W1gsciYP == "Very good", 3
  ),
  gdIct = W1gictYP,
  gdIct_num = fcase(
    W1gictYP == "No good at all", 0,
    W1gictYP == "Not very good", 1,
    W1gictYP == "Fairly good", 2,
    W1gictYP == "Very good", 3
  ),
  gdMarksW1 = W1yys12YP,
  gdMarksW1_num = fcase(
    W1yys12YP == "Strongly disagree", 0,
    W1yys12YP == "Disagree", 1,
    W1yys12YP == "Agree", 2,
    W1yys12YP == "Strongly agree", 3
  )
)] %>%
  merge(
    lsype1YP[[2]][, .(
      NSID,
      gdMarksW2 = W2YYS12YP,
      gdMarksW2_num = fcase(
        W2YYS12YP == "Strongly disagree", 0,
        W2YYS12YP == "Disagree", 1,
        W2YYS12YP == "Agree", 2,
        W2YYS12YP == "Strongly agree", 3
      )
    )]
  ) %>%
  merge(
    lsype1YP[[3]][, .(
      NSID,
      gdMarksW3 = W3yys12YP,
      gdMarksW3_num = fcase(
        W3yys12YP == "Strongly disagree", 0,
        W3yys12YP == "Disagree", 1,
        W3yys12YP == "Agree", 2,
        W3yys12YP == "Strongly agree", 3
      )
    )]
  )

dtCog[, cogScore := stdise(rowSums(.SD)),
      .SDcols = c("gdMath_num", "gdEngl_num", "gdSci_num", "gdIct_num",
                  "gdMarksW1_num", "gdMarksW2_num", "gdMarksW3_num")]

# select variables and merge waves FB1&4 and YP1&4
dtLsype <- lsype1FB[[1]][, .(
  NSID,
  annualIncomeMP_w1 = W1inc1estMP,
  annualIncomeMP_topBand_w1 = W1inc2estMP,
  annualIncomeHH_w1 = W1inc1est,
  contAnnIncHH_w1 = W1GrssyrHH
)] %>%
  merge(
    lsype1FB[[4]][, .(
      NSID,
      annualIncomeHH_w4 = W4Inc1EstMP
    )],
    by = c("NSID"), all = TRUE
  ) %>%
  merge(
    lsype1YP[[4]][, .(
      NSID, att2Debt16 = W4debtattYP,
      att2Schl16 = W4schatYP,
      infoStudFin16 = W4SupConfYP, sex = W4SexYP,
      likeY11 = W4YelevenYP,
      leaveHomeGd = W4BenefitsYP0h, leaveHomeBad = W4CostsYP0k,
      mainAct16 = W4MainActYP
    )], by = c("NSID"), all = TRUE
  ) %>%
  merge(
    lsype1YP[[8]][, .(
      NSID, grssWkPay25 = W8GROW, degree25 = W8DDEGP,
      mainAct25 = W8DACTIVITY,
      mainAct25_backcoded = W8DACTIVITYC
    )],
    by = c("NSID"), all = TRUE
  ) %>%
  merge(
    dtNonCog[, .(
      NSID,
      locScore, att2schlScr, ghqScr,
      locScore_std, att2schlScr_std, ghqScr_std,
      noncogScore
    )],
    by = "NSID"
  ) %>%
  merge(
    dtCog[, .(
      NSID,
      gdMath_num, gdSci_num, gdIct_num, gdEngl_num,
      gdMarksW1_num, gdMarksW2_num, gdMarksW3_num,
      cogScore
    )]
  )

# drops observations with missing data
dtLsypeNoMissing <- dtLsype[
  !is.na(degree25) & !is.na(grssWkPay25) & !is.na(contAnnIncHH_w1)
]

# rename and log outcome variables y1 and y2
dtLsypeNoMissing[, c("logParInc", "logWkPay") := .(log(contAnnIncHH_w1), log(grssWkPay25))]

# only keep those with non-missing and finite outcomes
dtLsype4Em <- dtLsypeNoMissing[!is.na(logParInc) & !is.na(logWkPay) &
                                 is.finite(logParInc) & is.finite(logWkPay)]

dtLsype4Em[, leaveHome := fcase(
  leaveHomeGd == leaveHomeBad, 2,
  leaveHomeGd == "Mentioned", 3,
  leaveHomeBad == "Mentioned", 1
)]

dtLsype4Em[, leaveHome := factor(leaveHome,
                                 levels = 1:3,
                                 labels = c("Bad", "Neutral", "Good"))]

# save to /data
use_data(dtLsype4Em, overwrite = TRUE)
