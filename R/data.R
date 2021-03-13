# documenting the datasets in /data

#' Young person data from the LSYPE1 waves 1-8
#'
#' A list containing 8 data.frames corresponding to the 8 waves of the LSYPE1.
#'
#' @format A list of 8 data.frames. The unique common identifier for each of the
#' 15,770 individuals in wave 1 is \code{NSID}.
#' @source \url{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5545}
"lsype1YP"

#' Variable labels for the LSYPE1 young person data
#'
#' A list of 8 data.tables containing variable names and labels corresponding to
#' the variables in \code{lsype1YP}.
#'
#' @format A list of 8 data.tables:
#' \describe{
#'   \item{varNames}{The name of a variable in a data.frame in \code{lsype1YP}}
#'   \item{varLabel}{The corresponding variable label containing additional
#'     information}
#' }
#' @source \url{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5545}
"lsype1YPlabels"

#' Family background data from the LSYPE1 waves 1-8
#'
#' A list containing 8 data.frames corresponding to the 8 waves of the LSYPE1.
#'
#' @format A list of 8 data.frames. The unique common identifier for each of the
#' 15,770 individuals in wave 1 is \code{NSID}.
#' @source \url{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5545}
"lsype1FB"

#' Variable labels for the LSYPE1 family background data
#'
#' A list of 8 data.tables containing variable names and labels corresponding to
#' the variables in \code{lsype1FB}.
#'
#' @format A list of 8 data.tables:
#' \describe{
#'   \item{varNames}{The name of a variable in a data.frame in \code{lsype1FB}}
#'   \item{varLabel}{The corresponding variable label containing additional
#'     information}
#' }
#' @source \url{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5545}
"lsype1FBlabels"

#' Data collected in the 1986 and 1996 waves of the BCS1970
#'
#' A list of 7 data.tables containing the information collected in the 1986 and
#' 1996 waves of the BCS1970.
#'
#' @format A named list of 7 data.tables. The unique common identifier is
#' \code{bcsid}. The tables are:
#' \describe{
#'   \item{bcs1986_arithmetic_data}{Results of an arithmetic test taken by all
#'     cohort members at 16}
#'   \item{bcs1986_occupational_interests}{Responses and results of a
#'     questionnaire on future occupations}
#'   \item{bcs1986_reading_matrices}{Results of a reading test taken by all
#'     cohort members at 16}
#'   \item{bcs1986derived}{Derived variables from the 1986 wave of the BCS1970}
#'   \item{bcs1986x}{Responses to the main survey of the 1986 wave}
#'   \item{bcs1996derived}{Derived variables from the 1996 wave of the BCS1970}
#'   \item{bcs1996x}{Responses to the main survey of the 1996 wave}
#' }
#' @source \url{https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200001}
"bcs70"

#' Variable labels for \code{bcs70}
#'
#' A list of 7 data.tables containing labels for the variables in \code{bcs70}.
#'
#' @format A list of 7 data.tables.
#' \describe{
#'   \item{varName}{The name of the variable in \code{bcs70}}
#'   \item{varLabel}{The corresponding variable label containing additional
#'     information}
#' }
#' @source \url{https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200001}
"bcs70labels"

#' BCS data formatted for use in \code{progUkheEm}.
#'
#' A data.table of cleaned and formatted BCS data ready for the EM algorithm,
#' as implemented by \code{progUkheEm}.
#'
#' @format A data.table with 2,614 rows and 7 variables:
#' \describe{
#'   \item{bcsid}{The unique identifier for each member of the BCS1970 (chr)}
#'   \item{attSchl}{Reported attitude to school, z (factor, 5 levels)}
#'   \item{parInc}{Parental income in bins, y1 (factor, 13 levels)}
#'   \item{degree}{Whether the individual holds an UG degree at 25, d (logical)}
#'   \item{wklypay}{Weekly pay at 25 in GBP, y2 (numeric)}
#'   \item{left}{The lower bound for parental income in log(GBP), y1 (numeric)}
#'   \item{right}{The upper bound for parental income in log(GBP), y1 (numeric)}
#' }
#' @source See \code{bcs70}.
"dtBcs4Em"

#' LSYPE1 data formatted for use in \code{progUkheEm}
#'
#' A data.table of cleaned and formatted date ready for the EM algorithm as
#' implemented by \code{progUkheEm}.
#'
#' @format A data.table of 1,584 rows and 18 variables:
#' \describe{
#'   \item{NSID}{Unique identifier for LSYPE cohort members (CMs)}
#'   \item{annualIncomeMP_w1}{Main parent's annual income (and partner) in wave
#'     1, banded (factor with 33 levels)}
#'   \item{annualIncomeMP_topBand_w1}{Further detail if in the top band of main
#'     parent's annual income, banded (factor with 60 levels)}
#'   \item{annualIncomeHH_w1}{Annual income of the household in wave 1
#'     (both parents?), banded (factor with 33 levels)}
#'   \item{contAnnIncHH_w1}{Annual income of the household in wave 1
#'     (both parents?), in GBP (numeric)}
#'   \item{annualIncomeHH_w4}{Annual income of the household in wave 4
#'     (both parents?), banded (factor with 12 levels)}
#'   \item{att2Debt16}{CMs attitude to debt at 16, score 2-24 (numeric)}
#'   \item{att2Schl16}{CMs attitude to school at 16, score 0-20 (numeric)}
#'   \item{infoStudFin16}{How well informed the CM felt about student finance
#'     at 16 (factor with 4 levels)}
#'   \item{sex}{CM's gender (factor with 2 levels)}
#'   \item{likeY11}{Whether CM enjoyed Year 11 (factor with 4 levels)}
#'   \item{mainAct16}{CM's main activity at 16 (factor with 5 levels)}
#'   \item{grssWkPay25}{CM's gross weekly pay at 25, in GBP (numeric)}
#'   \item{degree25}{Whether CM holds an UG degree at 25 (factor with 2 levels)}
#'   \item{mainAct25}{CM's main activity at 25 (factor with 14 levels)}
#'   \item{mainAct25_backcoded}{CM's main activity at 25 (factor with 10
#'     levels)}
#'   \item{y1}{Log of \code{contAnnIncHH_w1} (numeric)}
#'   \item{y2}{Log of \code{grssWkPay25} (numeric)}
#' }
#' @source See \code{lsype1YP} and \code{lsype1FB}.
"dtLsype4Em"

#' Example data to demonstrate how to deal with binned outcome data
#'
#' A simplified dataset containing only binned income data (\code{binnedDate})
#' and an \code{id} variable to use in a demonstration of formatting binned data
#' to work with \code{progUkheEm}.
#'
#' @format A data.table of 2,614 rows and 2 variables:
#' \describe{
#'   \item{\code{id}}{A unique identifier for each observation}
#'   \item{\code{binnedData}}{Income data in bins}
#' }
#' @source See \code{bcs70}.
"binnedDataExample"

#' Data collected in the seventh wave of the Millennium Cohort Study
#'
#' A list of 8 data.tables containing the information collected in the
#'
#' @format A named list of 8 data.tables. The unique common identifier is
#' \code{MCSID}. The tables are:
#' \describe{
#'   \item{cm_cognitive_assessment}{Results of cognitive tests taken by cohort
#'     members (CM)}
#'   \item{cm_derived}{Derived variables from CM data}
#'   \item{cm_interview}{Variables from CM interview}
#'   \item{family_derived}{Derived variables from family interview}
#'   \item{family_interview}{Responses to the family interview}
#'   \item{hhgrid}{Data from the household grid}
#'   \item{parent_cm_interview}{Responses parent/CM joint interview}
#'   \item{parent_interview}{Responses parent interview}
#' }
#' @source \url{https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000031}
"mcs7"

#' Variable labels for \code{mcs7}
#'
#' A list of 8 data.tables containing labels for the variables in \code{mcs7}.
#'
#' @format A list of 8 data.tables. Each contains the following two variables:
#' \describe{
#'   \item{varName}{The name of the variable in \code{mcs7}}
#'   \item{varLabel}{The corresponding variable label containing additional
#'     information}
#' }
#' @source \url{https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000031}
"mcs7Labels"
