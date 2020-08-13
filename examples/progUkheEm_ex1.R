# example1 for progUkheEm()

# call progUkheEm() on BCS data with maxiter = 20
progUkheEm(
  dt = "data/dtBcs4Em.rds",
  K = 4,
  varList = c(y1 = "parInc", y2 = "wklypay", z = "attSchl", d = "degree"),
  maxiter = 20,
  y1cont = FALSE
)
