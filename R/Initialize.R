############ This is the Initialize function #########

Initialize <- function(p, P) {
  # Create matrix with P rows and p columns
  FirstGen <- matrix(rbinom(p * P, 1, 0.5), nrow = P)

  return(FirstGen)
}
