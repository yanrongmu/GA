############ This is the Initialize function #########
########### Last Updated by Hector 11/30 ###################

Initialize <- function(p, P) {
  # Create matrix with P rows and p columns
  FirstGen <- matrix(rbinom(p * P, 1, 0.5), nrow = P)
  
  return(FirstGen)
}