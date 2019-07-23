## This program is for calculating the expectation and variance of present value
## streams in an N-period bond.

# This function reads input from the user
read <- function() {
  r_c <<- as.numeric(readline(prompt = "Enter the coupon rate: "))
  r <<- as.numeric(readline(prompt = "Enter the discount rate: "))
  p_d <<-
    as.numeric(readline(prompt = "Enter the probability of default: "))
  N <<-
    as.numeric(readline(prompt = "Enter the number of possible payment periods: "))
  f <<- as.numeric(readline(prompt = "Enter the face value: "))
  EV()
}

# This function calculates the present value for N payment streams
PV <- function() {
  c <- r_c * f
  # First, create the coupon matrix
  cMat <- matrix(data = 0,
                 nrow = N,
                 ncol = N)
  x <- 1
  y <- 1
  for (i in x:N) {
    for (j in y:N) {
      cMat[j, i] <- c
    }
    x <- x + 1
    y <- y + 1
  }
  cMat[N, N] <- c + f
  
  # Second, create the discount rate vector
  dr <- seq(0, 0, length.out = N)
  z <- 1
  for (k in z:N) {
    for (l in 1:z) {
      dr[k] <- dr[k] + ((1 + r) ^ -l)
    }
    z <- z + 1
  }
  
  #Now, multiply dr and cMat together
  V <- cMat %*% dr
  return(V)
}

# This function populates the vector for the probabilities.
PD <- function() {
  pdVec <- seq(0, 0, length.out = N)
  for(i in 1:N-1){
    pdVec[i]<-p_d*((1-p_d)^i)
  }
  pdVec[N]<-((1-p_d)^N)
  return(pdVec)
}

# This function computes the expected value of the present value squared.
# The result is to be used in the variance calculation.
sEV<- function() {
  sV <- PV()
  for(i in 1:N){
    sV[i] = sV[i] * sV[i]
  }
  return(t(sV) %*% PD()) 
}

# This function returns the expected value and the variance for the bond
EV <- function() {
  E <- t(PV()) %*% PD()
  V <- sEV() - (E^2)
  return(c("The expected value is: ", round(E, digits = 3), "The variance is: ", round(V, digits = 3)))
}
