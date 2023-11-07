orderPoly <- 3
a <- c(1,3,6,7)
b <- c(10, 20, 19, 33)
vectList <- list(a, b)

AugCoeffMatrix <- function(vectList, orderPoly) {
  independent <- vectList[[1]]
  dependent <- vectList[[2]]
  matlist <- numeric()
  colrow <- orderPoly + 1
  deg <- 0
  for (i in 1:colrow) {
    for (j in 1:colrow) {
      matlist <- append(matlist, sum(independent^deg))
      deg <- deg + 1
    }
    deg <- deg - orderPoly
  }
  
  deg<-0
  for (k in 1: colrow) {
    matlist <- append(matlist, sum((independent^deg)*dependent))
    deg <- deg + 1;
  }

  augmat <- matrix(data=matlist, nrow = orderPoly+1, ncol = orderPoly+2, byrow = FALSE)
  return(augmat)
}

GaussJordanMethod <- function(retList) {
  augCoeff <- retList
  n <- orderPoly + 1
  for (i in 1:n) {
    if (i != n) {
      #find pivot row
      if ((all(augCoeff[i:n, i] == augCoeff[i,i])==TRUE)) {
        
      } else {
        maxnum <- max(abs(c(augCoeff[i:n,i])))
        rw <- which(abs(augCoeff[,i]) == maxnum)   #this will be the pivot row
        
        #no unique solution exists, STOP
        if ((augCoeff[rw,i]) == 0) {
          return(NA)
        }
        
        #partial pivoting/ switching row using temporary list called sepList
        sepList <- augCoeff[i,]
        augCoeff[i,] <- augCoeff[rw,]
        augCoeff[rw,] <- sepList
      }
    }
    augCoeff[i,] <- augCoeff[i,] / augCoeff[i,i]  
    for (j in 1:n) {
      if (i == j) {
        next
      }
      normalizedRow <- augCoeff[j,i] * augCoeff[i,]
      augCoeff[j,] <- augCoeff[j,] - normalizedRow
    }
  }
  
  #storing the solutions to the list
  solutions <- c()
  for (k in 1:n) {
    solutions[k] <- augCoeff[k,ncol(augCoeff)]
  }
  
  #labeled list to be returned
  listAnswers <- list (
    variables = retList[[1]],
    Augmented_Coefficient_Matrix = augCoeff,
    solution = solutions
  )
  
  #this will check if the system has unique solutions/ will return the list if the solution is unique
  if (sum(abs(augCoeff[1:n, n])) < 1e-10) {
    return(NA)
  } else {
    return(listAnswers)
  }
  
}

PolynomialRegression <- function(vectList, orderPoly) {
  augmat <- AugCoeffMatrix(vectList, orderPoly)
  gaussmethod <- GaussJordanMethod(augmat)
  
  
  polynomial_string <- paste()
  newList <- list(augmat, gaussmethod)
  return(newList)
}


  
if (length(vectList[[1]]) != length(vectList[[2]])) {
  print("Not possible")
} else {
  print(PolynomialRegression(vectList, orderPoly))
}



