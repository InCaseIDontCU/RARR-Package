#' Function for PRESS Statistics and Predicted R2
#'
#' @param formula a symbolic description of the model to be fitted, including the response and the predictors
#' @param data the name of the data frame
#'
#' @return the vector that contains the PRESS statistics and the predicted R2
#' @export
#'
#' @examples
#' library(RARR)
#' Press_and_R2()
Press_and_R2 <- function(formula="Petal.Width~Sepal.Length+Sepal.Width+Petal.Length", data=iris){
  fit.OLS <- lm(formula, data)
  myresidual <- residuals(fit.OLS)
  mymodel <- fit.OLS$model
  Xdata <- mymodel[,-1]
  mymatrix <- data.matrix(Xdata)
  hatmatrix <- mymatrix%*%(solve(t(mymatrix)%*%mymatrix))%*%t(mymatrix)
  diagonalVec <- c()
  for(i in 1:length(myresidual)){
    diagonalVec <- c(diagonalVec, hatmatrix[i,i])
  }
  PRESS.Stat <- sum((myresidual/(1 - diagonalVec))^2)
  PRESS.R2 <- 1 - PRESS.Stat / sum(myresidual^2)
  cat("The PRESS Statistics is ", PRESS.Stat, " and the predicted R2 is ", PRESS.R2)
  return(c(PRESS.Stat, PRESS.R2))
}
