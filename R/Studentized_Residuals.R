#' Function for Studentized Residuals
#'
#' @param formula a symbolic description of the model to be fitted, including the response and the predictors
#' @param data the name of the data frame
#'
#' @return the studentized residuals of the fitted model
#' @export
#'
#' @examples
#' library(RARR)
#' Studentized_Residuals()
Studentized_Residuals<-function(formula="Petal.Width~Sepal.Length+Sepal.Width+Petal.Length", data=iris){
  fit.OLS <- lm(formula, data)
  myresidual <- residuals(fit.OLS)
  MSRes <- sum(myresidual^2) / (length(myresidual)-2)
  mymodel <- fit.OLS$model
  Xdata <- mymodel[,-1]
  mymatrix <- data.matrix(Xdata)
  hatmatrix <- mymatrix%*%(solve(t(mymatrix)%*%mymatrix))%*%t(mymatrix)
  diagonalVec <- c()
  for(i in 1:length(myresidual)){
    diagonalVec <- c(diagonalVec, hatmatrix[i,i])
  }
  StuRes <- myresidual / sqrt(MSRes*(1 - diagonalVec))
  return(StuRes)
}

