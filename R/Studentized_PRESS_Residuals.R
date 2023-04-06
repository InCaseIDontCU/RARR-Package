#' Function for PRESS Statistics and Predicted R2
#'
#' @param formula a symbolic description of the model to be fitted, including the response and the predictors
#' @param data the name of the data frame
#'
#' @return the Studentized PRESS Residuals of the fitted model
#' @export
#'
#' @examples
#' library(RARR)
#' Studentized_Press_Residuals()
Studentized_Press_Residuals<-function(formula="Petal.Width~Sepal.Length+Sepal.Width+Petal.Length", data=iris){
  fit.OLS <- lm(formula, data)
  myresidual <- residuals(fit.OLS)
  myvar <- var(myresidual)
  mymodel <- fit.OLS$model
  Xdata <- mymodel[,-1]
  mymatrix <- data.matrix(Xdata)
  hatmatrix <- mymatrix%*%(solve(t(mymatrix)%*%mymatrix))%*%t(mymatrix)
  diagonalVec <- c()
  for(i in 1:length(myresidual)){
    diagonalVec <- c(diagonalVec, hatmatrix[i,i])
  }
  StuPressRes <- myresidual / sqrt(myvar*(1 - diagonalVec))
  return(StuPressRes)
}
