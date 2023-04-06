#' Function for R-Student Residuals
#'
#' @param formula a symbolic description of the model to be fitted, including the response and the predictors
#' @param data the name of the data frame
#'
#' @return the R-Student Residuals of the fitted model
#' @export
#'
#' @examples
#' library(RARR)
#' R_Student_Residuals()
R_Student_Residuals <- function(formula="Petal.Width~Sepal.Length+Sepal.Width+Petal.Length", data=iris){
  fit.OLS <- lm(formula, data)
  myresidual <- residuals(fit.OLS)
  mymodel <- fit.OLS$model
  SiVec <- c()
  Xdata <- mymodel[,-1]
  mymatrix <- data.matrix(Xdata)
  hatmatrix <- mymatrix%*%(solve(t(mymatrix)%*%mymatrix))%*%t(mymatrix)
  diagonalVec <- c()
  for(i in 1:length(myresidual)){
    diagonalVec <- c(diagonalVec, hatmatrix[i,i])
  }
  for(i in 1:length(myresidual)){
    newmodel <- mymodel[-i,]
    rownames(newmodel) <- 1:nrow(newmodel)
    newfit <- lm(formula, newmodel)
    newresidual <- residuals(newfit)
    Si2 <- sum(newresidual^2) / (length(myresidual)-length(newfit$coefficient))
    SiVec <- c(SiVec, Si2)
  }
  RStuRes <- myresidual / sqrt(SiVec*(1 - diagonalVec))
}
