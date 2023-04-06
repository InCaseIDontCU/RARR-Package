#' Function for Standardized Residuals
#'
#' @param formula a symbolic description of the model to be fitted, including the response and the predictors
#' @param data the name of the data frame
#'
#' @return the standardized residuals of the fitted model
#' @export
#'
#' @examples
#' library(RARR)
#' Standardized_Residuals()
Standardized_Residuals<-function(formula="Petal.Width~Sepal.Length+Sepal.Width+Petal.Length", data=iris){
  fit.OLS <- lm(formula, data)
  myresidual <- residuals(fit.OLS)
  MSRes <- sum(myresidual^2) / (length(myresidual)-2)
  StRes <- myresidual / (sqrt(MSRes))
  return(StRes)
}
