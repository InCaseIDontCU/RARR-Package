
#' Function to visualize scaling residuals versus fitted values
#'
#' @param formula a symbolic description of the model to be fitted, including the response and the predictors
#' @param data the name of the data frame
#'
#' @return 4 plots corresponding to different scaling residuals versus fitted values
#' @export
#'
#' @examples
#' library(RARR)
#' resid_fitted_plot()
resid_fitted_plot <- function(formula="Petal.Width~Sepal.Length+Sepal.Width+Petal.Length", data=iris){
  fit.OLS <- lm(formula, data)
  mypredict <- predict(fit.OLS)
  op <- par(mfrow=c(2,2), las=1)

  resid1 <- Standardized_Residuals(formula, data)
  fit_val1 <- mypredict
  plot(fit_val1, resid1, type="p",
       main = "Plot of Residuals Against Fitted Values",
       xlab = "Fitted Value",
       ylab = "Scaling Residuals",
       sub = "Scaling Residual: Standardized Residuals", font.sub=3)
  abline(h=0, col="red", lty=3, lw=3)

  resid2 <- Studentized_Residuals(formula, data)
  plot(fit_val1, resid2, type="p",
       main = "Plot of Residuals Against Fitted Values",
       xlab = "Fitted Value",
       ylab = "Scaling Residuals",
       sub = "Scaling Residual: Studentized Residuals", font.sub=3)
  abline(h=0, col="red", lty=3, lw=3)

  resid3 <- Studentized_Press_Residuals(formula, data)
  plot(fit_val1, resid3, type="p",
       main = "Plot of Residuals Against Fitted Values",
       xlab = "Fitted Value",
       ylab = "Scaling Residuals",
       sub = "Scaling Residual: PRESS Residuals", font.sub=3)
  abline(h=0, col="red", lty=3, lw=3)

  resid4 <- R_Student_Residuals(formula, data)
  plot(fit_val1, resid4, type="p",
       main = "Plot of Residuals Against Fitted Values",
       xlab = "Fitted Value",
       ylab = "Scaling Residuals",
       sub = "Scaling Residual: R-student Residuals", font.sub=3)
  abline(h=0, col="red", lty=3, lw=3)
  on.exit(par(op))
}
