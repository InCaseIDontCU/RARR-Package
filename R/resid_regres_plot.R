#' Function to visualize scaling residuals versus Regressors
#'
#' @param formula a symbolic description of the model to be fitted, including the response and the predictors
#' @param data the name of the data frame
#' @param resid_type the type number of the scaling residuals, 1-Standardized Residuals, 2-Studentized Residuals, 3-Studentized PRESS Residuals, 4-R-student Residuals
#'
#' @return a figure showing the relationship between residuals and regressors
#' @export
#'
#' @examples
#' library(RARR)
#' resid_regres_plot()
resid_regres_plot <- function(formula="Petal.Width~Sepal.Length+Sepal.Width+Petal.Length", data=iris, resid_type=1){
  myfit <- lm(formula, data)
  x <- myfit$model[,2:dim(myfit$model)[2]]  # extract x
  if(is.null(dim(x))){
    mydim <- 1
    op <- par(mfrow=c(1,1), las=1)
  }else{
    mydim <- dim(x)[2]
    number_of_regressor <- ceiling(mydim/2)
    if(number_of_regressor>4){
      number_of_regressor <- 4
    }
    op <- par(mfrow=c(number_of_regressor,2), las=1)
  }

  # get residuals depending on the given type
  if(resid_type==1){
    resid <- Standardized_Residuals(formula, data)
  }else if(resid_type==2){
    resid <- Studentized_Residuals(formula, data)
  }else if(resid_type==3){
    resid <- Studentized_Press_Residuals(formula, data)
  }else if(resid_type==4){
    resid <- R_Student_Residuals(formula, data)
  }else{
    print("Invalid residuals type! Please enter[1/2/3/4]")
    return()
  }

  i <- 0
  while(i<mydim){
    i <- i + 1
    current_reg = bquote(paste("Current Regressor: ", x[.(i)]))
    if(mydim==1){
      myx <- x
    }else{
      myx <- as.vector(unlist(x[i]))
    }
    if(class(myx) != "numeric"){
      next
    }else{
      plot(myx, resid, type="p",
           main = "Plot of Residuals Against Regressors",
           xlab = "Value of Regressor",
           ylab = "Value of Scaling Residuals",
           sub = current_reg, font.sub=3,
      )
      abline(h=0, col="red", lty=3, lw=3)
    }
  }
  on.exit(par(op))
  par(mfrow=c(1,1))
}
