
#' Function for visualizing Normal Probability Plot
#'
#' @param formula a symbolic description of the model to be fitted, including the response and the predictors
#' @param data the name of the data frame
#' @param resid_type the type number of the scaling residuals, 1-Standardized Residuals, 2-Studentized Residuals, 3-Studentized PRESS Residuals, 4-R-student Residuals
#'
#' @return A plot showing whether the residuals follow normal distribution
#' @export
#'
#' @examples
#' library(RARR)
#' normal_plot()
normal_plot <- function(formula="Petal.Width~Sepal.Length+Sepal.Width+Petal.Length", data=iris, resid_type=1){
  par(mfrow=c(1,1))

  # get residuals depending on the given type
  if(resid_type==1){
    resid <- Standardized_Residuals(formula, data)
    my_main <- "Normal Probability Plot of Sorted Standardized Residuals"
    myxlab <- "Sorted Standardized Residuals\n"
  }else if(resid_type==2){
    resid <- Studentized_Residuals(formula, data)
    my_main <- "Normal Probability Plot of Sorted Studentized Residuals"
    myxlab <- "Sorted Studentized Residuals\n"
  }else if(resid_type==3){
    resid <- Studentized_Press_Residuals(formula, data)
    my_main <- "Normal Probability Plot of Sorted Studentized PRESS Residuals"
    myxlab <- "Sorted Studentized PRESS Residuals\n"
  }else if(resid_type==4){
    resid <- R_Student_Residuals(formula, data)
    my_main <- "Normal Probability Plot of Sorted R student Residuals"
    myxlab <- "Sorted R student Residuals\n"
  }else{
    print("Invalid residuals type! Please enter[1/2/3/4]")
    return()
  }

  resid_sort <- sort(resid)
  count_length <- length(resid_sort)
  p_array <- (c(1:count_length)-0.5) / count_length

  plot(resid_sort, p_array, type="p", cex=1, col="blue",
       main = my_main,
       sub = "NOTE: Points with cummulative distribution values between 0.33~0.67 should look like a straight line",
       font.sub = 3,
       xlab = myxlab,
       ylab = "Cummulative Distribution Fuction Value",
       las=1  # adjust y-axis
  )
  # Draw a line between 0.33 to 0.67
  x_start <- resid_sort[round(0.33*count_length + 0.5)]
  x_end <- resid_sort[round(0.67*count_length + 0.5)]
  y_start <- p_array[round(0.33*count_length + 0.5)]
  y_end <- p_array[round(0.67*count_length + 0.5)]
  abline(h=0.33, col='red', lw=3, lty=3)
  abline(h=0.67, col='red', lw=3, lty=3)
  lines(c(x_start,x_end), c(0.33,0.67),lw=5, lty=3,
        col="red")
}
