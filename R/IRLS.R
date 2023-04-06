#' Function for iteratively reweighed least squares algorithm
#'
#' @param formula a symbolic description of the model to be fitted, including the response and the predictors.The response is strictly restricted with rank one.
#' @param data the name of the data frame
#' @param threshold the threshold used to determine when the algorithm ends with default value 0.000001.
#' @param state the robust criterion function to apply, “Huber” represents Huber’s t function, “Ramsay” represents Ramsay’s Ea function, “Andrews” represents Andrews’ wave function, and “Hampel” represents Hampel’s 17A function.
#' @param t the value of t to be assigned to Huber’s t function if chosen
#' @param a the value of a to be assigned to Ramsay’s Ea function, Andrews’ wave function or Hampel’s 17A function if chosen.
#' @param b the value of b to be assigned to Hampel’s 17A function if chosen.
#' @param c the value of c to be assigned to Hampel’s 17A function if chosen.
#'
#' @return the calculated robust regression coefficient matrix
#' @export
#'
#' @examples
#' library(RARR)
#' IRLS(Petal.Width~Sepal.Length+Sepal.Width+Petal.Length, iris, state="Huber", t=2)
IRLS = function(formula,data,threshold = 0.000001,state,t=NULL,a=NULL,b=NULL,c=NULL){
  temp_model <- model.frame(formula,data = data)
  temp_model <- data.frame(lapply(temp_model,as.numeric))
  model <- lm(formula = formula,data = temp_model)
  s <- median(abs(model$residuals-median(model$residuals)))/0.6745
  rnk <- ncol(temp_model)
  num <- nrow(temp_model)
  temp_coefficient = as.matrix(model$coefficients)
  y <- as.matrix(temp_model[1])
  x <- as.matrix(temp_model[-1])
  temp_one <- rep(1,num)
  x <- cbind(temp_one,x)
  while(TRUE){
    y_hat <- x%*%temp_coefficient
    fake_residual <- y - y_hat
    W <- matrix(rep(0,num^2),nrow = num)
    for (i in 1:num){
      origin <- fake_residual[i]/s
      if (origin != 0){
        W[i,i] <- Robust_influence(z = origin,criterion = state,t=t,a=a,b=b,c=c)/origin
      }
      else{
        W[i,i] <- 1
      }
    }
    inverse <- solve(t(x)%*%W%*%x)
    new = inverse%*%t(x)%*%W%*%y
    modified_coefficient <- as.matrix(new)
    temp_sum <- sum(temp_coefficient - modified_coefficient)^2
    if(temp_sum < threshold){
      return(modified_coefficient)
    }
    temp_coefficient <- modified_coefficient
  }
}
