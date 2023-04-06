#' Function for robust influence
#'
#' @param z a scaled residual to be transformed into robust influence
#' @param criterion the robust criterion function to apply, “Huber” represents Huber’s t function, “Ramsay” represents Ramsay’s Ea function, “Andrews” represents Andrews’ wave function, and “Hampel” represents Hampel’s 17A function.
#' @param t the value of t to be assigned to Huber’s t function if chosen
#' @param a the value of a to be assigned to Ramsay’s Ea function, Andrews’ wave function or Hampel’s 17A function if chosen.
#' @param b the value of b to be assigned to Hampel’s 17A function if chosen.
#' @param c the value of c to be assigned to Hampel’s 17A function if chosen.
#'
#' @return the calculated robust influence function value of the input scaled residual
#' @export
#'
#' @examples
#' library(RARR)
#' Robust_influence(0.2, "Huber", t=2)
Robust_influence <- function(z,criterion,t = NULL,a = NULL,b = NULL,c = NULL){
  if (criterion == "Huber"){
    if (abs(z)<=t){
      return(z)
    }
    else{
      if (z>=0){
        return(t)
      }
      else{
        return(-t)
      }
    }
  }
  else if (criterion == "Ramsay"){
    return(z*exp(-a*abs(z)))
  }
  else if (criterion == "Andrews"){
    if (abs(z)<=a*pi) {
      return (sin(z/a))
    } else {
      return(0)
    }
  }
  else if (criterion == "Hampel"){
    if (abs(z)<=a){
      return(z)
    }
    else if (abs(z)>a && abs(z)<=b){
      if (z>0){
        return(a)
      }
      else{
        return(-a)
      }
    }
    else if (abs(z)>b && abs(z)<=c){
      if (z>0){
        return(a*(c-abs(z))/(c-b))
      }
      else{
        return(-a*(c-abs(z))/(c-b))
      }
    }
    else{
      return(0)
    }
  }
  else {
    warning("Unable to identify the robust criterion functions.")
  }
}
