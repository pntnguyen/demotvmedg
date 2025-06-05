#' Confident interval calculation
#'
#' @param data - input data
#' @param ci - percentage of confident interval
#' @param boot - doing bootstrapping
#'
#' @return
#' @export
cal_ci <- function(data,ci = 0.95,boot = T){

  if (boot == F){
    resu <- NULL
  } else {
    qnt <- quantile(data, na.rm = TRUE, probs = c((1-ci)/2,1 - (1-ci)/2))
    resu <- paste0("(",round(qnt[1],3),",",round(qnt[2],3),")")
  }
  resu
}
