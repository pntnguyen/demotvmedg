#' predict binary variables
#'
#' @param mod - model
#' @param ndat - new data
#'
#' @return
#' @export
rFunc <- function(mod, ndat) {
  pred_prob <- predict(mod, newdata = ndat, type = "response")
  return(rbinom(1, size = 1, prob = pred_prob))
}
