#' Center and scale time variable
#'
#' @param time - time variables
#'
#' @return
#' data frame with scaled time variabels with mean and standard deviation
#' @export
cen_and_scale <- function(time){

  j_out <- list()

  jj <- time %>%
    scale()

  j_out[["jj"]] <- jj %>% as.numeric()
  j_out[["mean_j"]] <- attributes(jj)$`scaled:center`
  j_out[["sd_j"]] <- attributes(jj)$`scaled:scale`

  j_out
}
