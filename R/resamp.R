#' resampling data
#'
#' @param data - processed data
#' @param boot - doing bootstrap
#'
#' @return
#' @export
resamp <- function(data,boot = FALSE){

  df <- data

  # set.seed(seed)
  # cat("Running SEED", seed, "\n")
  # cat("\n")
  # cat("Resampling Data", "\n")

  clusters <- names(table(df$id))
  index <- sample(1:length(clusters), length(clusters), replace = TRUE)
  bb <- table(clusters[index])
  boot_df <- NULL

  if(boot == F) {
    # not doing bootstrap
    boot_df <- df
  } else {
    for(zzz in 1:max(bb)) {
      # Loop over repeated id
      cc <- df[df$id %in% names(bb[bb %in% c(zzz:max(bb))]), ]
      cc$bid <- paste0(cc$id, zzz)
      boot_df <- rbind(boot_df, cc)
    }
  }

  boot_df$jj <- cen_and_scale(boot_df$j)$jj

  boot_df
}
