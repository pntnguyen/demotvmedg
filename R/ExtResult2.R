#' Extract result
#'
#' @param data - estimated data
#'
#' @return
#' mean of q11,q10,q00
#' @export
ExtResult2 <- function(data) {
  Q11 <- data |> filter(lastid == 1 & Ay ==1 & Am ==1)
  Q10 <- data |> filter(lastid == 1 & Ay ==1 & Am ==0)
  Q00 <- data |> filter(lastid == 1 & Ay ==0 & Am ==0)

  qq <- data.frame(mQ11 = mean(Q11$Yp2),
                   mQ10 = mean(Q10$Yp2),
                   mQ00 = mean(Q00$Yp2))
  qq
}
