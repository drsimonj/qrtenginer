#' Score a set of response times.
#'
#' Score proportion of anticipatory resposnes and lapse responses, and mean
#' response time for acceptable times from a vector of response times.
#'
#' @export
#' @param rtd Numeric vector. Vector of response times captured in milliseconds.
#' @param anti.time Number. Number of milliseconds below which a repsonse should
#'   be classified as an anticipatory. Default = 100
#' @param lapse.time Number. Number of milliseconds above which a repsonse should
#'   be classified as a lapse. Deafult = 500
#' @return Named vector with proportion of anticipatory resposnes (p.anti),
#'   proportion of lapses (p.lapse), and mean response time for all responses
#'   not classified as anticipatory or lapse.
scoreRts <- function(rts, anti.time = 100, lapse.time = 500) {
  anti  <- rts < anti.time
  lapse <- rts > lapse.time

  p.anti  <- mean(anti)
  p.lapse <- mean(lapse)
  mean.rt <- mean(rts[!(anti | lapse)])

  c(p.anti = p.anti, p.lapse = p.lapse, mean.rt = mean.rt)
}
