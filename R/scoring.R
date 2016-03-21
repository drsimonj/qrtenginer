#' Convenience function to determine if Response time is valid.
#'
#' Will return a logical vector set to TRUE for any response time falling within
#' a valid range of times. Can also be to test greater than or less than ONLY (e.g., for
#' count of anticipatory or lapse responses).
#'
#' @export
#' @param rts Numeric vector of response times captured in milliseconds.
#' @param min Number. Minumum response time considered to be valid.
#' @param max Number. Maximum response time considered to be valid.
validateRts <- function(rts, min = NULL, max = NULL) {
  valid <- rep(TRUE, length(rts))

  if(is.numeric(min))
    valid[rts < min] <- FALSE

  if(is.numeric(max))
    valid[rts > max] <- FALSE

  return (valid)
}


#' Score a set of response times.
#'
#' Calculate (1) count of anticipatory responses, (2) count of lapse responses,
#' (3) count of valid responses, and (4) mean response time for valid responses
#' (default between 100 and 500 ms) from a vector of response times.
#'
#' @export
#' @inheritParams validateRts
#' @return Named vector with proportion of anticipatory resposnes (p.anti),
#'   proportion of lapses (p.lapse), and mean response time for all responses
#'   not classified as anticipatory or lapse (mean.rt).
scoreRts <- function(rts, min = 100, max = 500) {
  valid <- validateRts(rts, min = min, max = max)
  mean.rt <- mean(rts[valid])
  n.valid <- sum(valid)

  n.anti  <- sum(!validateRts(rts, min = min))
  n.lapse <- sum(!validateRts(rts, max = max))

  c(mean.rt = mean.rt, n.valid = n.valid, n.anti = n.anti, n.lapse = n.lapse)
}

#' Score data from a compatible QRTEngine test
#'
#'
#' This function will score variables for a QRTEngine test from the parsed raw
#' data. It uses the BlockId column in the parsed data to identify the test and
#' apply the appropriate variable calculations. It also uses the BlockID to
#' exclude practice trial data. \cr\cr
#' To determine the available optional arugments and variables returned by the function: \cr\cr
#' For Simple Reaction Time test, see ?scoreRts
#'
#' @export
#' @param data Data Frame of QRTEngine data after it has been parsed - e.g.,
#'   using extractQrteGz() or parseQrte()
#' @param ... Optional arguements to pass on to various specific scoring
#'   functions.
#' @return Data frame.
scoreQrte <- function(data, ...) {

  # Object to be returned as a data frame
  tmp <- NULL

  # Get block ids to check test acronym
  block.ids <- data$BlockId

  # Remove practice data
  data <- data[!grepl("practice", block.ids), ]

  # If Simple Reaction Time Test
  if (all(grepl("SRT", block.ids))) {
    tmp <- by(data$Stimulus.RT, data$ResponseID, scoreRts, ...)
    tmp <- do.call(rbind, tmp)
    tmp <- as.data.frame(tmp)
  }

  # If Flanker Test
  else if (all(grepl("FLANKER", block.ids))) {

    tmp <- by(data$Stimulus.RT, data[, c("ResponseID", "Congruency", "Stimulus.ACC")],
              scoreRts, ...)
    tmp.dat <- do.call(rbind, tmp)
    tmp.dat <- as.data.frame(tmp.dat)

    for (i in seq(dim(tmp))) {
      var.label <- names(dimnames(tmp))[i]
      n.rep.each <- tail(cumprod(dim(tmp)[-c(1:i)]), 1)
      if(length(n.rep.each) == 0)
        n.rep.each <- 1
      var.dat <- rep(dimnames(tmp)[[i]], each = n.rep.each)
      tmp.dat[, var.label] <- var.dat
    }

    tmp <- tmp.dat
  }

  # If Task Switching Test
  else if (all(grepl("SWITCHING", block.ids))) {

    # Order trials based on appearance
    data <- data[order(data$ResponseID, data$SWITCHINGtest.TrialNr),]

    # Assign trial type to each row
    x <- data$instruction
    x2 <- head(x, -1)
    x <- x[-1]
    data$Type <- c(NA, ifelse(x == x2, "repeat", "switch"))

    tmp <- by(data$Stimulus.RT, data[, c("ResponseID", "Type", "Stimulus.ACC")],
              function(x) {
                scoreRts(x[-1], ...) # Drop First row for each person (as no trial type)
              })
    tmp.dat <- do.call(rbind, tmp)
    tmp.dat <- as.data.frame(tmp.dat)

    for (i in seq(dim(tmp))) {
      var.label <- names(dimnames(tmp))[i]
      n.rep.each <- tail(cumprod(dim(tmp)[-c(1:i)]), 1)
      if(length(n.rep.each) == 0)
        n.rep.each <- 1
      var.dat <- rep(dimnames(tmp)[[i]], each = n.rep.each)
      tmp.dat[, var.label] <- var.dat
    }

    tmp <- tmp.dat

  }

  # If Running Letters
  else if (all(grepl("RUNNING", block.ids))) {
    tmp <- by(data$Stimulus.ACC, data$ResponseID,
              function(x) {
                n.correct  <- sum(x)
                n.wrong    <- sum(1 - x)
                return (c(n.correct = n.correct, n.wrong = n.wrong))
              }
    )
    tmp <- do.call(rbind, tmp)
    tmp <- as.data.frame(tmp)
  }

  # If Esoteric Analogies Test
  else if (all(grepl("EA", block.ids))) {
    tmp <- by(data[, c("Stimulus.ACC", "Confidence.RESP")], data$ResponseID,
              function(x) {
                n.correct  <- sum(x$Stimulus.ACC)
                n.wrong    <- sum(1 - x$Stimulus.ACC)
                mean.confidence <- mean(x$Confidence.RESP)
                return (c(n.correct = n.correct, n.wrong = n.wrong,
                          mean.confidence = mean.confidence))
              }
           )
    tmp <- do.call(rbind, tmp)
    tmp <- as.data.frame(tmp)
  }

  return(tmp)
}
