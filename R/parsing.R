#' Parse QRTEngine data.
#'
#' Parse QRTEngine data downloaded from Qualtrics as a zip file containing a csv
#' file. This function requires that Python 2.7 is running on your machine by
#' default.
#'
#' @export
#' @param survey.zip String. File path to zipped survey csv downloaded from Qualtrics.
#' @param parser.py String. File path to the QRTEngine Pythong parser.
#' @param ... Optional arguments to pass to read.table()
#' @param out.dir String. Name (possibly with path to) of directory in which to
#'   store output. Will be created if it doesn't exist. Default = "qrteout"
#' @param unlink.out logical. Should output directory and all files be deleted
#'   before function return? Default = TRUE
#' @return Data frame.
parseQrte <- function(survey.zip, parser.py, ...,
                      out.dir = "qrteout", unlink.out = TRUE) {

  # Error handling
  py.version <- system("python --version", intern = T, show.output.on.console = F)
  if (!grepl("Python 2.7", py.version)) {
    stop("Your system is running ", py.version, " by default.
         QRTEngine parser requires that Python 2.7.x is running by default.
         Please change the default Python version on your system to 2.7.x and try again.
         Download Python 2.7.11 from https://www.python.org/downloads/release/python-2711/
         if you do not have Python 2.7.x installed on your system.")
  }

  # Get name of survey
  survey.name <- basename(survey.zip)
  survey.name <- gsub("\\.zip", "", survey.name)

  # Get survey csv (extracted to out.dir)
  dir.create(out.dir, showWarnings = FALSE)
  unzip(survey.zip, exdir = out.dir)
  survey.csv <- file.path(out.dir, paste0(survey.name, ".csv"))

  # Get survey csv.gz outputted by th QRTEngine python parser
  system(paste("python", parser.py, survery.csv))
  survery.csv.gz <- file.path(out.dir, paste0(survey.name, ".csv.gz"))
  survery.csv.gz <- gzfile(survery.csv.gz)

  # Get data after parsing
  data <- readLines(survery.csv.gz)
  data <- textConnection(data)
  data <- read.table(data, header = T, sep = ",", ...)

  # Rename parsed data columns. colnames in QRTE have '[' and ']', which above
  # command outputs as '.' So remove trailing '.' to avoid confusion
  colnames(data) <- gsub("\\.$", "", colnames(data))

  # Delete output files if requested
  if (unlink.out) {
    unlink(out.dir, recursive = T)
  }

  # Return data
  return (data)
}
