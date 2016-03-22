## ---- echo = FALSE-------------------------------------------------------
# Set global knitr options
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ---- eval = FALSE-------------------------------------------------------
#  library(qrtenginer)
#  d <- extractQrteGz("/path/to/YourParsedFile_out.csv.gz")
#  write.csv(scoreQrte(d), "/path/to/save/your/testResults.csv", row.names = FALSE)

## ------------------------------------------------------------------------
library(devtools)

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("drsimonj/qrtenginer")

## ------------------------------------------------------------------------
system("python --version")

## ------------------------------------------------------------------------
library(qrtenginer)

## ------------------------------------------------------------------------
d <- parseQrte("/Users/Simon/Desktop/Flanker.zip",
               "/Users/Simon/Desktop/qrteparser-webservice-master/parser.py")

## ------------------------------------------------------------------------
str(d)

## ---- eval = FALSE-------------------------------------------------------
#  library(qrtenginer)
#  d <- parseQrte("path/to/Qualtrics/data.zip", "path/to/QRTEngine/parser.py")

## ------------------------------------------------------------------------
library(qrtenginer)
d <- extractQrteGz("/Users/Simon/Desktop/Flanker_out.csv.gz")

## ------------------------------------------------------------------------
str(d)

## ---- eval = FALSE-------------------------------------------------------
#  library(qrtenginer)
#  d <- extractQrteGz("/path/to/YourParsedFile_out.csv.gz")

## ------------------------------------------------------------------------
scoreQrte(d)

## ---- eval = FALSE-------------------------------------------------------
#  write.csv(scoreQrte(d), "/Users/Simon/Desktop/Flanker_results.csv", row.names = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  library(qrtenginer)
#  d <- extractQrteGz("/path/to/YourParsedFile_out.csv.gz")
#  write.csv(scoreQrte(d), "/path/to/save/your/testResults.csv", row.names = FALSE)

## ------------------------------------------------------------------------
unique(d$ResponseID)

## ------------------------------------------------------------------------
split(scoreQrte(d), scoreQrte(d)$ResponseID)

