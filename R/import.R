#' Import data from XPS-ESCA spectrometer
#'
#' Import ASCII text-file with one or more intervals of XPS data,
#' where each interval (transition) specifies a description,
#' x-start (binding energy), x-step, and number of rows.
#' These ASCII files commonly use the *.asc extension, but may use
#' other extensions.
#'
#' @param xps.path Path to file with XPS data in expected format
#'
#' @return dataframe with
#'     + interval : chr
#'     + xstart   : num
#'     + xstep    : num
#'     + x        : num
#'     + y        : num
#' @export
xps2df <- function(xps.path) {

   # extract the file extension from path (we use it later)
   xps.ext <- sub("^.*\\.", "", xps.path)

   # Read the whole input file
   dfile <- file(xps.path, "r")
   # Note that readLines skips empty lines, causing line numbers
   # to not match between xps.path and f vector!
   # n=-1 means to read *all* lines
   f <- readLines(dfile, n = -1)
   close(dfile)

   # the xps.path may contain one or more data intervals
   # each one is expected to start with a line saying "Full"
   interval.start.rexp <- "^Full"

   # Look for data interval header markers
   intervals <- data.frame(begin = which(regexpr(interval.start.rexp, f, perl = TRUE) == 1))
   # Look for data interval footer markers
   if (length(intervals$begin) > 1) {
      intervals$end <- c(intervals$begin[2:length(intervals$begin)], length(f))
   } else {
      intervals$end <- length(f)
   }

   # For each data interval, we extract description, x start, x step, and total length
   intervals$desc       <- ""
   intervals$xstart     <- as.numeric(NA)
   intervals$xstep      <- as.numeric(NA)
   intervals$xlen       <- as.numeric(NA)
   intervals$begin.data <- as.numeric(NA)
   intervals$end.data   <- as.numeric(NA)
   for (i in 1:dim(intervals)[1]) {
      intervals$desc[i]   <- f[intervals$begin[i]:intervals$end[i]][3]
      intervals$xstart[i] <- as.numeric(f[intervals$begin[i]:intervals$end[i]][5])
      intervals$xstep[i]  <- as.numeric(f[intervals$begin[i]:intervals$end[i]][6])
      intervals$xlen[i]   <- as.numeric(f[intervals$begin[i]:intervals$end[i]][7])
   }
   intervals$begin.data <- intervals$begin + 7
   intervals$end.data   <- intervals$begin.data + intervals$xlen - 1

   data.exp <-
      data.frame(
         # substrateid = sampleid,
         # sampleid = paste(
         #    sampleid,
         #    # use the last two digits in the filename as unique specifier
         #    gsub(
         #       pattern = "^.+_", replacement = "",
         #       x = gsub(
         #          pattern = paste0("\\.", xps.ext, "$"),
         #          replacement = "",
         #          x = basename(xps.path))),
         #    sep = "-"),
         spectrum = common::int2padstr(ii = 1, pchr = "0", w = 3),
         interval = intervals$desc[1],
         xstart   = intervals$xstart[1],
         xstep    = intervals$xstep[1],
         x = seq(from = intervals$xstart[1], by = intervals$xstep[1], length.out = intervals$xlen[1]),
         y = as.numeric(f[intervals$begin.data[1]:intervals$end.data[1]]))

   if (dim(intervals)[1] > 1) {
      for (i in 2:dim(intervals)[1]) {
         data.exp <-
            rbind(
               data.exp,
               data.frame(
                  # substrateid = sampleid,
                  # sampleid = paste(
                  #    sampleid,
                  #    # use the last two digits in the filename as unique specifier
                  #    gsub(
                  #       pattern = "^.+_", replacement = "",
                  #       x = gsub(
                  #          pattern = paste0("\\.", xps.ext, "$"),
                  #          replacement = "",
                  #          x = basename(xps.path))),
                  #    sep = "-"),
                  spectrum = common::int2padstr(ii = i, pchr = "0", w = 3),
                  interval = intervals$desc[i],
                  xstart   = intervals$xstart[i],
                  xstep    = intervals$xstep[i],
                  x = seq(from = intervals$xstart[i], by = intervals$xstep[i], length.out = intervals$xlen[i]),
                  y = as.numeric(f[intervals$begin.data[i]:intervals$end.data[i]])))
      }
   }
   # data.exp$idunique <- paste(data.exp$sampleid, data.exp$spectrum, sep = "-")

   return(data.exp)
}
