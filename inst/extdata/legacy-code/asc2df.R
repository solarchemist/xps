source("/home/taha/chepec/chetex/common/R/common/ProvideSampleId.R")
source("/home/taha/chepec/chetex/common/R/common/int2padstr.R")
options(stringsAsFactors = FALSE)
require(plyr)


##################################################
################### asc2df #######################
##################################################
asc2df <- function(datafile) {
   ## Description:
   ##   Read *.asc ASCII-files from ESCA spectrometer,
   ##   calculate energy steps and read intensity values.
   ## Usage:
   ##   asc2df(datafile)
   ## Arguments:
   ##   datafile: text string with full path to .ASC file
   ##             containing single or multiple data ranges
   ## Value:
   ##   Dataframe with the following columns:
   ##   $ sampleid : chr
   ##   $ interval : chr
   ##   $ xstart   : num
   ##   $ xstep    : num
   ##   $ x        : num
   ##   $ y        : num
   #
   
   interval.start.rexp <- "^Full"   
   # Read the input file
   dfile <- file(datafile, "r")
   # Note that readLines apparently completely skips empty lines. 
   # That causes line numbers to not match between source and f vector.
   f <- readLines(dfile, n=-1) # read _all_ lines from data file
   close(dfile)
   #
   
   # Fetch a sampleid for the current job
   sampleid <- ProvideSampleId(pathexpfile = datafile, implementation = "dirname")
   
   # Look for data interval header markers
   intervals <- data.frame(begin = which(regexpr(interval.start.rexp, f, perl = TRUE) == 1))
   # Look for data interval footer markers
   if (length(intervals$begin) > 1) {
      intervals$end <- c(intervals$begin[2:length(intervals$begin)], length(f))
   } else {
      intervals$end <- length(f)
   }
   
   # In each interval, we extract description, x start, x step, and total length
   intervals$desc <- ""
   intervals$xstart <- as.numeric(NA)
   intervals$xstep <- as.numeric(NA)
   intervals$xlen <- as.numeric(NA)
   intervals$begin.data <- as.numeric(NA)
   intervals$end.data <- as.numeric(NA)
   for (i in 1:dim(intervals)[1]) {
      intervals$desc[i] <- f[intervals$begin[i]:intervals$end[i]][3]
      intervals$xstart[i] <- as.numeric(f[intervals$begin[i]:intervals$end[i]][5])
      intervals$xstep[i] <- as.numeric(f[intervals$begin[i]:intervals$end[i]][6])
      intervals$xlen[i] <- as.numeric(f[intervals$begin[i]:intervals$end[i]][7])
   }
   intervals$begin.data <- intervals$begin + 7
   intervals$end.data <- intervals$begin.data + intervals$xlen - 1
   
   data.exp <- data.frame(substrateid = sampleid,
                          sampleid = paste(sampleid, 
                                           # use the last two digits in the filename as unique specifier
                                           gsub(pattern="^.+_", replacement="", 
                                                x=gsub(pattern="\\.asc$", replacement="", x=basename(datafile))),
                                           sep = "-"),
                          spectrum = int2padstr(ii=1, pchr="0", w=3),
                          interval = intervals$desc[1],
                          xstart = intervals$xstart[1],
                          xstep = intervals$xstep[1],
                          x = seq(from = intervals$xstart[1], by = intervals$xstep[1], length.out = intervals$xlen[1]),
                          y = as.numeric(f[intervals$begin.data[1]:intervals$end.data[1]]))
   # For diffractometry algorithms to have a chance to work, xy data should be sorted by increasing x
   data.exp <- arrange(data.exp, x)
   #
   if (dim(intervals)[1] > 1) {
      for (i in 2:dim(intervals)[1]) {
         data.exp <- rbind(data.exp,
                           data.frame(substrateid = sampleid,
                                      sampleid = paste(sampleid, 
                                                       # use the last two digits in the filename as unique specifier
                                                       gsub(pattern="^.+_", replacement="", 
                                                            x=gsub(pattern="\\.asc$", replacement="", x=basename(datafile))),
                                                       sep = "-"),
                                      spectrum = int2padstr(ii=i, pchr="0", w=3),
                                      interval = intervals$desc[i],
                                      xstart = intervals$xstart[i],
                                      xstep = intervals$xstep[i],
                                      x = seq(from = intervals$xstart[i], by = intervals$xstep[i], length.out = intervals$xlen[i]),
                                      y = as.numeric(f[intervals$begin.data[i]:intervals$end.data[i]])))
         # For diffractometry algorithms to have a chance to work, xy data should be sorted by increasing x
         data.exp <- arrange(data.exp, x)
      }
   }
   data.exp$idunique <- paste(data.exp$sampleid, data.exp$spectrum, sep = "-")
   # return
   return(data.exp)
}