#source("/home/taha/chepec/chetex/common/R/common/int2padstr.R")

xpspkWrapper <- 
   function(data.exp, 
            run, 
            override = FALSE, 
            kerpk = 1, 
            fitmaxiter = 25, 
            gam = 1, 
            scl.factor = 1.2, 
            tau = 2.5,
            maxwdth = 10) { 
   # the override flag IS IN USE
      
   print("... Started xpspkWrapper")
      
   # check if xpspk has already completed successfully for the current job
   current.dirname <- getwd()
   print(current.dirname)
   #current.filename <- paste("xps-peak-data-", int2padstr(ii = run, pchr = "0", w = 3), ".rda", sep = "")
   current.filename <- "xps-peak-data.rda"
   xpsdatafile <- paste(current.dirname, current.filename, sep = "/")
   
   
   
   if (file.exists(xpsdatafile) && !override) {
      # If file does exist AND override flag is FALSE
      print("... Started if-clause 1")
      
      # Load the existing data from file
      load(file = xpsdatafile)
      
      # Only run the peak-fitting algorithm if 
      # <run> is higher than what the file contains
      if (run > length(xpsres)) {
         print("... Started if-clause 1:1")
         xpsres[[run]] <- xpspk(data.exp, 
                                kerpk = kerpk, 
                                fitmaxiter = fitmaxiter, 
                                gam = gam, 
                                scl.factor = scl.factor,
                                tau = tau,
                                maxwdth = maxwdth)
         # I wonder if saving overwriting the previously saved data
         # (run < length(xrfres))?
         save(xpsres, file = xpsdatafile)
         print("... Ended if-clause 1:1")
      }
      
      print("... Ended if-clause 1")
   }
   if (file.exists(xpsdatafile) && override) {
      # If file does exist AND override flag is TRUE
      print("... Started if-clause 2")
      
      # Load the existing data from file
      load(file = xpsdatafile)
      
      xpsres[[run]] <- xpspk(data.exp, 
                             kerpk = kerpk, 
                             fitmaxiter = fitmaxiter, 
                             gam = gam, 
                             scl.factor = scl.factor,
                             tau = tau,
                             maxwdth = maxwdth)
      save(xpsres, file = xpsdatafile)
      print("... Ended if-clause 2")
   }
   # If the file does not exist, 
   # it doesn't really matter what the override flag says
   if (!file.exists(xpsdatafile)) {
      print("... Started if-clause 3")
      
      xpsres <- list()
      print("... xpsres list created")
      
      # Need to call xpspk() and save its results to file as above
      xpsres[[run]] <- xpspk(data.exp, 
                             kerpk = kerpk, 
                             fitmaxiter = fitmaxiter, 
                             gam = gam, 
                             scl.factor = scl.factor,
                             tau = tau,
                             maxwdth = maxwdth)
      save(xpsres, file = xpsdatafile)
      print("... Ended if-clause 3")
   }
   return(xpsres)      
}
