XtabAttrFittedPeakParams <- 
   ### OLD NAME: ResetTabAttributes
   function(xtab.object, 
            table.caption = "XtabAttrFittedPeakParams caption", 
            table.label = "tab:labelnotset") {
   #####
   caption(xtab.object) <- table.caption
   label(xtab.object) <- table.label
   names(xtab.object) <- 
      c("{Peak}",
        "{Kernel}",
        "{Energy/\\si{\\eV}}",
        "{Height/\\si{\\counts}}",
        "{Area/\\si{\\counts\\eV}}",
        "{FWHM/\\si{\\eV}}",
        "{$m$}",
        "{Accept}")
   digits(xtab.object) <- 
      c(0, #row.names
        2, #peak
        1, #kernel
        6, #energy
        1, #height
        2, #area
        1, #FWHM
        1, #m
        0) #accept
   display(xtab.object) <- 
      c("s", #row.names
        "d", #peak
        "d", #kernel
        "f", #energy
        "e", #height
        "e", #area
        "e", #FWHM
        "f", #m
        "s") #accept
   align(xtab.object) <- 
      c("l", #row.names
        "S[table-format=2.0]", #peak
        "S[table-format=2.0]", #kernel
        "S[table-format=2.6]", #energy
        "S[table-format=1.1e2]", #height
        "S[table-format=1.2e2]", #area
        "S[table-format=1.1e2]", #FWHM
        "S[table-format=3.1]", #m
        "c") #accept
   #
   return(xtab.object)
}
