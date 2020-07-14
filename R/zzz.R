
## called on startup
## .First.lib = function(...) {
##   cat("\n\nP M G\n")
##   cat("To restart pmg, use the command:\n")
##   cat("pmg()\n")

##   pmg()
## }

## when we have a namespace, we can do this.
.onLoad <- function(...){
  cat("Loading mcaGUI()\n")
  
  print("You can access the mcaGUI vignette by browsing to:  ")
  print(system.file("doc","An_Introduction_and_User_Guide_for_mcaGUI.pdf",package="mcaGUI"))
  
  mcaGUI()
}
.onAttach <- function(libname, pkgname) {
    msg <- sprintf(
        "Package '%s' is deprecated and will be removed from Bioconductor
         version %s", pkgname, "3.13")
    .Deprecated(msg=paste(strwrap(msg, exdent=2), collapse="\n"))
}

