
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
