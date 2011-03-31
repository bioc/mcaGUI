### Define the main menu here

OTUbase.menu=list()
pmg.menu = list()
pmg.menu$File$"Read OTU Base"$handler =
  function(h,...) OTUbasereader()
pmg.menu$File$"Read OTU Base"$icon="file"

pmg.menu$File$"Read Proportion or Count Table"$handler =
  function(h,...) Proportion_Or_Count_Reader()
pmg.menu$File$"Read Proportion or Count Table"$icon="file"

pmg.menu$File$"Read Table"$handler =
 function(h,...) pmg.gw(read.table.list)
pmg.menu$File$"Read Table"$icon="file"


pmg.menu$File$"Save Workspace..."$handler =
  function(h,...) gfile("Save workspace",type="save", action="save.image")
pmg.menu$File$"Save Workspace..."$icon = "save"
pmg.menu$File$"Restore Workspace"$handler =
  function(h, ...) gfile("Restore workspace",type="open", action="load")
pmg.menu$File$"Restore Workspace"$icon = "revert-to-saved"
pmg.menu$File$"Load package..."$handler =
  function(h,...) pmg.loadPackages()
pmg.menu$File$"Install CRAN package..."$handler =
  function(h,...) pmg.installCRANPackage()
pmg.menu$File$"Install CRAN package..."$icon = "network"
pmg.menu$File$"Install local package..."$handler =
  function(h,...) {
    old = options("repos")$repos; options("repos"=NULL);
    gfile("Select a package file...","open",action="install.packages",
          filter=   list(
            "tar.gz files"=list(
              patterns=c("*.tgz","*.tar.gz")
              ),
            "zip files"=list(
              patterns=c("*.zip")
              ),
            "All files"=list(
              patterns=c("*")
              )
            )
          )
    options("repos"=old)
  }
pmg.menu$File$"Install local package..."$icon = "file"
pmg.menu$File$"Set working directory..."$handler =
  function(h,...) gfile("Select a directory","selectdir",action="setwd")
pmg.menu$File$"Set working directory..."$icon = "directory"


pmg.menu$File$"View window list"$handler = function(h,...) pmgWC$show()
pmg.menu$File$"Exit mcaGUI"$handler =
  function(h,...)  {
    dispose(pmg.dialogs.window)
    assignInNamespace("pmg.dialogs.window", NULL,"mcaGUI")
    pmg.closeAll()
  }
pmg.menu$File$"Exit mcaGUI"$icon ="quit"
##
##
pmg.menu$Data$browseEnv$handler =
  function(h,...) browseEnv()
pmg.menu$Data$"Load data set..."$handler =
  function(h,...) pmg.viewDataSets()
pmg.menu$Data$"Import data set..."$handler =
  function(h,...) pmg.specifyFileForImport();
pmg.menu$Data$"Write data as CSV file..."$handler =
  function(h,...) pmg.gw(write.csv.list)
pmg.menu$Data$"Abundance Table"$handler =
  function(h,...) {
  g1 = ggroup(use.scrollwindow=TRUE)
  add(pmg.dialog.notebook, g1, label="Abundance Table", pageno = 3)

  tbl <- glayout(cont=g1, expand=FALSE)

  tbl[1,1] = (glabel(text="object", containter=tbl))
  tbl[1,2] = (object <- gedit(text ="", container=tbl))
  tbl[1,3] = (glabel(text="weighted", container=tbl))
  tbl[1,4] = (weight <- gradio(items=c("TRUE", "FALSE"), container=tbl))

  tbl[2,1] = (glabel(text="Sample data as rows?"))
  tbl[2,2] = (transpose <- gradio(items=c("No", "Yes"), container=tbl))

  tbl[3,1]= (glabel(text="Assign To", container=tbl))
  tbl[3,2] = (assignTo <- gedit(text="", container=tbl))


  tbl[4,1]= (advanced <- gbutton(text="Update Feature and Sample Meta-Data", container=tbl))

  getUpdate <- function(h,...) {
  assignmentName = paste(svalue(object), "_assignmentData", sep="")
  assignmentName = names(eval(as.symbol(assignmentName)))

  sampleName = paste(svalue(object), "_sampleData", sep="")
  sampleName = names(eval(as.symbol(sampleName)))

  tbl[5,1] = (glabel(text="Feature Data Column", container=tbl))
  tbl[5,2] = (assignmentCol <<- gdroplist(items=assignmentName, container=tbl))
  tbl[6,1] = (glabel(text="Sample Meta-Data Column", container=tbl))
  tbl[6,2] = (collab <<- gdroplist(items=sampleName, container=tbl))
  }

  addHandlerClicked(advanced, handler = getUpdate, action= NULL)

  tbl[7,1] = (createAbundance <- gbutton(text="Create Abundance Table", container=tbl))

  createAbundanceTable <- function(h,...) {
    checkAssignment = mget("assignmentCol", envir=.GlobalEnv, mode = "any", ifnotfound = list(NA), inherits = FALSE)

    if(is.na(as.list(checkAssignment[1]))) {
      assign(as.character(svalue(assignTo)), abundance(object=eval(as.symbol(svalue(object))), weighted=svalue(weight), envir = .GlobalEnv))
      } else {
      assign(as.character(svalue(assignTo)), abundance(object=eval(as.symbol(svalue(object))), weighted=svalue(weight), assignmentCol=svalue(assignmentCol), collab = svalue(collab)), envir = .GlobalEnv)
    }

    if(svalue(transpose) == "No") {
      assign(as.character(as.symbol(svalue(assignTo))), eval(as.symbol(svalue(assignTo))), envir = .GlobalEnv)
      #rm(list=c("collab","assignmentCol"), envir = .GlobalEnv)
      } else {
      assign(as.character(as.symbol(svalue(assignTo))), t(eval(as.symbol(svalue(assignTo)))), envir = .GlobalEnv)
    }
  }

  addHandlerClicked(createAbundance, handler = createAbundanceTable, action= NULL)
  }
pmg.menu$Data$"Abundance Table"$icon=""              
## dynamic

pmg.menu$Data$"Dynamic summaries"$handler =
  function(h,...) dSummaryDialog()
pmg.menu$Data$"Dynamic summaries"$icon = "execute"
pmg.menu$Data$"Univariate summaries"$table$handler =
  function(h,...) pmg.gw(table.list)
pmg.menu$Data$"Univariate summaries"$"stem and leaf"$handler =
  function(h,...) pmg.gw(stem.list)
pmg.menu$Data$"Univariate summaries"$"summary"$handler =
  function(h,...) pmg.gw(summary.list)
pmg.menu$Data$"Univariate summaries"$"summary"$icon = "info"
pmg.menu$Data$"Univariate summaries"$mean$handler =
  function(h,...) pmg.gw(mean.list)
pmg.menu$Data$"Univariate summaries"$median$handler =
  function(h,...) pmg.gw(median.list)
pmg.menu$Data$"Univariate summaries"$"standard deviation"$handler =
  function(h,...) pmg.gw(sd.list)
pmg.menu$Data$"Univariate summaries"$IQR$handler =
  function(h,...) pmg.gw(IQR.list)
pmg.menu$Data$"Univariate summaries"$mad$handler =
  function(h,...) pmg.gw(mad.list)
pmg.menu$Data$"Univariate summaries"$quantiles$handler =
  function(h,...) pmg.add(quantileWidget(),label="quantile()") # add here
pmg.menu$Data$"Univariate summaries"$skewness$handler =
  function(h,...) pmg.gw(skewnessList)
pmg.menu$Data$"Univariate summaries"$kurtosis$handler =
  function(h,...) pmg.gw(kurtosisList)
##
pmg.menu$Data$"Bivariate summaries"$correlation$handler =
  function(h,...) pmg.gw(cor.list)
pmg.menu$Data$"Bivariate summaries"$"Cross tabulation"$handler =
  function(h,...) pmg.gw(xtabs.list)
##
pmg.menu$Data$"Nonparametric Bootstrap Summaries"$handler =
  function(h,...) pmg.gw(bootstrap.list)
##
pmg.menu$Data$"Random data"$"Cumulative Probabilities"$handler = function(h,...) 
  add(pmg.dialog.notebook,dpqrfuncs(type="p"),label="p funcs")
pmg.menu$Data$"Random data"$"Probabilities"$handler = function(h,...) 
  add(pmg.dialog.notebook,dpqrfuncs(type="d"),label="d funcs")
pmg.menu$Data$"Random data"$Quantiles$handler = function(h,...) 
  add(pmg.dialog.notebook,dpqrfuncs(type="q"),label="q funcs")
pmg.menu$Data$"Random data"$"Random samples"$handler = function(h,...) 
  add(pmg.dialog.notebook,dpqrfuncs(type="r"),label="r funcs")


pmg.menu$Data$"Random data"$"Sample"$handler =
  function(h,...) pmg.gw(sample.list)
##

##########
## Simulation. What else?
pmg.menu$Data$Simulation$"Repeat trials"$handler = function(h,...) {
  add(pmg.dialog.notebook, repeatTrialsGUI(), label = "Repeat trials")
}




## Manipulate
if("reshape" %in% .packages(TRUE)) {
  pmg.menu$Data$"Manipulate"$reshape$melt$handler = function(h,...) pmg.meltGUI()
  pmg.menu$Data$"Manipulate"$reshape$cast$handler = function(h,...) pmg.castGUI()
}


pmg.menu$Data$"Manipulate"$"subset"$handler =
  function(h,...) add(pmg.dialog.notebook,pmg.subset.dialog(),label="subset()")
pmg.menu$Data$"Manipulate"$"subset"$icon = "subset"
pmg.menu$Data$"Manipulate"$"subset"$handler =
  function(h,...) add(pmg.dialog.notebook,pmg.subset.dialog(),label="subset()")
pmg.menu$Data$"Manipulate"$"stack"$handler =
  function(h,...) pmg.gw(stack.list)
pmg.menu$Data$"Manipulate"$"unstack"$handler =
  function(h,...) pmg.gw(unstack.list)
pmg.menu$Data$"Manipulate"$"Edit data frame properties"$handler =
  function(h,...) add(pmg.dialog.notebook,pmg.edit.dataframe.properties.dialog(),label="edit properties")
pmg.menu$Data$"Manipulate"$"Edit data frame properties"$icon = "properties"
##
pmg.menu$Data$"Coerce"$"as.numeric"$handler =
  function(h,...) pmg.gw(as.numeric.list)
pmg.menu$Data$"Coerce"$"as.character"$handler =
  function(h,...) pmg.gw(as.character.list)
pmg.menu$Data$"Coerce"$"as.data.frame"$handler =
  function(h,...) pmg.gw(as.data.frame.list)
pmg.menu$Data$"Coerce"$"as.matrix"$handler =
  function(h,...) pmg.gw(as.matrix.list)
pmg.menu$Data$"Coerce"$"matrix"$handler =
  function(h,...) pmg.gw(matrix.list)
pmg.menu$Data$"Coerce"$"groupedData"$handler =
  function(h,...) pmg.gw(groupedData.list)
pmg.menu$Data$"Coerce"$"factor"$handler =
  function(h,...) pmg.gw(factor.list)
##
## Plots
## Dynamic widget
pmg.menu$Plots$"Lattice explorer"$handler = function(h,...) {
  dLatticeExplorer(container=pmgWC$new("Lattice explorer", v=T))
}
pmg.menu$Plots$"Lattice explorer"$icon = "execute"
###
pmg.menu$Plots$"Set plot parameters"$Setup$handler =
  function(h,...) pmg.gw(par.setup.list)
pmg.menu$Plots$"Set plot parameters"$Setup$icon = "preferences"
pmg.menu$Plots$"Set plot parameters"$Axes$handler =
  function(h,...) pmg.gw(par.axes.list)
pmg.menu$Plots$"Set plot parameters"$Axes$icon = "preferences"
pmg.menu$Plots$"Set plot parameters"$Colors$handler =
  function(h,...) pmg.gw(par.colors.list)
pmg.menu$Plots$"Set plot parameters"$Colors$icon = "preferences"
pmg.menu$Plots$"Set plot parameters"$Fonts$handler =
  function(h,...) pmg.gw(par.fonts.list)
pmg.menu$Plots$"Set plot parameters"$Fonts$icon = "preferences"
pmg.menu$Plots$"Set plot parameters"$"Number of figures"$handler =
  function(h,...) pmg.gw(par.nofigures.list)
pmg.menu$Plots$"Set plot parameters"$"Number of figures"$icon = "preferences"

##
pmg.menu$Plots$univariate$"barplot"$handler = 
  function(h,...) pmg.gw(barplot.list)
pmg.menu$Plots$univariate$"barplot"$icon="barplot"
pmg.menu$Plots$univariate$"piechart"$handler = 
  function(h,...) pmg.gw(piechart.list)
pmg.menu$Plots$univariate$"boxplot"$handler = 
  function(h,...) pmg.gw(univariate.boxplot.list)
pmg.menu$Plots$univariate$"boxplot"$icon = "boxplot"
pmg.menu$Plots$univariate$"histogram"$handler = 
  function(h,...) pmg.gw(hist.list)
pmg.menu$Plots$univariate$"histogram"$icon ="hist"
pmg.menu$Plots$univariate$"density plot"$handler = 
  function(h,...) pmg.gw(densityplot.list)
pmg.menu$Plots$univariate$"quantile-normal plot"$handler = 
  function(h,...) pmg.gw(qqnorm.list)
pmg.menu$Plots$univariate$"stripchart"$handler = 
  function(h,...) pmg.gw(stripchart.list)
pmg.menu$Plots$univariate$"dotchart"$handler = 
  function(h,...) pmg.gw(dotchart.list)
pmg.menu$Plots$univariate$"ecdf"$handler = 
  function(h,...) pmg.gw(ecdf.list)
##
pmg.menu$Plots$bivariate$"boxplot"$handler = 
  function(h,...) pmg.gw(bivariate.boxplot.list)
pmg.menu$Plots$bivariate$"boxplot"$icon = "boxplot"
pmg.menu$Plots$bivariate$"scatterplot"$handler = 
  function(h,...) pmg.gw(scatterplot.list)
pmg.menu$Plots$bivariate$"scatterplot"$icon = "points"
pmg.menu$Plots$bivariate$"sunflower plot"$handler = 
  function(h,...) pmg.gw(sunflower.list)
pmg.menu$Plots$bivariate$"quantile-quantile plot"$handler = 
  function(h,...) pmg.gw(qqplot.list)
##
pmg.menu$Plots$multivariate$"plot"$handler = 
  function(h,...) pmg.gw(scatterplot.model.list)
pmg.menu$Plots$multivariate$"plot"$icon = "plot"
pmg.menu$Plots$multivariate$"boxplot"$handler = 
  function(h,...) pmg.gw(model.boxplot.list)
pmg.menu$Plots$multivariate$"boxplot"$icon = "boxplot"
pmg.menu$Plots$multivariate$"pairs plot"$handler = 
  function(h,...) pmg.gw(pairs.list)
##
pmg.menu$Plots$"Lattice graphics"$"xyplot"$handler = 
  function(h,...) pmg.gw(xyplot.list)
pmg.menu$Plots$"Lattice graphics"$"dotplot"$handler = 
  function(h,...) pmg.gw(dotplot.list)
pmg.menu$Plots$"Lattice graphics"$"barchart"$handler = 
  function(h,...) pmg.gw(barchart.list)
pmg.menu$Plots$"Lattice graphics"$"stripplot"$handler = 
  function(h,...) pmg.gw(stripplot.list)
pmg.menu$Plots$"Lattice graphics"$"bwplot"$handler = 
  function(h,...) pmg.gw(bwplot.list)
##
pmg.menu$Plots$"Add to graphic"$"points"$handler = 
  function(h,...) pmg.gw(add.points.list)
pmg.menu$Plots$"Add to graphic"$"points"$icon = "points"
pmg.menu$Plots$"Add to graphic"$"lines"$handler = 
  function(h,...) pmg.gw(add.lines.list)
pmg.menu$Plots$"Add to graphic"$"lines"$icon = "lines"
pmg.menu$Plots$"Add to graphic"$"density"$handler = 
  function(h,...) pmg.gw(add.density.list)
pmg.menu$Plots$"Add to graphic"$"curve"$handler = 
  function(h,...) pmg.gw(add.curve.list)
pmg.menu$Plots$"Add to graphic"$"curve"$icon = "curve"
pmg.menu$Plots$"Add to graphic"$"rug"$handler = 
  function(h,...) pmg.gw(rug.list)
pmg.menu$Plots$"Add to graphic"$"title"$handler = 
  function(h,...) pmg.gw(add.title.list)


## iplots conditionally
if("iplots" %in% .packages(TRUE)) {
  pmg.menu$Plots$"iplots"$handler = function(...) {
    pmg.iplots()
  }
}


help.menu=list()
help.menu$Help$"About R"$handler =
  function(h,...) add(pmg.dialog.notebook,pmg.about.R(),label = "About R")
#help.menu$Help$"About PMG"$handler =
  function(h,...) add(pmg.dialog.notebook,pmg.about(),label = "About P M G")
##pmg.about(container=pmgWC$new(v=TRUE))
#help.menu$Help$"About PMG"$icon="about"
help.menu$Help$"R helpbrowser"$handler =
  function(h,...) {
    if(is.null(pmg.helpBrowser.window) ||
       is.invalid(pmg.helpBrowser.window)) {
      assignInNamespace("pmg.helpBrowser.window", ghelpbrowser(),"mcaGUI")
    } else {
      focus(pmg.helpBrowser.window) <- TRUE # will this work
    }
  }
help.menu$Help$"View mcaGUI vignette"$handler = function(h,...) browseURL(system.file("doc","An_Introduction_and_User_Guide_for_mcaGUI.pdf",package="mcaGUI"))
#help.menu$Help$"R Site Search"$handler = function(h,...) RSiteSearch.Dialog()
#help.menu$Help$"View vignettes"$handler = function(h,...) viewVignettes.Dialog()
#help.menu$Help$"View demos"$handler = function(h,...) viewDemos.Dialog()
#help.menu$Help$"View P M G vignette"$handler = function(h,...) print(vignette("pmg",package="pmg"))

pmg.menu$Analysis$"Richness Estimation"$handler =
  function(h,...) pmg.gw(richness.list)
pmg.menu$Analysis$"Richness Estimation"$icon=""
pmg.menu$Analysis$"Diversity Estimation"$handler =
  function(h,...) pmg.gw(diversity.list)
pmg.menu$Analysis$"Diversity Estimation"$icon=""
pmg.menu$Analysis$"Multivariate"$"Cluster Analysis"$handler =
  function(h,...) pmg.gw(cluster.list)
pmg.menu$Analysis$"Multivariate"$"Cluster Analysis"$icon=""

pmg.menu$Analysis$"Multivariate"$"Principle Component Analysis (PCA)"$handler =
  function(h,...) pmg.gw(prcomp.list )
pmg.menu$Analysis$"Multivariate"$"Principle Component Analysis (PCA)"$icon=""

