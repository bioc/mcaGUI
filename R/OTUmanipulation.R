

OTUbasereader=function() {

  defHandler = function(h,...) {}

    openHandler = function(h,...) {
      visible(wOpen) <- TRUE
    }

    closeHandler = function(h,...) {
      visible(wOpen) <- FALSE
    }

    wOpen = gwindow("Open Files", visible=TRUE)
    g1Tbl = glayout(cont=wOpen)

    g1Tbl[1,1] <- glabel("Object Name (required)", cont=g1Tbl)
    g1Tbl[1,2] <- (objectName = gedit("OTUset", cont=g1Tbl))

    g1Tbl[2,1] <- glabel("OTU File (Required)", cont=g1Tbl)
    g1Tbl[2,2] <- (OTUselect = gfilebrowse (text = "", type = "open", quote = FALSE,
    container = g1Tbl, toolkit = guiToolkit(), handler= defHandler))
    g1Tbl[2,3] <- glabel(".list file", cont=g1Tbl)

    setwdHandler = function(h,...) {
      OTUwin <- unlist(strsplit(svalue(OTUselect), split="\\\\"))
      OTUmac <- unlist(strsplit(svalue(OTUselect), split="/"))

      if(length(OTUmac) == 1) {
      }
      if(length(OTUmac) != 1) {
        OTUfilename = OTUmac[length(OTUmac)]
        reduced = OTUmac[-length(OTUmac)]
        path = paste(reduced, collapse="/")
        setwd(path)
      }

      if(length(OTUwin) == 1) {
      }
      if(length(OTUwin) != 1) {
        OTUfilename = OTUwin[length(OTUwin)]
        reduced = OTUwin[-length(OTUwin)]
        path = paste(reduced, collapse="/")
        setwd(path)
      }
    }

  addHandlerChanged(OTUselect, handler = setwdHandler )

  g1Tbl[3,1] <- glabel("Sample File (Required)", cont=g1Tbl)
  g1Tbl[3,2] <- (SAMPLEselect = gfilebrowse (text = "", type = "open", quote = FALSE,
    container = g1Tbl, toolkit = guiToolkit(), handler= defHandler))
  g1Tbl[3,3] <- glabel(".groups", cont=g1Tbl, expand=FALSE)

  g1Tbl[4,1] <- glabel("Fasta File", cont=g1Tbl)
  g1Tbl[4,2] <- (FASTAselect = gfilebrowse (text = "", type = "open", quote = FALSE,
    container = g1Tbl, toolkit = guiToolkit(), handler= defHandler))
  g1Tbl[4,3] <- glabel(".fasta", cont=g1Tbl)

  g1Tbl[5,1] <- glabel("Quality File", cont=g1Tbl)
  g1Tbl[5,2] <- (QUALselect = gfilebrowse (text = "", type = "open", quote = FALSE,
    container = g1Tbl, toolkit = guiToolkit(), handler= defHandler))
  g1Tbl[5,3] <- glabel(".qual", cont=g1Tbl)

  g1Tbl[6,1] <- glabel("Sample Data", cont=g1Tbl)
  g1Tbl[6,2] <- (SAMPLEDATAselect = gfilebrowse (text = "", type = "open", quote = FALSE,
    container = g1Tbl, toolkit = guiToolkit(), handler= defHandler))
  g1Tbl[6,3] <- glabel("Sample Meta Data File or sampleADF", cont=g1Tbl)

  #g1Tbl[7,2] <- glabel("Sample Column", cont=g1Tbl)
  #g1Tbl[7,3] <- (SAMPLEDATAcolumn = gedit("", cont=g1Tbl))

  g1Tbl[7,1] <- glabel("Feature Data", cont=g1Tbl)
  g1Tbl[7,2] <- (ASSIGNMENTselect = gfilebrowse (text = "", type = "open", quote = FALSE,
    container = g1Tbl, toolkit = guiToolkit(), handler= defHandler))
  g1Tbl[7,3] <- glabel("Assigntment Data File or assignmentADF", cont=g1Tbl)

  g1Tbl[8,1] <- glabel("rdp", cont=g1Tbl)
  g1Tbl[8,2] <- (RDPselect = gradio(items=c("TRUE", "FALSE"), selected=2))
  g1Tbl[8,3] <- glabel("Set to TRUE if the Feature Data is a .rep.fix.tax file", cont=g1Tbl)

  #g1Tbl[9,2] <- glabel("Feature Column", cont=g1Tbl)
  #g1Tbl[9,3] <- (ASSIGNMENTcolumn = gedit("", cont=g1Tbl))

  g1Tbl[9,1] <- glabel("Level", cont=g1Tbl)
  g1Tbl[9,2] <- (level = gedit("0.03", cont=g1Tbl))

  g1Tbl[10,2] <- gbutton("cancel", cont=g1Tbl, expand=TRUE, handler=closeHandler)

  OTUbase = function(h,...) {

    OTUwin <- unlist(strsplit(svalue(OTUselect), split="\\\\"))
    OTUmac <- unlist(strsplit(svalue(OTUselect), split="/"))

    SAMPLEwin <- unlist(strsplit(svalue(SAMPLEselect), split="\\\\"))
    SAMPLEmac <- unlist(strsplit(svalue(SAMPLEselect), split="/"))

    FEATUREwin <- unlist(strsplit(svalue(ASSIGNMENTselect), split="\\\\"))
    FEATUREmac <- unlist(strsplit(svalue(ASSIGNMENTselect), split="/"))

    QUALwin <- unlist(strsplit(svalue(QUALselect), split="\\\\"))
    QUALmac <- unlist(strsplit(svalue(QUALselect), split="/"))

    FASTAwin <- unlist(strsplit(svalue(FASTAselect), split="\\\\"))
    FASTAmac <- unlist(strsplit(svalue(FASTAselect), split="/"))

    PHENOwin <- unlist(strsplit(svalue(SAMPLEDATAselect), split="\\\\"))
    PHENOmac <- unlist(strsplit(svalue(SAMPLEDATAselect), split="/"))


    if(length(OTUmac) == 1) {
    }
    if(length(OTUmac) != 1) {
      OTUfilename = OTUmac[length(OTUmac)]
      reduced = OTUmac[-length(OTUmac)]
      path = paste(reduced, collapse="/")
    }

    if(length(OTUwin) == 1) {
    }
    if(length(OTUwin) != 1) {
      OTUfilename = OTUwin[length(OTUwin)]
      reduced = OTUwin[-length(OTUwin)]
      path = paste(reduced, collapse="/")
    }

    if(length(SAMPLEmac) == 1) {
    }
    if(length(SAMPLEmac) != 1) {
      SAMPLEfilename = SAMPLEmac[length(SAMPLEmac)]
      reduced = SAMPLEmac[-length(SAMPLEmac)]
    }

    if(length(SAMPLEwin) == 1) {
    }
    if(length(SAMPLEwin) != 1) {
      SAMPLEfilename = SAMPLEwin[length(SAMPLEwin)]
      reduced = SAMPLEwin[-length(SAMPLEwin)]
    }

    if(length(FASTAmac) == 1) {
    }
    if(length(FASTAmac) != 1) {
      FASTAfilename = FASTAmac[length(FASTAmac)]
      reduced = FASTAmac[-length(FASTAmac)]
    }

    if(length(FASTAwin) == 1) {
    }
    if(length(FASTAwin) != 1) {
      FASTAfilename = FASTAwin[length(FASTAwin)]
      reduced = FASTAwin[-length(FASTAwin)]
    }

    if(length(QUALmac) == 1) {
    }
    if(length(QUALmac) != 1) {
      QUALfilename = QUALmac[length(QUALmac)]
      reduced = QUALmac[-length(QUALmac)]
    }

    if(length(QUALwin) == 1) {
    }
    if(length(QUALwin) != 1) {
      QUALfilename = QUALwin[length(QUALwin)]
      reduced = QUALwin[-length(QUALwin)]
    }

    if(length(PHENOmac) == 1) {
    }
    if(length(PHENOmac) != 1) {
      PHENOfilename = PHENOmac[length(PHENOmac)]
      reduced = PHENOmac[-length(PHENOmac)]
    }

    if(length(PHENOwin) == 1) {
    }
    if(length(PHENOwin) != 1) {
      PHENOfilename = PHENOwin[length(PHENOwin)]
      reduced = PHENOwin[-length(PHENOwin)]
    }

    if(length(FEATUREmac) == 1) {
    }
    if(length(FEATUREmac) != 1) {
      FEATUREfilename = FEATUREmac[length(FEATUREmac)]
      reduced = FEATUREmac[-length(FEATUREmac)]
    }

    if(length(FEATUREwin) == 1) {
    }
    if(length(FEATUREwin) != 1) {
      FEATUREfilename = FEATUREwin[length(FEATUREwin)]
      reduced = FEATUREwin[-length(FEATUREwin)]
    }

    if(length(FASTAfilename) == 0 && length(QUALfilename) != 0 ){
      #qual file without fasta
      print("error1")
    }

    if (length(OTUfilename) == 0) {
      print("error2")
    }

    if (length(SAMPLEfilename) == 0) {
      print("error")
    }

    if(length(OTUfilename) != 0 && length(QUALfilename) == 0 &&
    length(FASTAfilename) == 0 && length(PHENOfilename) == 0 &&
    length(FEATUREfilename) == 0 && length(SAMPLEfilename) != 0)
    {
      print("1")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), OTUset <<- readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename), envir = .GlobalEnv)
    }

    if(length(QUALfilename) == 0 && length(PHENOfilename) == 0 &&
    length(FEATUREfilename) == 0 && length(OTUfilename) != 0 &&
    length(SAMPLEfilename) != 0 && length(FASTAfilename) != 0)
    {
      print("2")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), OTUset <<- readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      fastafile=FASTAfilename), envir = .GlobalEnv)
    }

    if(length(QUALfilename) == 0 && length(FEATUREfilename) == 0 &&
    length(FASTAfilename) == 0 && length(OTUfilename) != 0 &&
    length(SAMPLEfilename) != 0 && length(PHENOfilename) != 0)
    {
      print("3")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      sampleADF=PHENOfilename), envir = .GlobalEnv)
    }

    if(length(QUALfilename) == 0 && length(FASTAfilename) == 0 &&
    length(FEATUREfilename) == 0 && length(OTUfilename) != 0 &&
    length(SAMPLEfilename) != 0 && length(PHENOfilename) != 0)
    {
      print("4")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      sampleADF=PHENOfilename), envir = .GlobalEnv)
    }

    if(length(QUALfilename) == 0 && length(FASTAfilename) == 0 &&
    length(PHENOfilename) == 0 && length(OTUfilename) != 0 &&
    length(SAMPLEfilename) != 0 && length(FEATUREfilename) != 0)
    {
      print("5")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      assignmentADF=FEATUREfilename, rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }

    if(length(QUALfilename) == 0 && length(FASTAfilename) == 0 &&
    length(PHENOfilename) == 0 && length(OTUfilename) == 0 &&
    length(SAMPLEfilename) == 0 && length(FEATUREfilename) != 0)
    {
      print("6")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      assignmentADF=FEATUREfilename, rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }

    if(length(QUALfilename) == 0 && length(FASTAfilename) == 0 &&
    length(OTUfilename) != 0 && length(SAMPLEfilename) != 0 &&
    length(FEATUREfilename) != 0 && length(PHENOfilename) != 0)
    {
      print("7")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      assignmentADF=FEATUREfilename,
      sampleADF=PHENOfilename,
      rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }

    if(length(QUALfilename) == 0 && length(FASTAfilename) == 0 &&
    length(OTUfilename) != 0 && length(SAMPLEfilename) != 0 &&
    length(FEATUREfilename) != 0 && length(PHENOfilename) != 0)
    {
      print("8")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      assignmentADF=FEATUREfilename,
      sampleADF=PHENOfilename,
      rdp=svalue(RDPselect)), envir = .GlobalEnv)
   }

    if(length(QUALfilename) == 0 && length(FASTAfilename) == 0 &&
    length(OTUfilename) != 0 && length(SAMPLEfilename) != 0 &&
    length(FEATUREfilename) != 0 && length(PHENOfilename) != 0)
    {
      print("9")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      assignmentADF=FEATUREfilename,
      sampleADF=PHENOfilename,
      rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }

    if(length(QUALfilename) == 0 && length(FASTAfilename) == 0 &&
    length(OTUfilename) != 0 && length(SAMPLEfilename) != 0 &&
    length(FEATUREfilename) != 0 && length(PHENOfilename) != 0)
    {
      print("10")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      assignmentADF=FEATUREfilename,
      sampleADF=PHENOfilename, rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }


    if(length(PHENOfilename) == 0 && length(FEATUREfilename) == 0 &&
    length(OTUfilename) != 0 && length(SAMPLEfilename) != 0 &&
    length(QUALfilename) != 0 && length(FASTAfilename) != 0)
    {
      print("11")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      qualfile=QUALfilename,
      fastafile=FASTAfilename), envir = .GlobalEnv)
    }

    if(length(QUALfilename) == 0 && length(PHENOfilename) == 0 &&
    length(OTUfilename) != 0 && length(SAMPLEfilename) != 0 &&
    length(FEATUREfilename) != 0 && length(FASTAfilename) != 0)
    {
      print("12")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      assignmentADF=FEATUREfilename,
      fastafile=FASTAfilename,
      rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }

    if(length(QUALfilename) == 0 && length(PHENOfilename) == 0 &&
    length(OTUfilename) != 0 && length(SAMPLEfilename) != 0 &&
    length(FEATUREfilename) != 0 && length(FASTAfilename) != 0)
    {
      print("13")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      assignmentADF=FEATUREfilename,
      fastafile=FASTAfilename, rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }

    if(length(SAMPLEfilename) != 0 && length(OTUfilename) != 0 &&
    length(QUALfilename) != 0 && length(FASTAfilename) != 0 &&
    length(PHENOfilename) != 0 && length(FEATUREfilename) != 0)
    {
      print("14")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      qualfile=QUALfilename, fastafile=FASTAfilename,
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      sampleADF=PHENOfilename,
      assignmentADF=FEATUREfilename,
      rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }

    if(length(SAMPLEfilename) != 0 && length(OTUfilename) != 0 &&
    length(QUALfilename) != 0 && length(FASTAfilename) != 0 &&
    length(PHENOfilename) != 0 && length(FEATUREfilename) != 0)
    {
      print("15")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      qualfile=QUALfilename, fastafile=FASTAfilename,
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      sampleADF=PHENOfilename,
      assignmentADF=FEATUREfilename,
      rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }

    if(length(SAMPLEfilename) != 0 && length(OTUfilename) != 0 &&
    length(QUALfilename) != 0 && length(FASTAfilename) != 0 &&
    length(PHENOfilename) != 0 && length(FEATUREfilename) != 0)
    {
      print("16")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      qualfile=QUALfilename, fastafile=FASTAfilename,
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      sampleADF=PHENOfilename,
      assignmentADF=FEATUREfilename,
      rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }

    if(length(SAMPLEfilename) != 0 && length(OTUfilename) != 0 &&
    length(QUALfilename) != 0 && length(FASTAfilename) != 0 &&
    length(PHENOfilename) != 0 && length(FEATUREfilename) != 0)
    {
      print("17")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      qualfile=QUALfilename, fastafile=FASTAfilename,
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      sampleADF=PHENOfilename,
      assignmentADF=FEATUREfilename,
      rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }

    if(length(SAMPLEfilename) != 0 && length(OTUfilename) != 0 &&
    length(QUALfilename) == 0 && length(FASTAfilename) != 0 &&
    length(PHENOfilename) != 0 && length(FEATUREfilename) != 0)
    {
      print("18")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      fastafile=FASTAfilename,
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      sampleADF=PHENOfilename,
      assignmentADF=FEATUREfilename,
      rdp=svalue(RDPselect)), envir = .GlobalEnv)
    }


    if(length(SAMPLEfilename) != 0 && length(OTUfilename) != 0 &&
    length(QUALfilename) == 0 && length(FASTAfilename) != 0 &&
    length(PHENOfilename) != 0 && length(FEATUREfilename) == 0)
    {
      print("19")
      workMessage = "print('Reading in the data set.  Depending on the size of the data set this may take anywhere from 10-15 minutes.')"
      svalue(send) <- workMessage
      assign(as.character(svalue(objectName)), readOTUset(dirPath=path, level=svalue(level),
      fastafile=FASTAfilename,
      otufile=OTUfilename,
      samplefile=SAMPLEfilename,
      sampleADF=PHENOfilename,), envir = .GlobalEnv)
    }
    #assign(as.character(paste(svalue(objectName), "slots", sep="_")), list(NULL, id = get(svalue(objectName))@id, otuID = get(svalue(objectName))@otuID, sampleID = get(svalue(objectName))@sampleID, assignmentData = get(svalue(objectName))@assignmentData, sampleData = get(svalue(objectName))@sampleData), envir = .GlobalEnv)
    #print(length(FEATUREfilename))
    if(length(FEATUREfilename) != 0){
      assign(as.character(paste(svalue(objectName), "assignmentData", sep="_")), aData(eval(as.symbol(svalue(objectName)))), envir = .GlobalEnv)
    }
#print(length(PHENOfilename))
    if(length(PHENOfilename) != 0){
      assign(as.character(paste(svalue(objectName), "sampleData", sep="_")), sData(eval(as.symbol(svalue(objectName)))), envir = .GlobalEnv)
    }

    visible(wOpen) <- FALSE
  }
  g1Tbl[10,1] <- gbutton("ok", cont=g1Tbl, handler = OTUbase)
}
#-----------------------------------------------

Proportion_Or_Count_Reader = function() {

  .readRDP<-function(ADF, dirPath, id, otuid){
    message("Note: Classification file must be RDP's fixed taxonomy.")
    adf<-read.table(file=dirPath, head=F, sep="\t", stringsAsFactors=F)

    ## get rid of NA columns in RDP output. I'm not sure why they are there.
    adf$V3<-"Root"
    adf$V4<-"norank"
    adf$V5<-"1.0"

    # strips off extra numbers on the sequence ID added by Mothur in versions before 1.13.0
    adf$V1<-sapply(strsplit(as.character(adf$V1),split="\\|"), function(i) i[1])

    ## get rid of useless "-" column
    adf[,-2]->adf

    ## label column names
    colnames(adf)<-c("otuID", "root", " ", "root_score", "domain", " ", "domain_score", "phylum", " ", "phylum_score", "class", " ", "class_score", "order", " ", "order_score", "family", " ", "family_score", "genus", " ", "genus_score")

    ## get rid of crap columns
    adf[,-c(3,6,9,12,15,18,21)]->adf

    ## match rdp seqnames with seqnames in object, then replace seq name with otuid
    if (!missing(otuid)) adf$otuID<-otuid[match(adf$otuID, id)]

    adf[,1]->row.names(adf)
    adf[,-1]->adf
    return(adf)
  }


  defHandler = function(h,...) {}

  openHandler = function(h,...) {
    visible(wOpen) <- TRUE
  }

  closeHandler = function(h,...) {
    visible(wOpen) <- FALSE
  }

  wOpen = gwindow("Open Files", visible=TRUE)
  g1Tbl = glayout(cont=wOpen)

  g1Tbl[1,1] <- glabel("Assign Abundance Table To:", cont=g1Tbl)
  g1Tbl[1,2] <- (assignTo = gedit("Abundance_Table", cont=g1Tbl))

  g1Tbl[2,1] <- glabel("Proportion or Count Data", cont=g1Tbl)
  g1Tbl[2,2] <- (abundanceTable = gfilebrowse (text = "", type = "open", quote = FALSE,
  container = g1Tbl, toolkit = guiToolkit(), handler= defHandler))

  g1Tbl[3,1] <- glabel("header", cont=g1Tbl)
  g1Tbl[3,2] <- (head <- gradio(items=c(TRUE, FALSE), container=g1Tbl))
  g1Tbl[3,3] <- glabel("separation", cont=g1Tbl)
  g1Tbl[3,4] <- (sep = gedit(",", cont=g1Tbl))

  g1Tbl[6,1] = (create_tables<- gbutton(text="Read in Sample Meta Data or Assignment/Feature Data", container=g1Tbl))

  createAbundanceTable <- function(h,...) {
    checkAssignment = mget("collab2", envir=.GlobalEnv, mode = "any", ifnotfound = list(NA), inherits = FALSE)


    if(is.na(as.list(checkAssignment[1]))) {
      assign(as.character(svalue(assignTo)), read.table(file=svalue(abundanceTable), header=eval(svalue(head)=="TRUE"), sep=svalue(sep)), envir = .GlobalEnv)
      } else {
      sampleCol = c("X",as.vector(Sample_Data[,svalue(collab2)]))
      assign(as.character(svalue(assignTo)), read.table(file=svalue(abundanceTable), header=FALSE, col.names=sampleCol, sep=svalue(sep), skip=1), envir = .GlobalEnv)
    }

  }

  g1Tbl[9,1] = (createAbundance <- gbutton(text="Create Abundance Table", container=g1Tbl))
  addHandlerClicked(createAbundance, handler = createAbundanceTable, action= NULL)

  new.window = function(h,...) {
    closeHandler2 = function(h,...) {
      visible(wOpen2) <- FALSE
    }

    wOpen2 <- gwindow("Read in Sample and Assignment File", visible=TRUE)

    g2Tbl = glayout(cont=wOpen2)

    g2Tbl[1,1] <- glabel("*Note:  The sample and assignment data file must be tab delimeted", cont=g2Tbl)

    g2Tbl[2,1] <- glabel("Assign Sample/Meta Data To:", cont=g2Tbl)
    g2Tbl[2,2] <- (assignment2 = gedit("Sample_Data", cont=g2Tbl))

    g2Tbl[3,1] <- glabel("Sample/Meta Data", cont=g2Tbl)
    g2Tbl[3,2] <- (abundanceTable2 = gfilebrowse (text = "", type = "open", quote = FALSE,
    container = g1Tbl, toolkit = guiToolkit(), handler= defHandler))

    g2Tbl[4,1] <- glabel("header", cont=g2Tbl)
    g2Tbl[4,2] <- (head2 <- gradio(items=c(TRUE, FALSE), container=g2Tbl))

    g2Tbl[6,1] <- glabel("Assign Assignment/Feature Data To:", cont=g2Tbl)
    g2Tbl[6,2] <- (assignment3 = gedit("Assignment_Data", cont=g2Tbl))

    g2Tbl[7,1] <- glabel("Assignment/Feature Data", cont=g2Tbl)
    g2Tbl[7,2] <- (abundanceTable3 = gfilebrowse (text = "", type = "open", quote = FALSE,
    container = g1Tbl, toolkit = guiToolkit(), handler= defHandler))

    g2Tbl[8,1] <- glabel("header", cont=g2Tbl)
    g2Tbl[8,2] <- (head3 <- gradio(items=c(TRUE, FALSE), container=g2Tbl))

    g2Tbl[9,1] <- glabel("RDP", cont=g2Tbl)
    g2Tbl[9,2] <- (RDP <- gradio(items=c(TRUE, FALSE), container=g2Tbl))

    createAssignmentData = function(h,...) {
      assign(as.character(svalue(assignment2)), read.table(file=svalue(abundanceTable2), header=eval(svalue(head2)=="TRUE"), sep="\t"), envir = .GlobalEnv)
      if(svalue(RDP) == "TRUE") {
        assign(as.character(svalue(assignment3)), .readRDP(dirPath=svalue(abundanceTable3)), envir = .GlobalEnv)
      } else {
        assign(as.character(svalue(assignment3)), read.table(file=svalue(abundanceTable3), header=eval(svalue(head3)=="TRUE"), sep="\t"), envir = .GlobalEnv)
      }
     #g1Tbl[7,1] <- glabel("test", cont=g1Tbl)
     assignmentName = names(eval(as.symbol(svalue(assignment3))))
     sampleName = names(eval(as.symbol(svalue(assignment2))))

     g1Tbl[8,1] = (glabel(text="Sample/Meta Data Column", container=g1Tbl))
     g1Tbl[8,2] = (collab2 <<- gdroplist(items=sampleName, container=g1Tbl))
     closeHandler2()
    }

    g2Tbl[10,1] = (sample_assign_button <- gbutton(text="Create Sample Data and Assignment Data Tables", container=g2Tbl))
    addHandlerClicked(sample_assign_button, handler = createAssignmentData, action= NULL)
  }

  addHandlerClicked(create_tables, handler = new.window, action= NULL)

}

#Proportion_Or_Count_Reader()

