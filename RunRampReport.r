##############################################################################
#this script creates a standard creel report from a command line using rscript
##############################################################################
#
# arguments (in order, directories must use double slashes)
#
#arg[1] = output directory
#arg[2] = file name for report output
#arg[3] = directory that has reporting scripts.....also working directory for r
#arg[4] = format for report output..."pdf" or "htm" or "docs" or "all"
#arg[5] = creel UID to create report from
##############################################################################


options(warn = -1,stringsAsFactors = FALSE,scipen = 3)

library("knitr")
library("markdown")
library("pander")
library("rmarkdown")

#Get CommandLine Arguements
#args <- commandArgs(trailingOnly = TRUE)
rd<-getwd() #output directory
file_name<-"ramps" #file name for report output
#file_name<-str_trim(args[2]) #file name for report output
#wd<-args[3] #working directory for r...also directory that has reporting scripts
reportFormat<-"pdf" #format for report output
#reportFormat<-args[4] #format for report output
#rm(args)

#Sys.getenv("RSTUDIO_PANDOC")
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
render("ramps.Rmd")

#create .md file using knitr
#setwd(wd)
#knit("ramps.Rmd", output=paste(rd,"\\",file_name, ".md",sep=""))
#setwd(rd)

#create document files as per format arguement
#if (reportFormat=="pdf" || reportFormat=="all") Pandoc.convert(f=paste(file_name,".md",sep=""), format="pdf", options="-S", open=FALSE)
#if (reportFormat=="htm" || reportFormat=="all") Pandoc.convert(f=paste(file_name,".md",sep=""), format="htm", options="-S", open=FALSE)
