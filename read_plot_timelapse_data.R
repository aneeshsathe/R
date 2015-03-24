# open excel files in folders
#clear
rm(list=ls()) 
##############
#set paths
root<-"W:/Data/Paxillin timelapse/SD_20140902_pax_timelapse/pax_timelapse_20140902/results_24-Sep-2014/results_pax_timelapse_20140902.xlsx"


in_sheet_name<-"Sheet2"


#initialize libraries
library(xlsx) # req to read xls files
library(psych)# req for stats
library(reshape2)#req to melt
library(ggplot2)# req for plotting
library(gridExtra)#req for adding table to plot
source("C:/Users/Aneesh/Dropbox/Aneesh_R/functions/multiplot.R")
source("C:/Users/Aneesh/Dropbox/Aneesh_R/functions/summarySE.R")


mydata <- read.xlsx(root, sheetName=in_sheet_name)

m_in_data<-melt(in_data,id=c('Time'),variable.name='stat',na.rm = TRUE)