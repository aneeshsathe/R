# open excel files in folders
#clear
rm(list=ls()) 
##############
#set paths

root<-"Y:/Data/paxillin mutants/SD_20140223_pax_s722_s274_s2a_s2d"

#details_path<-c(Sys.glob(file.path(root, "axis_*.txt", fsep = .Platform$file.sep),dirmark = TRUE))


##### 
# to read details from a text file
# axis_details=data.frame(read.table(details_path,
#                                    sep="\t", stringsAsFactors =FALSE,
#                                    header = TRUE))
# 
# 
# in_sheet_name<-axis_details$sheet.name[1]
# 
# out_plot_title<-axis_details$plot.title[1]
# 
# y_axis_lbl<-axis_details$y.axis.lbl
# x_axis_lbl<-axis_details$x.axis.lbl
# 
# sample_names<-axis_details$sample.names
#####
# root<-getwd()
in_sheet_name<-"Sheet2"
out_path<-file.path(root, paste("analysis",Sys.Date(),sep=""))

# #sample_names used to store names of samples
# # out_plot_title<-"Cycloheximide 100\U03BCg/ml 13.5hr\n Actinomycin D 50\U03BCg/ml 13 h"
out_plot_title<-"Paxillin S272 and S274\n double mutant"
# 
x_axis_lbl<-"Nuclear/Total Intensity Ratio"
y_axis_lbl<-c("WT Paxillin", "WT Paxillin\n+LMB",
              "S272A S274A", "S272A S274A\n+LMB",
              "S272D S274D", "S272D S274D\n+LMB"             
              
              )
#y_axis_scale<-seq(0,0.5,0.10)
sample_names<-c("wt.pax", "wt.pax.LMB",
                "s272/4a", "s272/4a.LMB",
                "s272/4d", "s272/4d.LMB"
                
                )
out_font_size<-65

#########
#initialize libraries
library(xlsx) # req to read xls files
library(psych)# req for stats
library(reshape2)#req to melt
library(ggplot2)# req for plotting
library(gridExtra)#req for adding table to plot
source("C:/Users/Aneesh/Dropbox/Aneesh_R/functions/multiplot.R")#for laptop
source("C:/Users/Aneesh/Dropbox/Aneesh_R/functions/summarySE.R")#for laptop
#source("C:/Users/g0901121/Documents/Dropbox/Aneesh_R/functions/multiplot.R")#for desktop
#source("C:/Users/g0901121/Documents/Dropbox/Aneesh_R/functions/summarySE.R")#for lab desktop

######
### READ DATA

fold_list<-dir(root)
all_data<-data.frame()

for(count in 1:length(y_axis_lbl)){
  #below line to read xls files
  excel_path<-c(Sys.glob(file.path(root,fold_list[count], "results_*","*.xlsx", fsep = .Platform$file.sep),dirmark = TRUE))
  #below line to read txt files
  #excel_path<-c(Sys.glob(file.path(root,fold_list[count], "results_*","*.txt", fsep = .Platform$file.sep),dirmark = TRUE))
  if (length(excel_path)!=0){
    pro_mydata <- read.xlsx(excel_path, sheetName=in_sheet_name)#for xls files
    #pro_mydata <- read.table(excel_path, header=TRUE, sep = ",")
  }
  #melt data
  colnames(pro_mydata)[1]<-sample_names[count]
  names(pro_mydata)<-tolower(names(pro_mydata))
  pro_mydata.melt<-melt(pro_mydata, id=2:length(pro_mydata))
  #change colnames before rbind
  colnames(pro_mydata.melt)[c(length(pro_mydata.melt)-1):c(length(pro_mydata.melt))]<-c("sample","cellid")
  #rbind
  all_data<-rbind(all_data,pro_mydata.melt)
  
}

# summarize data
all_data.stats <- summarySE(all_data, measurevar="ratio", groupvars=c("sample"))
all_data.stats$ratio<-signif(all_data.stats$ratio,digits=4)
################# 
# Calculate p-values
attach(all_data)
# no adjustments
# p_val_out<-pairwise.t.test(ratio,sample,p.adjust.method="none",paired=FALSE, pool.sd=FALSE)
#holm method of adjustment
p_val_out<-pairwise.t.test(ratio,sample,p.adjust.method="holm",paired=FALSE, pool.sd=FALSE)
detach()
p_table<-signif(data.frame(p_val_out$p.value), digits = 4)

p_star<-data.frame(matrix(NA, nrow = dim(p_table)[1], ncol = dim(p_table)[2]))
p_star<-p_table
names(p_star)<-names(p_table)
row.names(p_star)<-row.names(p_table)
p_star[p_table <= .05]  <- "*"
p_star[p_table <= .01]  <- "**"
p_star[p_table <= .001] <- "***"

#Options for table on plot uncomment as seen fit
# p_star_table<-rbind(p_table,p_star)
# p_star_table<-p_table
p_star_table<-p_star


#######
# PLOT
out_plot2<-ggplot(all_data.stats,aes(x=sample,y=ratio, stat="identity",
                                     label=ratio,fill=sample))+
  #actual plot
  #geom_bar(alpha=0.5)+
  geom_boxplot(data=all_data,mapping=aes(x=sample,y=ratio),notch = TRUE, notchwidth = .3,
               outlier.colour = "green", outlier.size = 3)+
  geom_text(position = position_dodge(width=0.3),size=14,hjust=-0.5)+
  #geom_errorbar(aes(ymax=ratio+se,ymin=ratio-se),width=0.3,size=1)+
  scale_fill_brewer("sample",palette="Paired")+
  
  #jitter plot
  layer(data=all_data,mapping=aes(x=sample,y=ratio),geom="jitter",
        stat="identity",size=3.5,alpha=0.7,colour=factor("sample"))+
  
  #themes
  theme_bw(base_size = out_font_size, base_family = "serif")+
  theme(legend.position = "none") +
  ylab(x_axis_lbl)+ #ylim(0,1000) +#ylim(0,0.5)+
  xlab(NULL)+
  ggtitle(out_plot_title) + 
  scale_x_discrete(breaks=c(levels(all_data$sample)), 
                   labels=y_axis_lbl)+
  #scale_y_continuous(breaks=y_axis_scale)+
  coord_flip()

#####
# Write plot
dir.create(out_path)# create out folder
png(file.path(out_path, paste("out_plot",".png",sep="")),width=1200,height=1200,
    type = "windows",bg = "transparent")
print(out_plot2)
dev.off()
#write p-values
png(file.path(out_path, paste("out_p_table",".png",sep="")),width=850,height=300,
    type = "windows",bg = "transparent")
print(grid.table(p_star_table,
                 gpar.coretext=gpar(fontsize=24), 
                 gpar.coltext=gpar(fontsize=24,fontface = "bold"), 
                 gpar.rowtext=gpar(fontsize=24,fontface = "bold")))
dev.off()

save.image(file = file.path(out_path, paste("RAnalysis",".RData",sep="") ))
unlink(".RData")