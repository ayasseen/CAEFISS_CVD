#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Project:      Automation of CVD extract to supplement CAEFISS
# Date created: Feb 21, 2022
# Author:       Abdool Yasseen
# Last updated: Feb 23, 2022
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
################################################################################
# Read in data from L drive folder and extract the raw CVD data
# Takes a long time to read in all 69 excel files (approximatly 1 day)

setwd("L:/COMMON/CIRID/VSS/PUBLIC HEALTH PROTECTION AND PROMOTION HC8/Quality Assurance/Canada Vigilance - Duplicate Check CAEFISS vs CV/Excel files")
EXCELFILE_NAMES = dir()[substr(dir(),0,2)=="CV" | substr(dir(),0,3)=="New"]
# library(readxl) # 5-10 min per file
# 
# for(i in 1:length(EXCELFILE_NAMES)){
#   print(paste(EXCELFILE_NAMES[i], Sys.time()))
#   RAW      = data.frame(read_excel(EXCELFILE_NAMES[i], sheet = "XLS-Raw"))
#   RAW$FILE = EXCELFILE_NAMES[i]
#   if(i==1){RAW2 = list(RAW[grepl("COVID-19", RAW$Product.Indications),]); next}
#   RAW2[[i]] = RAW[grepl("COVID-19", RAW$Product.Indications),]}
# 
# saveRDS(RAW2, "C:/Users/AYASSEEN/Desktop/Secondment/CVD/RAW.rds")

################################################################################
# Read in extracted list of raw CVD data and convert data to wide format

XLS_RAW_22May2021_04Feb2022 = readRDS(file="C:/Users/AYASSEEN/Desktop/Secondment/CVD/RAW-RAW.rds")
XLS_RAW_22May2021_04Feb2022 = lapply(XLS_RAW_22May2021_04Feb2022,function(x)x[,which(names(x)!="Aer.Id")])
CVD                         = do.call(rbind,lapply(XLS_RAW_22May2021_04Feb2022,function(x)x))
CVD                         = CVD[order(CVD$Init.Rec.Date),]
CVD                         = subset(CVD, grepl("COVID-19", CVD$Product.Indications)) # Restrict to only AE initial reaction date after Nov 2020
CVD                         = subset(CVD,CVD$Init.Rec.Date>="2020-12-01") # Restrict to only AE initial reaction date after Nov 2020
#CHECKFUN=function(VAR){nrow(unique(CVD[,which(names(CVD) %in% c("Aer.No", VAR))]))}
#NAMS = names(CVD); for(i in 1:length(NAMS)){print(paste(NAMS[i],CHECKFUN(NAMS[i])))}
CVD_SINGLE                  = unique(CVD[, c("Aer.No", "Init.Rec.Date", "Report.Feature", "Source", "MAH.No")])
CVD_MULTPL                  = CVD[,-which(names(CVD) %in% c("Init.Rec.Date", "Report.Feature", "Source", "MAH.No"))]
CVD_MULTPL                  = CVD_MULTPL[order(CVD_MULTPL$Aer.No),]
library(dplyr)
library(tidyr)
L2W = function(DAT, NAM){TRY  = unique(DAT) %>% group_by(Aer.No) %>% mutate(counter=row_number()) %>% spread(counter, NAM, fill="") %>% data.frame()
                         names(TRY)[2:ncol(TRY)] = paste0(NAM, 1:(ncol(TRY)-1)); return(TRY)}
for(i in 2:ncol(CVD_MULTPL)){if(i==2){OUT = L2W(CVD_MULTPL[,c(1,i)], names(CVD_MULTPL)[i]); next}
                                      OUT = cbind(OUT, L2W(CVD_MULTPL[,c(1,i)], names(CVD_MULTPL)[i])[,-1])}
library(sqldf)
names(CVD_SINGLE) = gsub("\\.","_",names(CVD_SINGLE))
names(OUT)        = gsub("\\.","_",names(OUT))
CVD_WIDE          = sqldf("select * from CVD_SINGLE as a left join OUT as b on a.Aer_No = b.Aer_No")
CVD_WIDE          = CVD_WIDE[,!duplicated(names(CVD_WIDE))]

################################################################################
# Check list of SOC and PT terms from MEDDRA version 

CROSSWALK = read.csv("C:/Users/AYASSEEN/Desktop/Secondment/Auto-immune/MedDRA_CROSSWALK_20211007.csv")
SOC       = unique(CROSSWALK$soc_term)
PT        = unique(CROSSWALK$pt_term)

# SOC compare
COU = unique(CVD_MULTPL[,c("Aer.No", "Case.SOC.Name")])
TAB = data.frame(table(COU[,2])); TAB = TAB[order(TAB$Freq, decreasing=T),]
TAB
table(as.character(TAB$Var1) %in% SOC)

# PT compare
COU = unique(CVD_MULTPL[,c("Aer.No", "PT.Name")])
TAB = data.frame(table(COU[,2])); TAB = TAB[order(TAB$Freq, decreasing=T),]
TAB

par(mar=c(15,4,4,2)); barplot(TAB[,2], names.arg=TAB[,1], las=2)

table(as.character(TAB$Var1) %in% PT)
## 4 PT terms used in CVD do not exist in the cross walk file
## as.character(TAB$Var1)[which(!as.character(TAB$Var1) %in% PT)]

################################################################################
# produce summary table

TEM = data.frame(table(round(as.numeric(CVD_WIDE$Age__years_1)), round(as.numeric(CVD_WIDE$Age__years_2)), round(as.numeric(CVD_WIDE$Age__years_3)), useNA = 'always'))
TEM = TEM[TEM$Freq!=0,]
TEM[!is.na(TEM$Var1)&(!is.na(TEM$Var2)|!is.na(TEM$Var3)),]

CVD_WIDE$AGE1 = round(as.numeric(CVD_WIDE$Age__years_1))
CVD_WIDE$AGE2 = round(as.numeric(CVD_WIDE$Age__years_2))
CVD_WIDE$AGE3 = round(as.numeric(CVD_WIDE$Age__years_3))

ages = cbind(CVD_WIDE$AGE1, CVD_WIDE$AGE2, CVD_WIDE$AGE3)

ages[(ages[,1]==0) & (!is.na(ages[,2]) | !is.na(ages[,3])),1] = ages[(ages[,1]==0) & (!is.na(ages[,2]) | !is.na(ages[,3])),2]


ages[(ages[,1]==0) & (!is.na(ages[,2]) | !is.na(ages[,3])),]






barplot(data.frame(table(CVD_WIDE$AGE))[,2], names.arg=data.frame(table(CVD_WIDE$AGE))[,1]) # Years 0, 19, and 65 seem to be over populated










CVD_WIDE$AGE[CVD_WIDE$AGE==0 & (CVD_WIDE$Age__years_2!=0 | !is.na(CVD_WIDE$Age__years_2))] = CVD_WIDE$Age__years_2[CVD_WIDE$AGE==0 & (CVD_WIDE$Age__years_2!=0 | !is.na(CVD_WIDE$Age__years_2))]
barplot(data.frame(table(CVD_WIDE$AGE))[,2], names.arg=data.frame(table(CVD_WIDE$AGE))[,1])


CVD_WIDE$AGE[CVD_WIDE$AGE==0 & (CVD_WIDE$Age__years_2!=0 | !is.na(CVD_WIDE$Age__years_2))] = CVD_WIDE$Age__years_2[CVD_WIDE$AGE==0 & (CVD_WIDE$Age__years_2!=0 | !is.na(CVD_WIDE$Age__years_2))]






CVD_WIDE$AGE_GR = ifelse(as.numeric(CVD_WIDE$Age__years_1) <10, "0-9",
                   ifelse(as.numeric(CVD_WIDE$Age__years_1)<20, "10-19",
                   ifelse(as.numeric(CVD_WIDE$Age__years_1)<30, "20-29",
                   ifelse(as.numeric(CVD_WIDE$Age__years_1)<40, "30-39",
                   ifelse(as.numeric(CVD_WIDE$Age__years_1)<50, "40-49",
                   ifelse(as.numeric(CVD_WIDE$Age__years_1)<60, "50-59",
                   ifelse(as.numeric(CVD_WIDE$Age__years_1)<70, "60-69",
                   ifelse(as.numeric(CVD_WIDE$Age__years_1)<80, "70-79",
                   ifelse(as.numeric(CVD_WIDE$Age__years_1)>=80, "80+", "")))))))))

data.frame(table(CVD_WIDE$AGE_GR))








table(CVD_WIDE$Sex1, CVD_WIDE$AGE_GR)


table(CVD_WIDE$Age__years_1, CVD_WIDE$Age__years_2)























TP           = unique(CVD[,which(names(CVD) %in% c("Aer.No", "Init.Rec.Date"))])
TP$COUNT     = rep(1,nrow(TP))
TP$YEAR_WEK  = strftime(TP$Init.Rec.Date, "%Y-%W")
TP_WEEK      = aggregate(COUNT~YEAR_WEK, TP, sum)

seq(as.Date("2020-12-01"), as.Date(max(CVD$Init.Rec.Date)), by="week")

library(ggplot2)
ggplot(TP_WEEK, aes(x=YEAR_WEK, y=COUNT, group=1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("",limits = c(0,570), breaks=seq(0,600,50)) +
  scale_x_discrete("Year-Week", breaks=TP_WEEK$YEAR_WEK[seq(1, length(TP_WEEK$YEAR_WEK), by=4)]) +
  ggtitle("Counts of CVD AERs related to COVID-19 immunization by Unut.Rec.Date")+
  theme_minimal()




# For probabilistic linkage of CAEFISS and CVD
#https://rpubs.com/ahmademad/RecordLinkage
# require(RecordLinkage)
# a <- compare.linkage(nsf, patents, blockfld = c("state"), strcmp = T, exclude=c(1))
# print(head(a$pairs))
