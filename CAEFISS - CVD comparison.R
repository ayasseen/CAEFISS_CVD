################################################################################
# Read in data
library(haven)

MONDAY   = "21feb2022"

ST=Sys.time(); ST
CAE = read_sas(paste0("D:/IDPC_PCMI/VSS/CAEFISS/Data/Monday_pull/caefiss_",MONDAY,".sas7bdat"),
               col_select = c("V_NUMBER", "AGE", "SEX", 
                              "VACCINE_ADMIN_DATE", 
                              "REPORT_RECEIVED_DATE", 
                              "DATE_REPORT_COMPLETED", 
                              "REPORTED_DATE",
                              paste0("VACC_VACCINE_ABBR", 1:10), 
                              paste0("VACC_TRADE_NAME", 1:10), 
                              paste0("MEDDRA_PT_TERM", 1:30), 
                              paste0("MEDDRA_SOC_TERM", 1:30)))
Sys.time()-ST

CAEF = CAE
CAE  = CAEF
#rm(list=setdiff(ls(), c("CAEF"))); gc()
################################################################################

# COND1 - Select out COVID-19 cases
SEL_VARS   = which(substr(names(CAE),0,17)=="VACC_VACCINE_ABBR")
VacABBR    = gsub("--","",apply(CAE[,SEL_VARS], 1, paste, collapse = "-"))
COND1      = grepl("COVID-19",VacABBR)

# COND3 - Set date ranges
DATE_RANGE = seq(as.Date("2021-05-22"), as.Date("2022-02-04"), by="days")
COND3      = !is.na(CAE$VACCINE_ADMIN_DATE)    & as.Date(CAE$VACCINE_ADMIN_DATE)    %in% DATE_RANGE 

CAEFISS    = subset(CAE, COND1 & COND3)

################################################################################
# Age by five year increments
CAEFISS$AGEGR5 = ifelse(as.numeric(CAEFISS$AGE)<5,  "00-04",
                 ifelse(as.numeric(CAEFISS$AGE)<10, "05-09",
                 ifelse(as.numeric(CAEFISS$AGE)<15, "10-14",
                 ifelse(as.numeric(CAEFISS$AGE)<20, "15-19",
                 ifelse(as.numeric(CAEFISS$AGE)<25, "20-24",
                 ifelse(as.numeric(CAEFISS$AGE)<30, "25-29",
                 ifelse(as.numeric(CAEFISS$AGE)<35, "30-34",
                 ifelse(as.numeric(CAEFISS$AGE)<40, "35-39",
                 ifelse(as.numeric(CAEFISS$AGE)<45, "40-44", 
                 ifelse(as.numeric(CAEFISS$AGE)<50, "45-49",
                 ifelse(as.numeric(CAEFISS$AGE)<55, "50-54",
                 ifelse(as.numeric(CAEFISS$AGE)<60, "55-59",
                 ifelse(as.numeric(CAEFISS$AGE)<65, "60-64",
                 ifelse(as.numeric(CAEFISS$AGE)<70, "65-69",
                 ifelse(as.numeric(CAEFISS$AGE)<75, "70-74",
                 ifelse(as.numeric(CAEFISS$AGE)<80, "75-79",
                 ifelse(as.numeric(CAEFISS$AGE)>=80, "80+", "")))))))))))))))))
# Age by ten year increments
CAEFISS$AGEGR10 = ifelse(as.numeric(CAEFISS$AGE)<10, "00-09",
                  ifelse(as.numeric(CAEFISS$AGE)<20, "10-19",
                  ifelse(as.numeric(CAEFISS$AGE)<30, "20-29",
                  ifelse(as.numeric(CAEFISS$AGE)<40, "30-39",
                  ifelse(as.numeric(CAEFISS$AGE)<50, "40-49",
                  ifelse(as.numeric(CAEFISS$AGE)<60, "50-59",
                  ifelse(as.numeric(CAEFISS$AGE)<70, "60-69",
                  ifelse(as.numeric(CAEFISS$AGE)<80, "70-79",
                  ifelse(as.numeric(CAEFISS$AGE)>=80, "80+", "")))))))))
# Sex groups
CAEFISS$SEXGR   = ifelse(CAEFISS$SEX=="Female", "F", 
                  ifelse(CAEFISS$SEX=="Male", "M", "Other_Missing"))
# Vaccine trade name
SEARCH_TERMS    = function(DAT, SER, MED){ SELS = which(substr(names(DAT),0,nchar(MED))==MED)
                                           OUT  = do.call(cbind, lapply(DAT[,SELS], function(x) grepl(paste(SER, collapse="|"), x)))
                                           return(apply(OUT,1,sum)>0) }
CAEFISS$VACTRAD = ifelse(SEARCH_TERMS(CAEFISS, "AZC COVID-19 \\(VAXZEVRIA\\)", "VACC_TRADE_NAME"), "AZ",
                  ifelse(SEARCH_TERMS(CAEFISS, "Cov COVID-19",                 "VACC_TRADE_NAME"), "CO",
                  ifelse(SEARCH_TERMS(CAEFISS, "Jan COVID-19",                 "VACC_TRADE_NAME"), "JA",
                  ifelse(SEARCH_TERMS(CAEFISS, "Mod COVID-19 \\(SPIKEVAX\\)",  "VACC_TRADE_NAME"), "MO",
                  ifelse(SEARCH_TERMS(CAEFISS, "PB COVID-19 \\(COMIRNATY\\)",  "VACC_TRADE_NAME"), "PB", "UN")))))
# First reported date
CAEFISS$DATE_WEEK  = format(CAEFISS$REPORTED_DATE, "%Y-%U") # Week starting on Sunday
CAEFISS$DATE_MONTH = format(CAEFISS$REPORTED_DATE, "%Y-%m")
# PT grouped together
SELS               = which(substr(names(CAEFISS),0,nchar("MEDDRA_PT_TERM"))=="MEDDRA_PT_TERM")
CAEFISS$PTGR       = apply(CAEFISS[,SELS], 1, function(x) paste(x, collapse="|"))
# SOC grouped together
SELS               = which(substr(names(CAEFISS),0,nchar("MEDDRA_SOC_TERM"))=="MEDDRA_SOC_TERM")
CAEFISS$SOCGR      = apply(CAEFISS[,SELS], 1, function(x) paste(x, collapse="|"))


CAEFISS2 = CAEFISS[,c("V_NUMBER", "REPORTED_DATE", "DATE_WEEK", "DATE_MONTH", "SEXGR", "AGE", "AGEGR5", "AGEGR10", "VACTRAD", "SOCGR", "PTGR")]

rm(list=setdiff(ls(), c("CAEF", "CAEFISS2"))); gc()

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

XLS_RAW_22May2021_04Feb2022 = readRDS(file="C:/Users/AYASSEEN/Desktop/Secondment/CVD/RAW-RAW.rds")
XLS_RAW_22May2021_04Feb2022 = lapply(XLS_RAW_22May2021_04Feb2022,function(x)x[,which(names(x)!="Aer.Id")])
CVD                         = do.call(rbind,lapply(XLS_RAW_22May2021_04Feb2022,function(x)x))
CVD                         = CVD[order(CVD$Init.Rec.Date),]
CVD                         = subset(CVD, grepl("COVID-19", CVD$Product.Indications)) # Restrict to only AE initial reaction date after Nov 2020
CVD                         = subset(CVD,CVD$Init.Rec.Date>="2020-12-01") # Restrict to only AE initial reaction date after Nov 2020
CVD_SINGLE                  = unique(CVD[, c("Aer.No", "Init.Rec.Date", "Report.Feature", "Source", "MAH.No")])
CVD_MULTPL                  = CVD[,-which(names(CVD) %in% c("Init.Rec.Date", "Report.Feature", "Source", "MAH.No"))]
CVD_MULTPL                  = CVD_MULTPL[order(CVD_MULTPL$Aer.No),]
library(tidyverse)
L2W = function(DAT, NAM){TRY= unique(DAT) %>% group_by(Aer.No) %>% mutate(counter=row_number()) %>% spread(counter, NAM, fill="") %>% data.frame()
names(TRY)[2:ncol(TRY)] = paste0(NAM, 1:(ncol(TRY)-1)); return(TRY)}
for(i in 2:ncol(CVD_MULTPL)){if(i==2){OUT = L2W(CVD_MULTPL[,c(1,i)], names(CVD_MULTPL)[i]); next}
                             OUT = cbind(OUT, L2W(CVD_MULTPL[,c(1,i)], names(CVD_MULTPL)[i])[,-1])}
library(sqldf)
names(CVD_SINGLE) = gsub("\\.","_",names(CVD_SINGLE))
names(OUT)        = gsub("\\.","_",names(OUT))
CVD_WIDE          = sqldf("select * from CVD_SINGLE as a left join OUT as b on a.Aer_No = b.Aer_No")
CVD_WIDE          = CVD_WIDE[,!duplicated(names(CVD_WIDE))]


#################################
# DATA CLEANING NEEDED TO MATCH CAEFISS2
#################################


# Age by five year increments
## Age rounded
CVD_WIDE$Age__years_1 = round(as.numeric(CVD_WIDE$Age__years_1))
CVD_WIDE$Age__years_2 = round(as.numeric(CVD_WIDE$Age__years_2))
CVD_WIDE$Age__years_3 = round(as.numeric(CVD_WIDE$Age__years_3))

################################### NEED TO OODER FILES variable #################################
# overwrites original variable with updated value
################################### NEED TO OODER FILES variable #################################
CVD_WIDE$AGE = CVD_WIDE$Age__years_1
CVD_WIDE$AGE[!is.na(CVD_WIDE$Age__years_2)] = CVD_WIDE$Age__years_2[!is.na(CVD_WIDE$Age__years_2)]
CVD_WIDE$AGE[!is.na(CVD_WIDE$Age__years_3)] = CVD_WIDE$Age__years_3[!is.na(CVD_WIDE$Age__years_3)]

CVD_WIDE$AGEGR5 = ifelse(as.numeric(CVD_WIDE$AGE)<5,  "00-04",
                  ifelse(as.numeric(CVD_WIDE$AGE)<10, "05-09",
                  ifelse(as.numeric(CVD_WIDE$AGE)<15, "10-14",
                  ifelse(as.numeric(CVD_WIDE$AGE)<20, "15-19",
                  ifelse(as.numeric(CVD_WIDE$AGE)<25, "20-24",
                  ifelse(as.numeric(CVD_WIDE$AGE)<30, "25-29",
                  ifelse(as.numeric(CVD_WIDE$AGE)<35, "30-34",
                  ifelse(as.numeric(CVD_WIDE$AGE)<40, "35-39",
                  ifelse(as.numeric(CVD_WIDE$AGE)<45, "40-44", 
                  ifelse(as.numeric(CVD_WIDE$AGE)<50, "45-49",
                  ifelse(as.numeric(CVD_WIDE$AGE)<55, "50-54",
                  ifelse(as.numeric(CVD_WIDE$AGE)<60, "55-59",
                  ifelse(as.numeric(CVD_WIDE$AGE)<65, "60-64",
                  ifelse(as.numeric(CVD_WIDE$AGE)<70, "65-69",
                  ifelse(as.numeric(CVD_WIDE$AGE)<75, "70-74",
                  ifelse(as.numeric(CVD_WIDE$AGE)<80, "75-79",
                  ifelse(as.numeric(CVD_WIDE$AGE)>=80, "80+", "")))))))))))))))))
# Age by ten year increments
CVD_WIDE$AGEGR10 = ifelse(as.numeric(CVD_WIDE$AGE)<10, "00-09",
                   ifelse(as.numeric(CVD_WIDE$AGE)<20, "10-19",
                   ifelse(as.numeric(CVD_WIDE$AGE)<30, "20-29",
                   ifelse(as.numeric(CVD_WIDE$AGE)<40, "30-39",
                   ifelse(as.numeric(CVD_WIDE$AGE)<50, "40-49",
                   ifelse(as.numeric(CVD_WIDE$AGE)<60, "50-59",
                   ifelse(as.numeric(CVD_WIDE$AGE)<70, "60-69",
                   ifelse(as.numeric(CVD_WIDE$AGE)<80, "70-79",
                   ifelse(as.numeric(CVD_WIDE$AGE)>=80, "80+", "")))))))))
# Sex groups
################################### NEED TO OODER FILES variable #################################
# overwrites original variable with updated value
################################### NEED TO OODER FILES variable #################################
CVD_WIDE$SEXGR = CVD_WIDE$Sex1
CVD_WIDE$SEXGR[CVD_WIDE$Sex2!=""] = CVD_WIDE$Sex2[CVD_WIDE$Sex2!=""]
CVD_WIDE$SEXGR = ifelse(CVD_WIDE$SEXGR=="Female", "F", 
                 ifelse(CVD_WIDE$SEXGR=="Male",   "M", "Other_Missing"))
# Vaccine trade name
PB = c("PFIZER-BIONTECH COVID-19 VACCINE VIAL CONTAINS 6 DOSES OF 0.3 ML AFTER DILUTION",
       "PFIZER-BIONTECH COVID-19 VACCINE",                                               
       "PFIZER-BIONTECH COVID-19 VACCINE VIAL CONTAINS 5 DOSES OF 0.3 ML",               
       "BNT162B2",                                                                       
       "COMIRNATY VIAL CONTAINS 6 DOSES OF 0.3 ML AFTER DILUTION",                       
       "COMIRNATY VIAL CONTAINS 10 DOSES OF 0.2 ML AFTER DILUTION",
       "TOZINAMERAN",
       "PFIZER BIONTECH COVID-19 VACCINE",
       "BNT162B3",
       "COVID-19 VACCINE MRNA (BNT162B2)")
MO = c("MRNA-1273 SARS-COV-2",                      
       "COVID-19 VACCINE MODERNA VIAL CONTAINS 10 DOSES OF 0.5ML",
       "SPIKEVAX VIAL CONTAINS 10 DOSES OF 0.5ML",                      
       "MODERNA COVID-19 VACCINE VIAL CONTAINS 10 DOSES OF 0.5ML",
       "COVID 19 VACCINE MODERNA")
AZ = c("ASTRAZENECA COVID-19 VACCINE VIAL CONTAINS 10 DOSES OF 0.5ML",
       "VAXZEVRIA VIAL CONTAINS 10 DOSES OF 0.5ML",
       "ASTRAZENECA COVID-19 VACCINE",
       "ASTRAZENECA COVID VACCINE",
       "CHADOX1 NCOV-19")
CO = "COVISHIELD VIAL CONTAINS 10 DOSES OF 0.5ML"                                     
UN = c("COVID-19 VACCINE", "ELIQUIS FILM COATED", "MACROGOL", "ZYRTEC - TAB 10MG")

CVD_WIDE$Trade_Name1[CVD_WIDE$Trade_Name1 %in% PB] = "PB"
CVD_WIDE$Trade_Name1[CVD_WIDE$Trade_Name1 %in% MO] = "MO"
CVD_WIDE$Trade_Name1[CVD_WIDE$Trade_Name1 %in% AZ] = "AZ"
CVD_WIDE$Trade_Name1[CVD_WIDE$Trade_Name1 %in% CO] = "CO"
CVD_WIDE$Trade_Name1[CVD_WIDE$Trade_Name1 %in% UN] = "UN"
CVD_WIDE$Trade_Name2[CVD_WIDE$Trade_Name2 %in% PB] = "PB"
CVD_WIDE$Trade_Name2[CVD_WIDE$Trade_Name2 %in% MO] = "MO"
CVD_WIDE$Trade_Name2[CVD_WIDE$Trade_Name2 %in% AZ] = "AZ"
CVD_WIDE$Trade_Name2[CVD_WIDE$Trade_Name2 %in% CO] = "CO"
CVD_WIDE$Trade_Name2[CVD_WIDE$Trade_Name2 %in% UN] = "UN"
CVD_WIDE$Trade_Name3[CVD_WIDE$Trade_Name3 %in% PB] = "PB"
CVD_WIDE$Trade_Name3[CVD_WIDE$Trade_Name3 %in% MO] = "MO"
CVD_WIDE$Trade_Name3[CVD_WIDE$Trade_Name3 %in% AZ] = "AZ"
CVD_WIDE$Trade_Name3[CVD_WIDE$Trade_Name3 %in% CO] = "CO"
CVD_WIDE$Trade_Name3[CVD_WIDE$Trade_Name3 %in% UN] = "UN"

################################### NEED TO OODER FILES variable #################################
# overwrites original variable with updated value
################################### NEED TO OODER FILES variable #################################
CVD_WIDE$VACTRAD = CVD_WIDE$Trade_Name1
CVD_WIDE$VACTRAD[CVD_WIDE$Trade_Name2!=""] = CVD_WIDE$Trade_Name2[CVD_WIDE$Trade_Name2!=""]
CVD_WIDE$VACTRAD[CVD_WIDE$Trade_Name3!=""] = CVD_WIDE$Trade_Name3[CVD_WIDE$Trade_Name3!=""]

# First reported date
CVD_WIDE$DATE_WEEK  = format(as.Date(CVD_WIDE$Init_Rec_Date), "%Y-%U") # Week starting on Sunday
CVD_WIDE$DATE_MONTH = format(as.Date(CVD_WIDE$Init_Rec_Date), "%Y-%m")
# PT grouped together
SELS               = which(substr(names(CVD_WIDE),0,nchar("PT_Name"))=="PT_Name")
CVD_WIDE$PTGR       = apply(CVD_WIDE[,SELS], 1, function(x) paste(x, collapse="|"))
# SOC grouped together
SELS               = which(substr(names(CVD_WIDE),0,nchar("Case_SOC_Name"))=="Case_SOC_Name")
CVD_WIDE$SOCGR      = apply(CVD_WIDE[,SELS], 1, function(x) paste(x, collapse="|"))


CVD2 = CVD_WIDE[,c("Aer_No", "Init_Rec_Date", "DATE_WEEK", "DATE_MONTH", "SEXGR", "AGE", "AGEGR5", "AGEGR10", "VACTRAD", "SOCGR", "PTGR")]

rm(list=setdiff(ls(), c("CAEF", "CAEFISS2", "CVD2"))); gc()


#COMPARE = list(CAEFISS2, CVD2)
#saveRDS(COMPARE, "C:/Users/AYASSEEN/Desktop/Secondment/CVD/COMPARE.rds")

################################################################################
# Read in extracted list of raw CVD data and convert data to wide format



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

#https://www.youtube.com/watch?v=Msl1Q5Yv8Ow

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################













# ################################################################################
# 
# # Compare different dates
# c1 = !is.na(CAEFISS$VACCINE_ADMIN_DATE)   & as.Date(CAEFISS$VACCINE_ADMIN_DATE)   %in% DATE_RANGE
# c2 = !is.na(CAEFISS$REPORT_RECEIVED_DATE) & as.Date(CAEFISS$REPORT_RECEIVED_DATE) %in% DATE_RANGE
# c3 = !is.na(CAEFISS$REPORTED_DATE)        & as.Date(CAEFISS$REPORTED_DATE)        %in% DATE_RANGE
# 
# dat = CAEFISS[c1,]
# COUNTS_DAY = data.frame(table(dat$VACCINE_ADMIN_DATE))
# COUNTS_WEK = data.frame(WEEK=strftime(COUNTS_DAY$Var1, "%Y-%W"), COUNTS_DAY)
# COUNTS_WEK = aggregate(Freq~WEEK, COUNTS_WEK, sum)
# COUNTS_WEK$DATE = "VACCINE_ADMIN_DATE"
# TP = COUNTS_WEK
# 
# dat = CAEFISS[c2,]
# COUNTS_DAY = data.frame(table(dat$REPORT_RECEIVED_DATE))
# COUNTS_WEK = data.frame(WEEK=strftime(COUNTS_DAY$Var1, "%Y-%W"), COUNTS_DAY)
# COUNTS_WEK = aggregate(Freq~WEEK, COUNTS_WEK, sum)
# COUNTS_WEK$DATE = "REPORT_RECEIVED_DATE"
# TP = rbind(TP, COUNTS_WEK)
# 
# dat = CAEFISS[c3,]
# COUNTS_DAY = data.frame(table(dat$REPORTED_DATE))
# COUNTS_WEK = data.frame(WEEK=strftime(COUNTS_DAY$Var1, "%Y-%W"), COUNTS_DAY)
# COUNTS_WEK = aggregate(Freq~WEEK, COUNTS_WEK, sum)
# COUNTS_WEK$DATE = "REPORTED_DATE"
# TP = rbind(TP, COUNTS_WEK)
# 
# library(ggplot2)
# BOT = ggplot(TP, aes(x=WEEK, y=Freq, group=DATE, color=DATE)) +
#   geom_point() +
#   geom_line() +
#   scale_y_continuous("", limits=c(0,1500), breaks=seq(0,2000,250)) +
#   scale_x_discrete("Year-Week",breaks = TP$WEEK[seq(1, 40, by=4)]) +
#   ggtitle("")+
#   theme_minimal()
# A = ggplot(CAEFISS, aes(x=VACCINE_ADMIN_DATE, y=REPORT_RECEIVED_DATE)) + geom_point(alpha = 0.1 ) +
#   scale_x_date(limits = as.Date(c('2021-01-01','2022-02-04'))) +
#   scale_y_date(limits = as.Date(c('2021-01-01','2022-02-04'))) +
#   theme_minimal()
# B = ggplot(CAEFISS, aes(x=VACCINE_ADMIN_DATE, y=REPORTED_DATE)) + geom_point(alpha = 0.1 ) +
#   scale_x_date(limits = as.Date(c('2021-01-01','2022-02-04'))) +
#   scale_y_date(limits = as.Date(c('2021-01-01','2022-02-04'))) +
#   theme_minimal()
# C = ggplot(CAEFISS, aes(x=REPORTED_DATE, y=REPORT_RECEIVED_DATE)) + geom_point(alpha = 0.1 ) +
#   scale_x_date(limits = as.Date(c('2021-01-01','2022-02-04'))) +
#   scale_y_date(limits = as.Date(c('2021-01-01','2022-02-04'))) +
#   theme_minimal()
# library(patchwork)
# library(cowplot)
# plot_grid(plotlist=list((A|B|C)/BOT))
