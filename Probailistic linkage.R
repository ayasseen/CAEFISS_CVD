


library(RecordLinkage)
data(RLdata500)
data(RLdata10000)
RLdata500[17, ]
RLdata10000[343, ]





rpairs <- compare.linkage(RLdata500,
                          RLdata10000,
                          blockfld=c(1), 
                          exclude=c(2:5,7))

rpairs$pairs[c(1:2), ] # Why is_match=NA? (should be 1)

rpairs <- epiWeights(rpairs) # (Weight calculation)

summary(rpairs) # (0 matches in Linkage Dataset)





















################################################################################

COMPARE = readRDS(file="C:/Users/AYASSEEN/Desktop/Secondment/CVD/COMPARE.rds")

library(plyr)
library(dplyr)

DAT1    = COMPARE[[1]] %>% 
          select(V_NUMBER, REPORTED_DATE, SEXGR, AGE, VACTRAD) %>% 
          rename_with(~ paste0("CAE_", .x))
DAT2    = COMPARE[[2]] %>% 
          select(Aer_No, Init_Rec_Date, SEXGR, AGE, VACTRAD) %>% 
          rename_with(~ paste0("CVD_", .x))
DAT3    = rbind.fill(DAT1, DAT2)


rec.pairs = compare.linkage(DAT1, DAT2,
                          blockfld = list(2:5),
                          strcmp   = c(7:10),
                          strcmpfun=levenshteinSim)




