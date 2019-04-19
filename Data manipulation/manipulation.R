options(stringsAsFactors = FALSE) 
library(reshape2)
library(dplyr)

rm(list=ls())
setwd("/Users/shnlee/Desktop/RA")

year=2000:2018

#### data_PAT
dd_pat=NULL
for (k in 1:length(year)){
  tmp=read.csv(paste('VAERS data/',year[k],"VAERSDATA",".csv",sep=""))
  dd_pat[[k]]=tmp[,c("VAERS_ID","CAGE_YR","NUMDAYS","STATE")]
}

data_PAT=Reduce("rbind",dd_pat)
data_PAT=data_PAT[order(data_PAT$VAERS_ID),]

#### data_VAC
dd_vac=NULL
for (k in 1:length(year)){
  tmp=read.csv(paste('VAERS data/',year[k],"VAERSVAX",".csv",sep=""))
  dd_vac[[k]]=tmp[,c("VAERS_ID","VAX_NAME","VAX_TYPE")]
}

data_VAC=Reduce("rbind",dd_vac)
data_VAC=data_VAC[order(data_VAC$VAERS_ID),]

## deal with mutliple vaccines per report
#calcuate the number of vaccines per report
vac.num=aggregate(x = data_VAC$VAERS_ID, by = list(data_VAC$VAERS_ID), FUN = "length")
vac.num$wt_report=1/vac.num[,2] 
names(vac.num)[1:2] <- c("VAERS_ID", "vaxnum")

data_VAC = merge(data_VAC, vac.num, by = "VAERS_ID", all.x = T)


#### data_AE
dd_ae=NULL
for (k in 1:length(year)){
  tmp=read.csv(paste('VAERS data/', year[k],"VAERSSYMPTOMS",".csv",sep=""))
  tmp=tmp[,c("VAERS_ID","SYMPTOM1","SYMPTOM2","SYMPTOM3","SYMPTOM4","SYMPTOM5")]
  tmp[tmp==""] = NA
  tmp.long=melt(tmp, id.vars = c('VAERS_ID'), measure.vars = 2:5, na.rm = T, value.name = "AE_NAME")
  dd_ae[[k]]=tmp.long
  dd_ae[[k]]$year=year[k]
}
data_ae=Reduce("rbind",dd_ae)
data_ae=data_ae[order(data_ae$VAERS_ID),]

meddra_ptlist = read.csv('meddra_ptlist.csv', header=T)
meddra_ptlist = meddra_ptlist[complete.cases(meddra_ptlist),]
data_AE = merge(data_ae, meddra_ptlist, by = 'AE_NAME', all.x = T)


##### dds 
dd2=merge(data_VAC,data_AE, by=c("VAERS_ID"),all.y = TRUE,all.x = TRUE)
dd2=merge(dd2,data_PAT, by=c("VAERS_ID"),all.y = TRUE,all.x = TRUE)
dds=dd2[order(dd2$VAERS_ID),]

write.csv(dds, 'full_dds.csv', row.names = F)

#### Summary
summary(dds)
length(unique(dds$AE_NAME[is.na(dds$MEDDRA_ID)]))  ### only 51 AE names are unmatched. + NA = 52

length(dds$AE_NAME[is.na(dds$MEDDRA_ID)]) 

dds[is.na(dds$vaxnum),]

dds[is.na(dds$year),] ### Meaning that these VAERS_IDs do not exist on data_VAC or data_PAT, but only on data_AE
