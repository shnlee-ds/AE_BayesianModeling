rm(list=ls())
options(stringsAsFactors = FALSE) 
library(reshape2)
library(dplyr)

#----------------------------------------------------------------
# upload VAX, AE and pat data from VAERS
# clean and merge them 
# Output: full_dds.csv
#----------------------------------------------------------------
setwd("Z:/He Yongqun/process data")

year=2000:2018

#### data_PAT
dd_pat=NULL
for (k in 1:length(year)){
  tmp=read.csv(paste('VAERS data/',year[k],"VAERSDATA",".csv",sep=""))
  dd_pat[[k]]=tmp[,c("VAERS_ID","AGE_YRS","CAGE_YR","CAGE_MO","NUMDAYS","SEX","STATE")]
}

data_PAT=Reduce("rbind",dd_pat)
data_PAT=data_PAT[order(data_PAT$VAERS_ID),]

#-- clean the age variable (CAGE= CAGE_YR+CAGE_MO by vax_date-birthdate; NUMDAYS:ONSET_DATE-VAX_DATE)
CAGE_MO=ifelse(is.na(data_PAT$CAGE_MO),0,data_PAT$CAGE_MO)
CAGE=data_PAT$CAGE_YR+CAGE_MO
data_PAT$AGE=ifelse(is.na(CAGE), data_PAT$AGE_YRS, CAGE)

#sum(!is.na(data_PAT$AGE_YRS) & is.na(CAGE))
#sum(abs(data_PAT$AGE_YRS-CAGE)>5,na.rm=TRUE)

#### data_VAC
dd_vac=NULL
for (k in 1:length(year)){
  tmp=read.csv(paste('VAERS data/',year[k],"VAERSVAX",".csv",sep=""))
  dd_vac[[k]]=tmp[,c("VAERS_ID","VAX_NAME","VAX_TYPE")]
}

data_VAC=Reduce("rbind",dd_vac)
data_VAC=data_VAC[order(data_VAC$VAERS_ID),]


#### data_AE
dd_ae=NULL
for (k in 1:length(year)){
  tmp=read.csv(paste('VAERS data/', year[k],"VAERSSYMPTOMS",".csv",sep=""))
  tmp=tmp[,c("VAERS_ID","SYMPTOM1","SYMPTOM2","SYMPTOM3","SYMPTOM4","SYMPTOM5")]
  tmp[tmp==""] = NA
  tmp.long=melt(tmp, id.vars = c('VAERS_ID'), measure.vars = 2:6, na.rm = T, value.name = "AE_NAME")
  dd_ae[[k]]=tmp.long
  dd_ae[[k]]$year=year[k]
}
data_ae=Reduce("rbind",dd_ae)
data_ae=data_ae[order(data_ae$VAERS_ID),]

#length(unique(data_ae$AE_NAME))  > 8592

meddra_ptlist = read.csv(paste('meddra data/','meddra_ptlist.csv',sep=""), header=T)
meddra_ptlist = meddra_ptlist[complete.cases(meddra_ptlist),]
data_AE = merge(data_ae, meddra_ptlist, by = 'AE_NAME', all.x = T)

#length(unique(data_AE$MEDDRA_AE_NAME))  > 7788

#### combine VAX-AE-PAT files
dd_VAC_AE=merge(data_VAC,data_AE, by=c("VAERS_ID"))
dd_VAC_AE_PAT=merge(dd_VAC_AE,data_PAT, by=c("VAERS_ID"),all.x = TRUE)
dds=dd_VAC_AE_PAT[order(dd_VAC_AE_PAT$VAERS_ID),]

dds = dds %>% select(VAERS_ID, MEDDRA_AE_NAME, MEDDRA_ID, VAX_NAME, VAX_TYPE, year, AGE, SEX, STATE) 


write.csv(dds, 'full_dds.csv', row.names = F)

#-- Summary and check
length(unique(dds$AE_NAME[is.na(dds$MEDDRA_ID)]))  ### only 413 AE names are unmatched. + NA = 413
length(unique(data_ae$AE_NAME[is.na(data_ae$ID)]))

length(unique(dds$AE_NAME))
length(unique(dds$MEDDRA_AE_NAME))

dds[is.na(dds$vaxnum),]

dds[is.na(dds$year),] ### Meaning that these VAERS_IDs do not exist on data_VAC or data_PAT, but only on data_AE

