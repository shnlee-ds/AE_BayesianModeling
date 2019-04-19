rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("/Users/shnlee/Desktop/RA")
library(tidyverse)
library(rvest)
library(dplyr)
library(jsonlite)


#### Getting all of the AE names from VAERS data
dd_ae=NULL
year = 2000:2018
for (k in 1:length(year)){
  tmp=read.csv(paste('VAERS data/', year[k],"VAERSSYMPTOMS",".csv",sep=""))
  tmp=tmp[,c("VAERS_ID","SYMPTOM1","SYMPTOM2","SYMPTOM3","SYMPTOM4","SYMPTOM5")]
  tmp[tmp==""] = NA
  tmp.long=melt(tmp, id.vars = c('VAERS_ID'), measure.vars = 2:5, na.rm = T, value.name = "AE_NAME")
  dd_ae[[k]]=tmp.long
  dd_ae[[k]]$year=year[k]
}
data_AE=Reduce("rbind",dd_ae)
data_AE=data_AE[order(data_AE$VAERS_ID),]

ae_names = unique(data_AE$AE_NAME)

ddall = read.csv('dd.all.csv')
ddall_ptlist = unique(ddall[,c('pt_name','pt')])

## Checking the number of unmatched Meddra PT terms
length(ae_names) - sum(ae_names %in% ddall$pt_name)


########## Generating a table that has the information of MEDDRA PT terms and their MEDDRA ID
meddra_ptlist = data.frame(matrix(NA, nrow = length(ae_names), ncol=4))
colnames(meddra_ptlist) = c('AE_NAME', 'ID', 'MEDDRA_AE_NAME', 'MEDDRA_ID')

### Step 1: Getting Meddra ID of the PT terms which already exist in the 'ddall.csv'
meddra_ptlist$AE_NAME = ae_names
meddra_ptlist$ID = ifelse(meddra_ptlist$AE_NAME %in% ddall_ptlist$pt_name,ddall_ptlist$pt, NA)

### Step 2: Getting Meddra ID of the PT terms which are not matched with 'ddall.csv'
## Webscraping using 'RESTful API': Getting Jason data and extracting Meddra information

idx = which(is.na(meddra_ptlist$ID))
cnt = 0
for(i in idx){
  try({
    x = meddra_ptlist[i,'AE_NAME']
    url = paste('http://data.bioontology.org/search?apikey=cab5a7da-2d12-4391-9de6-90e6b4c71580&q=',
                gsub(' ', x, replacement = '%20'), '%7D', sep='')
    ae_url = fromJSON(url)$collection
    m_idx = (ae_url$prefLabel==x & grepl('MEDDRA',ae_url$`@id`))
    label_id = ae_url[m_idx,"@id"]
    meddra_ptlist[i,'ID'] = str_extract(label_id,'100[0-9]*')
  })
  cnt = cnt + 1
  print(cnt)
}

### Step 3: Getting Formal Meddra PT names and their Meddra ID

# Modified strsplit function for convenience
strsplit2 <- function(x,split,type = "remove", perl = FALSE, ...) {
  if (type == "remove") {
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    out <- base::strsplit(x = x, split = paste0("(?<=.)(?=", split, ")"),perl = TRUE,...)
  } else if (type == "after") {
    out <- base::strsplit(x = x, split = paste0("(?<=", split, ")"), perl = TRUE,...)
  } else {
    stop("type must be remove, after or before!")}
  return(out)
}

## Webscraping from BioPortal HTML sources 
cnt = 0
for(i in which(!is.na(meddra_ptlist$ID))){
  try({ ##Getting "Classified_as" and its ID
    url_c = paste('http://purl.bioontology.org/ontology/MEDDRA/', meddra_ptlist[i,'ID'], sep='')
    read = read_html(url_c)
    node = html_nodes(read, "td")
    text = as.character(html_text(node))
    
    classifiED = gsub('\n', replacement='', x=text[which(tolower(text) == "classified as") + 1])
    classifiED = unlist(strsplit2(classifiED, 'http://', type="before"))
    
    
    if(length(classifiED) > 0){
      vec_ED = NULL; vec_ED_id = NULL
      for(d in 1:length(classifiED)){
        page_ED = read_html(classifiED[d])
        node_ED = html_nodes(page_ED, "td")
        text_ED = as.character(html_text(node_ED))
        vec_ED = c(vec_ED,
                   gsub('\n', replacement='', x=text_ED[which(tolower(text_ED) == "preferred name") + 1]))
        vec_ED_id = c(vec_ED_id,
                      gsub('\n', replacement='', x=text_ED[which(tolower(text_ED) == "notation") + 1]))
      }
      meddra_ptlist[i,"MEDDRA_AE_NAME"] = paste(vec_ED, collapse = ", ")
      meddra_ptlist[i,"MEDDRA_ID"] = paste(vec_ED_id, collapse = ", ")
    }
  })
  cnt = cnt+1
  if(cnt%%10==0){print(cnt)} #8179
}

idx2 = which(is.na(meddra_ptlist$MEDDRA_AE_NAME))
meddra_ptlist[idx2, c("MEDDRA_AE_NAME","MEDDRA_ID")] = meddra_ptlist[idx2, c("AE_NAME","ID")]

write.csv(meddra_ptlist,'meddra_ptlist.csv', row.names = F)
