rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("/Users/shnlee/Desktop/RA")
library(tidyverse)
library(rvest)
library(dplyr)
library(jsonlite)

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

dds = read.csv('dds.csv')
dds_ae = unique(dds$value)

meddra = read.csv('UnmatchedAEs.csv')
meddra = meddra[,c("Name","MEDDRA_ID","classified_as","classified_as_ID")]
meddra = meddra[!is.na(meddra$MEDDRA_ID),]
 
ddall = read.csv('dd.all.csv')
aelist = data.frame(id = c(ddall$pt, ddall$hlgt, ddall$sog), 
                    name = c(ddall$pt_name, ddall$hlgt_name, ddall$sog_name))

aelist = unique(aelist)
#sum(grepl(',', meddra[,"classified_as_ID"]))

cleaned = data.frame(matrix(NA, nrow = length(dds_ae) , ncol = 4))
colnames(cleaned) = c('name_dds', 'id', 'formal_name', 'formal_id')
cleaned$name_dds = dds_ae

for(i in 1:NROW(cleaned)){
  x = cleaned[i,'name_dds']
  if(length(which(x==meddra$Name))>0){ # Case (A) : Already has "Classified as" information
    cleaned[i,] = as.vector(meddra[which(x==meddra$Name),])
  } 
  else { # Case (B) : Without the "Classified as" information
    if(length(which(x==aelist$name))>0){ ## Case (B)-(1): Has MEDDRA ID of original name (in our data)
      cleaned[i,'id'] = aelist[which(x==aelist$name),"id"]
    } 
    else{ ## Case (B)-(2): Only has the original name
      try({ ### Getting orinial ID from RESTful API
        url = paste('http://data.bioontology.org/search?apikey=cab5a7da-2d12-4391-9de6-90e6b4c71580&q=',
                    gsub(' ', x, replacement = '%20'), '%7D', sep='')
        ae_url = fromJSON(url)$collection$`@id`
        m_idx = (ae_list$prefLabel==x & grepl('MEDDRA',ae_url))
        label_id = ae_list[m_idx,c("prefLabel","@id")]
        cleaned[i,'id'] == str_extract(label_id$`@id`,'100[0-9]*')
      })}
    
    try({ ##Getting "Classified_as" and its ID
      url_c = paste('http://purl.bioontology.org/ontology/MEDDRA/', cleaned[i,'id'], sep='')
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
        cleaned[i,"formal_name"] = paste(vec_ED, collapse = ", ")
        cleaned[i,"formal_id"] = paste(vec_ED_id, collapse = ", ")
      }
    })
    
  }
  print(i)
}


new_dds = merge(dds, cleaned, by.x="value", by.y = "name_dds") 
new_dds$AE_NAME = ifelse(is.na(new_dds$formal_name), new_dds$value, new_dds$formal_name)
new_dds$MEDDRA_ID = ifelse(is.na(new_dds$formal_id), new_dds$id, new_dds$formal_id)


write.csv(new_dds,'dds(AEcleaned).csv', row.names = F)
