setwd("/Users/shnlee/Desktop/RA")
library(tidyverse)
library(rvest)
library(dplyr)
library(jsonlite)

#### Step 0. The list of Unmatched AE names

rm(list=ls())
data = read.csv('dds.csv')
data = data[complete.cases(data),]
ddall = read.csv('dd.all.csv')
data = unique(select(data, value))
idx = data$value %in% ddall$pt_name
ref = data[!idx,'value']


#### Step 1. Getting MEDDRA ID for the unmatched AEs ####

table = data.frame(matrix(NA, nrow = length(ref), ncol=5))
colnames(table) = c("Name", "MEDDRA_prefLabel", "identical","MEDDRA_ID", "url")
table$Name = ref


for(i in 1:length(ref)){
  try({
  url = paste('http://data.bioontology.org/search?apikey=cab5a7da-2d12-4391-9de6-90e6b4c71580&q=',
              gsub(' ', ref[i], replacement = '%20'), '%7D', sep='')
  ae_list = fromJSON(url)$collection
  ae_url = ae_list$`@id`
  m_idx = (ae_list$prefLabel==ref[i] & grepl('MEDDRA',ae_url))
  label_id = ae_list[m_idx,c("prefLabel","@id")]
  if(NROW(label_id)==0){}
  else{table[i, c(2,5)] = ae_list[m_idx,c("prefLabel","@id")]}
  })
  print(i)
}

table[,"MEDDRA_ID"] = str_extract(table[,'url'],'100[0-9]*')
table[,"identical"] = (table[,"Name"] == table[,"MEDDRA_prefLabel"])

write.csv(table, 'table_1.csv', row.names = F)

#### Step 2. 

## A string split function (with type) for convenience

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


table = read.csv('table_1.csv', header=T)

table = data.frame(table, classified_as = NA, classified_as_ID = NA, 
                          classifies = NA, classifies_ID = NA, 
                          subclassof = NA, subclassof_ID = NA)

for(i in 1:NROW(table)){
  try({
  read = read_html(as.character(table[i,'url']))
  node = html_nodes(read, "td")
  text = as.character(html_text(node))
  
  
  ## Classified as
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
    
    table[i,"classified_as"] = paste(vec_ED, collapse = ", ")
    table[i,"classified_as_ID"] = paste(vec_ED_id, collapse = ", ")
  }
  
  ## Classifies
  classifiES =  gsub('\n', replacement='', x=text[which(tolower(text) == "classifies") + 1])
  classifiES = unlist(strsplit2(classifiES, 'http://', type="before"))
  
  if(length(classifiES) > 0){
    vec_ES = NULL; vec_ES_id = NULL
    for(s in 1:length(classifiES)){
      page_ES = read_html(classifiES[s])
      node_ES = html_nodes(page_ES, "td")
      text_ES = as.character(html_text(node_ES))
      vec_ES = c(vec_ES,
                 gsub('\n', replacement='', x=text_ES[which(tolower(text_ES) == "preferred name") + 1]))
      vec_ES_id = c(vec_ES_id,
                    gsub('\n', replacement='', x=text_ES[which(tolower(text_ES) == "notation") + 1]))
    }     
    
    table[i,"classifies"] = paste(vec_ES, collapse = ", ")
    table[i,"classifies_ID"] = paste(vec_ES_id, collapse = ", ")
  }
  
  ## SubClassOf
  subclass =  gsub('\n', replacement='', x=text[which(tolower(text) == "subclassof") + 1])
  subclass = unlist(strsplit2(subclass, 'http://', type="before"))
  
  if(length(subclass) > 0){
    vec_SUB = NULL; vec_SUB_id = NULL
    for(b in 1:length(subclass)){
      page_SUB = read_html(subclass[b])
      node_SUB = html_nodes(page_SUB, "td")
      text_SUB = as.character(html_text(node_SUB))
      vec_SUB = c(vec_SUB,
                 gsub('\n', replacement='', x=text_SUB[which(tolower(text_SUB) == "preferred name") + 1]))
      vec_SUB_id = c(vec_SUB_id,
                    gsub('\n', replacement='', x=text_SUB[which(tolower(text_SUB) == "notation") + 1]))
    }     
    
    table[i,"subclassof"] = paste(vec_SUB, collapse = ", ")
    table[i,"subclassof_ID"] = paste(vec_SUB_id, collapse = ", ")
  }
  })
  print(i)
}

write.csv(table, 'table_2.csv', row.names = F)
