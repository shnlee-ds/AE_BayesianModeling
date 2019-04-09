setwd("/Users/shnlee/Desktop/RA")
library(tidyverse)
library(rvest)
library(dplyr)

rm(list=ls())

#### Get the table from 
url = 'http://sparql.hegroup.org/sparql?default-graph-uri=&query=%23To+find+all+OAE+terms+with+mapped+MEDDRA+ID%0D%0A%0D%0APREFIX+obo-term%3A+%3Chttp%3A%2F%2Fpurl.obolibrary.org%2Fobo%2F%3E%0D%0ASELECT+DISTINCT+%3Fx+%3Flabel+%3Fmeddra%0D%0Afrom+%3Chttp%3A%2F%2Fpurl.obolibrary.org%2Fobo%2Fmerged%2FOAE%3E%0D%0AWHERE%0D%0A%7B%0D%0A%3Fx+rdfs%3Alabel++%3Flabel.%0D%0A%3Fx+obo-term%3AOAE_0004334+%3Fmeddra.%0D%0A%7D%0D%0A&format=text%2Fhtml&timeout=0&debug=on'
webpage = read_html(url)
node = html_nodes(webpage, "td")
text = as.character(html_text(node))

table = data.frame(matrix(NA, nrow = length(text)/3, ncol=3))
colnames(table) = c('url','Name','ID')

for(i in 0:(length(text)/3-1)){
  table[i+1,1] = text[3*i+1]
  table[i+1,2] = gsub('\\"', replacement='', x = text[3*i+2])
  table[i+1,3] = str_extract(text[3*i+3],'100[0-9]*')
}

write.csv(table, file="webpage2.csv", row.names=F)