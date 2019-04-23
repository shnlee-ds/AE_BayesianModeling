rm(list=ls())
options(stringsAsFactors = FALSE)

visGBSnetwork = function(data, n = 10, file = NULL){
  
  library(igraph)
  library(visNetwork)  
  
  ## Cleaning into GBS-related dataset
  data_GBS=data[data$pt_name=="Guillain-Barre syndrome",]
  
  GBS_parents = names(table(data_GBS$hlgt_name))
  dds=data[,c(1,2,5,6)]
  dds=dds[order(dds$pt,dds$hlgt),]
  dds=dds[!duplicated(dds),]
  dds=dds[dds$hlgt_name %in% GBS_parents,]
  
  ## Manipulation for PT terms that have multiple parents
  dup = dds$pt[duplicated(dds$pt)]
  duplist = dds[dds$pt %in% dup, c('pt_name','hlgt_name')]
  mul_parent_pt = unique(duplist$pt_name) ## Should include GBS
  
  ## Select PT terms with multiple parents + Random sample nodes (size=n)
  GBS_idx = which(dds$pt_name %in% mul_parent_pt)
  idx = c(1:NROW(dds))[-GBS_idx]
  idx = sample(idx,n, replace = F)
  idx = c(idx, GBS_idx)
  dds = dds[idx,]
  
  ## Generate an edge list
  comb1=expand.grid(dds$pt,dds$pt)
  comb2=expand.grid(dds$hlgt,dds$hlgt)
  diff=comb2[,1]-comb2[,2]
  
  index=ifelse(diff==0,1,0)
  dg=comb1[(index==1 & comb1[,1] != comb1[,2]),]
  dg=dg[order(dg[,1]),]
  
  
  ## Manipulating data for using visNetwork package 
  
  nodes = data.frame(id = dds$pt, title = dds$hlgt_name, label = dds$pt_name,color.border = "black",
                     color.highlight.background = "red", color.highlight.border = "darkred")
  nodes = nodes[!duplicated(nodes$id),]
  
  # Handling PT terms with multiple parents 
  nodes[nodes$label %in% mul_parent_pt, 'title'] = c("Ancillary infectious topics & Autoimmune disorders", 
                                                     "Ancillary infectious topics & Autoimmune disorders & Peripheral neuropathies", 
                                                     "Autoimmune disorders & Peripheral neuropathies", 
                                                     "Ancillary infectious topics & Autoimmune disorders", 
                                                     "Autoimmune disorders & Peripheral neuropathies",
                                                     "Autoimmune disorders & Peripheral neuropathies",
                                                     "Autoimmune disorders & Peripheral neuropathies",
                                                     "Ancillary infectious topics & Autoimmune disorders")
  
  
  nodes$color.background <- c("slategrey", "steelblue", "gold", "tomato", "#ff9999", "green")[as.factor(nodes$title)]
  
  
  links = data.frame(t(apply(dg,1,sort)))
  links = links[!duplicated(links),]
  colnames(links) = c('from','to')
  
  ## Network visualization settings
  vis = visNetwork(nodes, links, width = "1200px", height = "600px") %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "title") %>% 
    visNodes(shape = 'dot', shadow = T, size = 10, borderWidth = 2) %>%
    visEdges(width = 0.1, physics = T , smooth = smooth) %>%
    visPhysics(maxVelocity = 1) 
  
  ## Export the output as a HTML file
  if(!is.null(file)){
    vis %>% visSave(file = paste(file, ".html", sep=''))
  }
  
  ## Display
  vis
  
}



###########
# You should input the ddall data from your local machine.
#'file' is the names of html output
# 'n' is the number of random sample nodes (PT terms) other than PT terms with multiple parents.
data=read.csv("/Users/shnlee/Desktop/RA/igraph/dd.all.csv")
visGBSnetwork(data = data, n = 20, file = 'netVis') 

