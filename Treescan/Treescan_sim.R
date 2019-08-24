rm(list=ls())
setwd('/Users/shnlee/Desktop/RA2019/Treescan/examples/')

### Creat a count list data by all possible cuts(PT and higer level terms) 
cnt_data = read.csv('sample_cnt.csv', header = F, stringsAsFactors = F)
colnames(cnt_data) = c("Node", "observed", "population")
tree_data = read.csv('tree.csv', header = F, stringsAsFactors = F)
colnames(tree_data) = c("child", "parent")

node_list = unique(c(tree_data$child, tree_data$parent))
node_list = node_list[-length(node_list)]

data = data.frame(cut = node_list, observed = NA, population = NA)
data$cut = as.character(data$cut)

for(i in 1:nrow(data)){
  node = data$cut[i]
  child = tree_data[tree_data$parent == node,'child']
  
  if(length(child) != 0){
    child_data = cnt_data[cnt_data$Node %in% child,]
    if(nrow(child_data) == 0){
      child_data = data[data$cut %in% child,]
    } 
    data$observed[i] = sum(child_data$observed)
    data$population[i] = sum(child_data$population)
    
  } else{
    data$observed[i] = cnt_data[cnt_data$Node == node,'observed']
    data$population[i] = cnt_data[cnt_data$Node == node,'population'] 
  }
}

data = data[1:18,]

## Calculate LLR for each cut
data$expected = data$population * 55/4100
data$I = ((data$observed/data$expected) > (55-data$observed)/(55-data$expected))*1
data$LLR = data$I * (data$observed*log(data$observed/data$expected) + 
                            (55-data$observed)*log((55-data$observed)/(55-data$expected)))


## Calculate p-value using llr file from Treescan software
llr = read.csv('Poisson_llr.csv')$LLR
data$pVal_ts = (10001 - sapply(round(data$LLR,6), function(x){rank(c(x, llr), ties.method = 'min')[1]}))/10000
data$LLR = round(data$LLR, 6)
data %>% arrange(pVal_ts) %>% filter(!is.na(LLR))


## Calculate p-value using simulation data generated manually
n.sim = 9999
sim_LLRs = c()

for(i in 1:n.sim){
  sim.data = data[,c(1,4)]
  sim.data$observed = rpois(18, sim.data$expected)
  sim.data$I = ((sim.data$observed/sim.data$expected) > (55-sim.data$observed)/(55-sim.data$expected))*1
  sim.data$LLR = sim.data$I * (round(sim.data$observed*log(sim.data$observed/sim.data$expected) + 
                            (55-sim.data$observed)*log((55-sim.data$observed)/(55-sim.data$expected)),6))
  max_llr = max(sim.data$LLR, na.rm = T)
  sim_LLRs = c(sim_LLRs, max_llr)
}

data$pVal_sim = (10001 - sapply(round(data$LLR,6), function(x){rank(c(x, sim_LLRs), ties.method = 'max')[1]}))/10000
data$LLR = round(data$LLR, 6)
data %>% arrange(pVal_sim) %>% filter(!is.na(LLR))


######## Compare distribution between 'sim_LLRs' and 'llr'

#sim_LLRs: List of maximum LLR generated manually in R
#llr: List of maximum LLR generated from Treescan software
hist(sim_LLRs, col=rgb(0,0,1,1/4), xlim=c(0,max(sim_LLRs)), ylim = c(0,3000),
     breaks = seq(0,max(sim_LLRs)+1, by=0.5))
hist(llr, col=rgb(1,0,0,1/4), xlim=c(0,max(sim_LLRs)), ylim = c(0,3000),
     breaks = seq(0,max(sim_LLRs)+1, by=0.5), add=T)

