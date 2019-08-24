rm(list=ls())
library(microbenchmark)
library(Rcpp)
library(BayesLogit)
library(igraph)
library(Matrix)
library(spam)

setwd("/Users/shnlee/Desktop/RA2019/R_Cpp_simulation")
sourceCpp("getL.cpp")
{
set.seed(290435)
adjMat <- rbind(
  c(0,1,1,0,0,0,0),
  c(1,0,1,1,0,0,0),
  c(1,1,0,0,0,0,0),
  c(0,1,0,0,1,1,1),
  c(0,0,0,1,0,1,0),
  c(0,0,0,1,1,0,1),
  c(0,0,0,1,0,1,0))

addm=matrix(rep(1,9),nrow=3)
diag(addm) <-0
adjMat=bdiag(adjMat,addm)
addm=matrix(rep(0,9),nrow=3)
diag(addm) <-0
adjMat=bdiag(adjMat,addm)
g  <- graph.adjacency(adjMat, mode = "undirected")
ne=ecount(g)
lapMat <- laplacian_matrix(g)
nb=nrow(adjMat)
n.kid=sample(c(15,20,25,30)*5,nb,replace=TRUE)
N=sum(n.kid)
mean.parent=rep(0,nb)
set.seed(290435)
phi=rmvnorm.prec(1,mean.parent,lapMat+diag(1,nb))
phi.id=rep(phi,n.kid)
logE=rnorm(N)
scale.s=exp(phi.id+logE)
lams=sapply(1:N,function(j) rgamma(1,1,scale=scale.s[j]))
y=rpois(N,c(lams)) 
L=lapMat
A=adjMat
id=rep(rep(1:nb),n.kid)
indexL=sapply(1:nb,function(s) min(which(id==s)))
indexU=sapply(1:nb,function(s) max(which(id==s)))
Ldiag=diag(L)
L_b=matrix(NA,nb,nb-1)
for(i in 1:nb){
  L_b[i,]=L[-i,i]
  }
}
#########R Simulation##################################
# prior
d0<-g0<-0.1  # for taub
e_0=1 # a value too small (e.g 0.1) cause numeric error for updating r
f_0=1
#initial values
gam=1
b=rep(0,nb)
r=1
b.id=rep(b,n.kid)

# save draws
nsim=500
burn=100
thin=5
nit <- (nsim-burn)/thin	# number of iterations stored 
b.save=matrix(0,nit,nb)
r.save=rep(0,nit)
lam.save=matrix(0,nit,N)
gam.save=rep(0,nit)
vd=md=rep(0,nb)

set.seed(42)
system.time({
  for(j in 1:nsim){
    # Update latents
    om = rpg(N, y+r, b.id+logE)
    k=(y-r)/2 # parameter in GP
    zb=k/om-logE
    for(i in 1:nb){
      vd= 1/(sum(om[indexL[i]:indexU[i]])+gam*L[i,i])
      md=vd*(sum((om*zb)[indexL[i]:indexU[i]])-gam*crossprod(L[-i,i],b[-i]))
      b[i]=rnorm(1,md,sqrt(vd))
    }
    b.id=rep(b,n.kid)
    sumpairb=as((t(b)%*%L%*%b),"vector")
    gam<-rgamma(1,g0+ne/2,rate=(2+d0*sumpairb)/(2*d0))
    p=1/(1+exp(-b.id-logE)) 
    l=LsumC(y,r)
    r=rgamma(1,e_0+l, rate=f_0-sum(log(1-p))); 
    if(j%%1000==0) print(j)
    if (j> burn & j%%thin==0){
      jj <- (j-burn)/thin
      b.save[jj,]=b
      r.save[jj]=r
      gam.save[jj]=gam
      lam.save[jj,]=rgamma(N,r+y,rate=(1-p)/p+1) 
    }
  }
})
mean(r.save)
mean(gam.save)
mean(b.save)

#########
sourceCpp("GibbsC_Andrew.cpp")
system.time({
  set.seed(42)
  res=GibbsC_Andrew(y, logE, ne, L, Ldiag, L_b, 
                     nsim=500, nburn=100,thin=5, nkid=n.kid,
                     d0= 0.1, g0= 0.1, e0=1, f0=1)
})

mean(res$rsave)
mean(res$gamsave)
mean(res$bsave)
