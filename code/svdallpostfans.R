rm(list = ls())

library(Matrix)

load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')

rs=rowSums(allpostfans)
cs=colSums(allpostfans)
summary(rs); summary(cs)
L = Diagonal(length(rs), 1/sqrt(rs + mean(rs)))%*%
  allpostfans%*%Diagonal(length(cs), 1/sqrt(cs + mean(cs)))

library(irlba)
sf=irlba(L,nu=20,nv=20)
sfd=sf$d[1:20]
sfd

save(sf, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallpostfannorm.RData")





load('F:/UWMadison/project/previous work/fordesk00/mydata/tables.RData')
lepe = tables$lepen
joly = tables$joly
bayr = tables$bayrou
holl = tables$hollande
mele = tables$melenchon
dupo = tables$dupont
sark = tables$sarkozy
pout = tables$poutou

n=rep(0,8)
n[1]=length(unique(lepe$parent_url));n[2]=length(unique(joly$parent_url));
n[3]=length(unique(bayr$parent_url));n[4]=length(unique(holl$parent_url));
n[5]=length(unique(mele$parent_url));n[6]=length(unique(dupo$parent_url));
n[7]=length(unique(sark$parent_url));n[8]=length(unique(pout$parent_url));
                                                
nc=ncol(allpostfans);nr=nrow(allpostfans)
psum=matrix(rep(0,8*nr),nrow=nr);x=rep(0,nr)
dropc=NULL
for (i in 1:nr)
{
  x=allpostfans[i,]
  
  psum[i,1]=sum(x[1:n[1]])
  for (c in 2:8)
  {psum[i,c]=sum(x[(n[c-1]+1):n[c]])}
  
  if (sum(psum[i,])-max(psum[i,])==0) {dropc=c(dropc,i)}
  
}


allfansdrop = allpostfans[-dropc,]
cs = colSums(allfansdrop)
Lfansdrop = Diagonal(length(rs), 1/sqrt(rs + mean(rs)))%*%
  allfansdrop%*%Diagonal(length(cs), 1/sqrt(cs + mean(cs)))

sfdrop=irlba(Lfansdrop,nu=20)
sfdropd=sfdrop$d[1:20]
#sfdropall=irlba(allfansdrop,nu=20)
#sfdropalld=sfdropall$d[1:20]

#save(sfdropalld, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallfansdrop.RData")
save(sfdropd, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallfansdropnorm.RData")
