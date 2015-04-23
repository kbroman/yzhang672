#rm(list = ls())
#load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans_7.RData')
#load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')
#n_7=n[1:7]
#cc_7 = c()
#for(i in 1:length(n_7)) cc_7 = c(cc_7, rep(i,n[i]))

#a=allpostfans_7

#Y = model.matrix(~as.factor(cc_7)-1)
#yy = solve(t(Y)%*%Y)
#hc=Y%*%yy%*%t(Y)
#I=diag(1,length(cc_7))

#nc = ncol(a)
#cs = colSums(a)
#tau = cs/nc
#Dcol = Diagonal(n = nc, x = 1/(cs + tau))

#Lb=Dcol%*%(I-hc)
#L=a%*%Lb

#library(irlba)
#sdelete_7 = irlba(L, nu = 20, nv = 20)

#save(sdelete_7, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svddeletestructure_7.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svddeletestructure_7.RData")
#sdelete_7

plot(sdelete_7$d)
dim(sdelete_7$u)
dim(sdelete_7$v)
v = sdelete_7$v
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))

par(mfrow = c(5,1), mar = rep(1,4))
for(i in 1:5) plot(v[,i], ylab = "", xlab="", col = cc_7)
for(i in 6:10) plot(v[,i], ylab = "", xlab="", col = cc_7)
for(i in 11:15) plot(v[,i], ylab = "", xlab="", col = cc_7)
for(i in 16:20) plot(v[,i], ylab = "", xlab="", col = cc_7)

#########################################################################


km2 = kmeans(v, centers = 2, nstart = 1000)
km3 = kmeans(v, centers = 3, nstart = 1000)
km4 = kmeans(v, centers = 4, nstart = 1000)
km5 = kmeans(v, centers = 5, nstart = 1000)

par(mfrow = c(2,2))
plot(rnorm(cc_7,cc_7,.05), rnorm(km2$clust, km2$clust, .05), col=cc_7) 
plot(rnorm(cc_7,cc_7,.05), rnorm(km3$clust, km3$clust, .05), col=cc_7)
plot(rnorm(cc_7,cc_7,.05), rnorm(km4$clust, km4$clust, .05), col=cc_7)
plot(rnorm(cc_7,cc_7,.05), rnorm(km5$clust, km5$clust, .05), col=cc_7)

par(mfrow = c(2,2))
library(cluster)
clusplot(v, km2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(v, km3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(v, km4$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(v, km5$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

par(mfrow=c(1,1))
clusplot(v, km4$cluster, color=TRUE, shade=TRUE, labels=2, lines=0,xlim=c(0.5,1.5),ylim=c(4.1,4.9))

par(mfrow = c(2,2))
#library(fpc)
plotcluster(v, km2$cluster)
plotcluster(v, km3$cluster)
plotcluster(v, km4$cluster)
plotcluster(v, km5$cluster)


dim(allpostfans_7)
nr=nrow(allpostfans_7)
poef=rep(0,nr)
dgef=rep(0,nr)
for(i in 1:nr)
{poef[i]=sum(allpostfans_7[i,]>0)
 dgef[i]=sum(allpostfans_7[i,])}
#hist(poef,breaks=nr)
#hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

nkm=3
km3_po=rep(0,nkm)
km3_fa=rep(0,nkm)
km3_dg=rep(0,nkm)
km3_faid=NULL
for(j in 1:nkm)
{
  km3_po[j]=sum(km3$cluster==j)
  km3_dg[j]=sum(allpostfans_7[,km3$cluster==j])
}

for(j in 1:nkm)
{
  for(i in 1:nr)
  {if(sum(allpostfans_7[i,km3$cluster==j])>0) 
  {km3_fa[j]=km3_fa[j]+1
   km3_faid=c(km3_faid,i)}
  }
}
#plot(km3_po)
#plot(km3_dg)
#plot(km3_fa)
km3_po
km3_dg
km3_fa

par(mfrow=c(1,1))
plot(km3_faid)

km3_poef=matrix(0,nrow=nr,ncol=nkm)
km3_dgef=matrix(0,nrow=nr,ncol=nkm)

for(i in 1:nr)
{
  for(j in 1:nkm)
  {if(sum(allpostfans_7[i,km3$cluster==j])>0)
  {km3_poef[i,j]=km3_poef[i,j]+sum(allpostfans_7[i,km3$cluster==j]>0)
   km3_dgef[i,j]=km3_dgef[i,j]+sum(allpostfans_7[i,km3$cluster==j])
  }
  }
}

#par(mfrow=c(nkm,2))
#for(j in 1:nkm)
#{hist(km3_poef[km3_poef[,j]>0,j],breaks=nr)
# hist(km3_dgef[km3_dgef[,j]>0,j],breaks=nr)
#}

summary(km3_poef[km3_poef[,1]>0,1])
summary(km3_poef[km3_poef[,2]>0,2])
summary(km3_poef[km3_poef[,3]>0,3])
summary(km3_dgef[km3_dgef[,1]>0,1])
summary(km3_dgef[km3_dgef[,2]>0,2])
summary(km3_dgef[km3_dgef[,3]>0,3])

#for(j in 1:nkm)
#{hist(km3_dgef[km3_dgef[,j]>15,j],breaks=nr)
#}

#for (j in 1:nkm)
#{plot(km3_poef[km3_poef[,j]>0,j])
# plot(km3_dgef[km3_dgef[,j]>0,j])}

#save(poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrpoef.RData")
#save(dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrdgef.RData")
#save(km3_poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3poef.RData")
#save(km3_dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3dgef.RData")
#save(km3_po, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3po.RData")
#save(km3_fa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3fa.RData")
#save(km3_dg, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3dg.RData")
#save(km3_faid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3faid.RData")
