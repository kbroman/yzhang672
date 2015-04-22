rm(list=ls())
library(irlba)

load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ffcc.RData")
nc=ncol(allpostfans);nr=nrow(allpostfans)

ns=rep(0,8); for (i in 1:8) {ns[i]=sum(n[1:i])}

drop1=which(rowSums(allpostfans)==rowSums(allpostfans[,1:ns[1]]))
drop2=which(rowSums(allpostfans)==rowSums(allpostfans[,(ns[1]+1):ns[2]]))
drop3=which(rowSums(allpostfans)==rowSums(allpostfans[,(ns[2]+1):ns[3]]))
drop4=which(rowSums(allpostfans)==rowSums(allpostfans[,(ns[3]+1):ns[4]]))
drop5=which(rowSums(allpostfans)==rowSums(allpostfans[,(ns[4]+1):ns[5]]))
drop6=which(rowSums(allpostfans)==rowSums(allpostfans[,(ns[5]+1):ns[6]]))
drop7=which(rowSums(allpostfans)==rowSums(allpostfans[,(ns[6]+1):ns[7]]))
drop8=which(rowSums(allpostfans)==rowSums(allpostfans[,(ns[7]+1):ns[8]]))

dropr=c(drop1,drop2,drop3,drop4,drop5,drop6,drop7,drop8)
length(dropr)

save(dropr, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropr.RData")

allfansdrop = allpostfans[-dropr,]
dim(allfansdrop)
summary(rowSums(allfansdrop))
summary(colSums(allfansdrop))

colsum=colSums(allfansdrop)
wc=which(colsum>0)
allfansdrop = allfansdrop[,wc]
dim(allfansdrop)
summary(rowSums(allfansdrop))
summary(colSums(allfansdrop))

save(allfansdrop, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfansdrop.RData")

cs = colSums(allfansdrop); rs= rowSums(allfansdrop)
Lfansdrop = Diagonal(length(rs), 1/sqrt(rs + mean(rs)))%*%
  allfansdrop%*%Diagonal(length(cs), 1/sqrt(cs + mean(cs)))

sfdrop=irlba(Lfansdrop,nu=20,nv=20)
sfdropd=sfdrop$d[1:20]

save(sfdrop, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallfansdrop.RData")

load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ccdrop.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallfansdrop.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfansdrop.RData")

par(mfrow=c(1,1))
plot(sfdrop$d)
dim(sfdrop$u)
dim(sfdrop$v)

par(mfrow = c(3,1), oma = rep(1,4))
for(i in 1:3) plot(sfdrop$v[,i], ylab = "", xlab="", col = ccdrop)
for(i in 4:6) plot(sfdrop$v[,i], ylab = "", xlab="", col = ccdrop)

v = (sfdrop$v)[,1:6]
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))

km2 = kmeans(v, centers = 2, nstart = 1000)
km3 = kmeans(v, centers = 3, nstart = 1000)
km4 = kmeans(v, centers = 4, nstart = 1000)
km5 = kmeans(v, centers = 5, nstart = 1000)

#save(km2, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm2.RData")
#save(km3, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm3.RData")
#save(km4, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm4.RData")
#save(km5, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm5.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm2.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm3.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm4.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm5.RData")


par(mfrow = c(2,2))
#library(fpc)
plotcluster(v, km2$cluster)
plotcluster(v, km3$cluster)
plotcluster(v, km4$cluster)
plotcluster(v, km5$cluster)

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ndrop.RData")

nclus=4;ncan=8
ndrops=rep(0,8); for (i in 1:8) {ndrops[i]=sum(ndrop[1:i])}
ndrops0=c(0,ndrops)
bp=matrix(0,nrow=nclus,ncol=ncan)
# j candidate, i clusters
for(j in 1:ncan)
{for (i in 1:nclus)
{bp[i,j]=sum(km4$cluster[(ndrops0[j]+1):ndrops0[j+1]]==i)}
}
rownames(bp)=c("cluster1","cluster2","cluster3","cluster4")
colnames(bp)=c("lepen","joly","bayrou","hollande",
               "melenchon","dupont","sarkozy","poutou")
par(mfrow=c(1,1))
balloonPlot(bp)
bp

#choose km3

#par(mfrow = c(1,1))
#library(cluster)
#clusplot(v, km3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# how many posts in each cluster? 
# km3_po
# who are the fans in each cluster?
# km3_poid
# how many fans in each cluster?
# km3_fa
# how many comments(degrees) in each cluster?
# km3_dg
# who are the fans in each cluster?
# km3_faid
# for each fan, how many posts?
# km3_poef
# for each fan, how many comments?
# km3_dgef

dim(allfansdrop)
nr=nrow(allfansdrop)
poef=rep(0,nr)
dgef=rep(0,nr)
for(i in 1:nr)
{poef[i]=sum(allfansdrop[i,]>0)
 dgef[i]=sum(allfansdrop[i,])}

par(mfrow=c(1,2))
hist(poef,breaks=nr)
hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

km3_poid=c(which(km3$cluster==1),which(km3$cluster==2),which(km3$cluster==3))

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ndrop.RData")
nccdrop=rep(0,8)
for(i in 1:8) {nccdrop[i]=sum(ndrop[1:i])}
nccdrop=c(0,nccdrop)
pccdrop=rep(0,length(km3_poid))
for(i in 1:length(km3_poid))
{
  for(j in 1:8)
  {
    if(nccdrop[j]<km3_poid[i] && km3_poid[i]<nccdrop[j+1]+1)
    {pccdrop[i]=j}
  }
}
par(mfrow=c(1,1))
plot(km3_poid,col=pccdrop)

nkm=3
km3_po=rep(0,nkm)
km3_fa=rep(0,nkm)
km3_dg=rep(0,nkm)
for(j in 1:nkm)
{
  km3_po[j]=sum(km3$cluster==j)
  km3_dg[j]=sum(allfansdrop[,km3$cluster==j])
}
par(mfrow=c(2,2))
plot(km3_po,main="posts")
plot(km3_dg,main="degrees")
plot(km3_fa,main="fans")
km3_po
km3_dg
km3_fa

km3_faid=NULL
for(j in 1:nkm)
{
  for(i in 1:nr)
  {if(sum(allfansdrop[i,km3$cluster==j])>0) 
  {km3_fa[j]=km3_fa[j]+1
   km3_faid=c(km3_faid,i)}
  }
}

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ffccdrop.RData")
ffccdrop=c(0,ffccdrop)
fccdrop=rep(0,length(km3_faid))
for(i in 1:length(km3_faid))
{
  for(j in 1:8)
  {
    if(ffccdrop[j]<km3_faid[i] && km3_faid[i]<ffccdrop[j+1]+1)
    {fccdrop[i]=j}
  }
}
#cha=rep(0,8); for (i in 1:8) {cha[i]=ffccdrop[i+1]-ffccdrop[i]}
#cha
#2191  562  894 4110  612   66    8    0
par(mfrow=c(1,1))
plot(km3_faid,col=fccdrop)

km3_poef=matrix(0,nrow=nr,ncol=nkm)
km3_dgef=matrix(0,nrow=nr,ncol=nkm)

for(i in 1:nr)
{
  for(j in 1:nkm)
  {if(sum(allfansdrop[i,km3$cluster==j])>0)
  {km3_poef[i,j]=km3_poef[i,j]+sum(allfansdrop[i,km3$cluster==j]>0)
   km3_dgef[i,j]=km3_dgef[i,j]+sum(allfansdrop[i,km3$cluster==j])
  }
  }
}

par(mfrow=c(nkm,2))
for(j in 1:nkm)
{hist(km3_poef[km3_poef[,j]>0,j],breaks=nr)
 hist(km3_dgef[km3_dgef[,j]>0,j],breaks=nr)
}

summary(km3_poef[km3_poef[,1]>0,1])
summary(km3_poef[km3_poef[,2]>0,2])
summary(km3_poef[km3_poef[,3]>0,3])
summary(km3_dgef[km3_dgef[,1]>0,1])
summary(km3_dgef[km3_dgef[,2]>0,2])
summary(km3_dgef[km3_dgef[,3]>0,3])


for (j in 1:nkm)
{plot(km3_poef[km3_poef[,j]>0,j])
 plot(km3_dgef[km3_dgef[,j]>0,j])}

save(poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrpoef.RData")
save(dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrdgef.RData")
save(km3_poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm3poef.RData")
save(km3_dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm3dgef.RData")
save(km3_poid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm3poid.RData")
save(km3_po, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm3po.RData")
save(km3_fa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm3fa.RData")
save(km3_dg, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm3dg.RData")
save(km3_faid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropstrkm3faid.RData")



Y = model.matrix(~as.factor(ccdrop)-1)
dim(Y)
head(Y)
yy = solve(t(Y)%*%Y)
a=allfansdrop
dim(a)
ap = a%*%Y%*%yy
apn =t(apply(ap,1,function(x) return(x/sqrt(sum(x^2)))))

kmap3 = kmeans(apn, 3, nstart = 5)
table(kmap3$clust)
Zhat = sparse.model.matrix(~as.factor(kmap3$clust)-1)
zz = solve(t(Zhat)%*%Zhat)
bhat = zz%*%t(Zhat)%*%a%*%Y%*%yy
round(bhat*length(sh_cc))

kmap4 = kmeans(apn, 4, nstart = 5)
table(kmap4$clust)
Zhat = sparse.model.matrix(~as.factor(kmap4$clust)-1)
zz = solve(t(Zhat)%*%Zhat)
bhat = zz%*%t(Zhat)%*%a%*%Y%*%yy
round(bhat*length(ccdrop))




