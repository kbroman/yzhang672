rm(list = ls())

library(Matrix)

load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shpostfans.RData')
dropsh1=which(rowSums(sh_postfans)==rowSums(sh_postfans[,1:n[4]]))
dropsh2=which(rowSums(sh_postfans)==rowSums(sh_postfans[,(n[4]+1):(n[4]+n[7])]))

dropshr=c(dropsh1,dropsh2)
length(dropshr)

sh_fansdrop = sh_postfans[-dropshr,]
dim(sh_fansdrop)
summary(rowSums(sh_fansdrop))
summary(colSums(sh_fansdrop))

colsum=colSums(sh_fansdrop)
wc=which(colsum>0)
sh_fansdrop = sh_fansdrop[,wc]
dim(sh_fansdrop)
summary(rowSums(sh_fansdrop))
summary(colSums(sh_fansdrop))

save(sh_fansdrop, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shfansdrop.RData")


rs=rowSums(sh_postfans)
cs=colSums(sh_postfans)
summary(rs); summary(cs)
L = Diagonal(length(rs), 1/sqrt(rs + mean(rs)))%*%
sh_postfans%*%Diagonal(length(cs), 1/sqrt(cs + mean(cs)))

library(irlba)
sh_sfdrop=irlba(L,nu=20,nv=20)
sfddrop=sh_sf$d[1:20]
plot(sfd)

save(sh_sfdrop, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdshdropfans.RData")

rm(list = ls())
library(cluster)
library(fpc)

load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shdropfans.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdshdropfans.RData")
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shdropfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dropshr.RData')

par(mfrow=c(1,1))
plot(sh_sfdrop$d)
dim(sh_sfdrop$u)
dim(sh_sfdrop$v)
v = sh_sfdrop$v[,1:4]
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))

cc = c()
for(i in 1:length(n)) cc = c(cc, rep(i,n[i]))
h_cc=cc[(sum(n[1:3])+1):sum(n[1:4])]
s_cc=cc[(sum(n[1:6])+1):sum(n[1:7])]
sh_cc=c(h_cc,s_cc)
shdrop_cc=sh_cc[-dropshr]

par(mfrow = c(4,1), mar = rep(1,4))
for(i in 1:4) plot(v[,i], ylab = "", xlab="", col = shdrop_cc)

km4 = kmeans(v, centers = 4, nstart = 1000)
save(km4, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shdropkm4.RData")

par(mfrow=c(1,1))
#library(fpc)
plotcluster(v, km4$cluster)
#library(cluster)
clusplot(v, km4$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# how many posts in each cluster? 
# km4_po
# who are the fans in each cluster?
# km4_poid
# how many fans in each cluster?
# km4_fa
# how many comments(degrees) in each cluster?
# km4_dg
# who are the fans in each cluster?
# km4_faid
# for each fan, how many posts?
# km4_poef
# for each fan, how many comments?
# km4_dgef

dim(sh_postfans)
nr=nrow(sh_postfans)
poef=rep(0,nr)
dgef=rep(0,nr)
for(i in 1:nr)
{poef[i]=sum(sh_postfans[i,]>0)
 dgef[i]=sum(sh_postfans[i,])}

par(mfrow=c(1,2))
hist(poef,breaks=nr)
hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

nkm=4

km4_poid=NULL;
for (i in 1:nkm) {km4_poid=c(km4_poid,which(km4$cluster==i))}

par(mfrow=c(1,1))
plot(km4_poid)

km4_po=rep(0,nkm)
km4_fa=rep(0,nkm)
km4_dg=rep(0,nkm)

for(j in 1:nkm)
{
  km4_po[j]=sum(km4$cluster==j)
  km4_dg[j]=sum(sh_postfans[,km4$cluster==j])
}

km4_faid=NULL
for(j in 1:nkm)
{
  for(i in 1:nr)
  {if(sum(sh_postfans[i,km4$cluster==j])>0) 
  {km4_fa[j]=km4_fa[j]+1
   km4_faid=c(km4_faid,i)}
  }
}

par(mfrow=c(2,2))
plot(km4_po,main="posts")
plot(km4_dg,main="degrees")
plot(km4_fa,main="fans")
km4_po
km4_dg
km4_fa

par(mfrow=c(1,1))
plot(km4_faid)

km4_poef=matrix(0,nrow=nr,ncol=nkm)
km4_dgef=matrix(0,nrow=nr,ncol=nkm)

for(i in 1:nr)
{
  for(j in 1:nkm)
  {if(sum(sh_postfans[i,km4$cluster==j])>0)
  {km4_poef[i,j]=km4_poef[i,j]+sum(sh_postfans[i,km4$cluster==j]>0)
   km4_dgef[i,j]=km4_dgef[i,j]+sum(sh_postfans[i,km4$cluster==j])
  }
  }
}

par(mfrow=c(nkm,2))
for(j in 1:nkm)
{hist(km4_poef[km4_poef[,j]>0,j],breaks=nr)
 hist(km4_dgef[km4_dgef[,j]>0,j],breaks=nr)
}

summary(km4_poef[km4_poef[,1]>0,1])
summary(km4_poef[km4_poef[,2]>0,2])
summary(km4_poef[km4_poef[,3]>0,3])
summary(km4_dgef[km4_dgef[,1]>0,1])
summary(km4_dgef[km4_dgef[,2]>0,2])
summary(km4_dgef[km4_dgef[,3]>0,3])

for (j in 1:nkm)
{plot(km4_poef[km4_poef[,j]>0,j])
 plot(km4_dgef[km4_dgef[,j]>0,j])}

save(poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrpoef.RData")
save(dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrdgef.RData")
save(km4_poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4poef.RData")
save(km4_dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4dgef.RData")
save(km4_poid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4poid.RData")
save(km4_po, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4po.RData")
save(km4_fa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4fa.RData")
save(km4_dg, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4dg.RData")
save(km4_faid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4faid.RData")
