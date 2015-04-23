rm(list = ls())

#library(Matrix)

#load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshpostfans.RData')

#rs=rowSums(bsh_postfans)
#cs=colSums(bsh_postfans)
#summary(rs); summary(cs)
#L = Diagonal(length(rs), 1/sqrt(rs + mean(rs)))%*%
#  bsh_postfans%*%Diagonal(length(cs), 1/sqrt(cs + mean(cs)))

#library(irlba)
#bsh_sf=irlba(L,nu=20,nv=20)
#sfd=bsh_sf$d[1:20]
#plot(sfd)

#save(bsh_sf, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdbshpostfan.RData")

rm(list = ls())

load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshpostfans.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdbshpostfan.RData")
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')

par(mfrow=c(1,1))
plot(bsh_sf$d)
dim(bsh_sf$u)
dim(bsh_sf$v)
v = bsh_sf$v[,1:5]
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))

cc = c()
for(i in 1:length(n)) cc = c(cc, rep(i,n[i]))
b_cc=cc[(sum(n[1:2])+1):sum(n[1:3])]
h_cc=cc[(sum(n[1:3])+1):sum(n[1:4])]
s_cc=cc[(sum(n[1:6])+1):sum(n[1:7])]
bsh_cc=c(b_cc,h_cc,s_cc)

par(mfrow = c(5,1), oma = rep(1,4))
for(i in 1:5) plot(v[,i], ylab = "", xlab="", col = bsh_cc)

km3 = kmeans(v, centers = 3, nstart = 1000)
km4 = kmeans(v, centers = 4, nstart = 1000)
km5 = kmeans(v, centers = 5, nstart = 1000)
km6 = kmeans(v, centers = 6, nstart = 1000)

save(km2, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm2.RData")
save(km3, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3.RData")
save(km4, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4.RData")
save(km5, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5.RData")

par(mfrow = c(2,2))
library(fpc)
plotcluster(v, km3$cluster)
plotcluster(v, km4$cluster)
plotcluster(v, km5$cluster)
plotcluster(v, km6$cluster)

#choose km3

par(mfrow = c(1,1))
library(cluster)
clusplot(v, km3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# how many posts in each cluster? 
# km3_po
# what posts?
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

dim(bsh_postfans)
nr=nrow(bsh_postfans)
poef=rep(0,nr)
dgef=rep(0,nr)
for(i in 1:nr)
{poef[i]=sum(bsh_postfans[i,]>0)
 dgef[i]=sum(bsh_postfans[i,])}
hist(poef,breaks=nr)
hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

nkm=3

km3_poid=NULL;
for (i in 1:nkm) {km3_poid=c(km3_poid,which(km3$cluster==i))}

par(mfrow=c(1,1))
plot(km3_poid)

km3_po=rep(0,nkm)
km3_fa=rep(0,nkm)
km3_dg=rep(0,nkm)
km3_faid=NULL
for(j in 1:nkm)
{
  km3_po[j]=sum(km3$cluster==j)
  km3_dg[j]=sum(bsh_postfans[,km3$cluster==j])
}

for(j in 1:nkm)
{
  for(i in 1:nr)
  {if(sum(bsh_postfans[i,km3$cluster==j])>0) 
  {km3_fa[j]=km3_fa[j]+1
   km3_faid=c(km3_faid,i)}
  }
}
plot(km3_po)
plot(km3_dg)
plot(km3_fa)
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
  {if(sum(bsh_postfans[i,km3$cluster==j])>0)
  {km3_poef[i,j]=km3_poef[i,j]+sum(bsh_postfans[i,km3$cluster==j]>0)
   km3_dgef[i,j]=km3_dgef[i,j]+sum(bsh_postfans[i,km3$cluster==j])
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

for(j in 1:nkm)
{hist(km3_dgef[km3_dgef[,j]>15,j],breaks=nr)
}

for (j in 1:nkm)
{plot(km3_poef[km3_poef[,j]>0,j])
 plot(km3_dgef[km3_dgef[,j]>0,j])}

#save(poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrpoef.RData")
#save(dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrdgef.RData")
#save(km3_poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3poef.RData")
#save(km3_dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3dgef.RData")
#save(km3_po, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3po.RData")
#save(km3_fa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3fa.RData")
#save(km3_dg, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3dg.RData")
#save(km3_faid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3faid.RData")


# how many posts in each cluster? 
# km4_po
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

dim(bsh_postfans)
nr=nrow(bsh_postfans)
poef=rep(0,nr)
dgef=rep(0,nr)
for(i in 1:nr)
{poef[i]=sum(bsh_postfans[i,]>0)
 dgef[i]=sum(bsh_postfans[i,])}
hist(poef,breaks=nr)
hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

nkm=4
km4_po=rep(0,nkm)
km4_fa=rep(0,nkm)
km4_dg=rep(0,nkm)
km4_faid=NULL
for(j in 1:nkm)
{
  km4_po[j]=sum(km4$cluster==j)
  km4_dg[j]=sum(bsh_postfans[,km4$cluster==j])
}

for(j in 1:nkm)
{
  for(i in 1:nr)
  {if(sum(bsh_postfans[i,km4$cluster==j])>0) 
  {km4_fa[j]=km4_fa[j]+1
   km4_faid=c(km4_faid,i)}
  }
}
plot(km4_po)
plot(km4_dg)
plot(km4_fa)
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
  {if(sum(bsh_postfans[i,km4$cluster==j])>0)
  {km4_poef[i,j]=km4_poef[i,j]+sum(bsh_postfans[i,km4$cluster==j]>0)
   km4_dgef[i,j]=km4_dgef[i,j]+sum(bsh_postfans[i,km4$cluster==j])
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
summary(km4_poef[km4_poef[,4]>0,4])
summary(km4_dgef[km4_dgef[,1]>0,1])
summary(km4_dgef[km4_dgef[,2]>0,2])
summary(km4_dgef[km4_dgef[,3]>0,3])
summary(km4_dgef[km4_dgef[,4]>0,4])

for(j in 1:nkm)
{hist(km4_dgef[km4_dgef[,j]>15,j],breaks=nr)
}

for (j in 1:nkm)
{plot(km4_poef[km4_poef[,j]>0,j])
 plot(km4_dgef[km4_dgef[,j]>0,j])}

#save(poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrpoef.RData")
#save(dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrdgef.RData")
save(km4_poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4poef.RData")
save(km4_dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4dgef.RData")
#save(km4_po, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4po.RData")
#save(km4_fa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4fa.RData")
#save(km4_dg, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4dg.RData")
#save(km4_faid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4faid.RData")




nkm=5
km5_po=rep(0,nkm)
km5_fa=rep(0,nkm)
km5_dg=rep(0,nkm)
km5_faid=NULL
for(j in 1:nkm)
{
  km5_po[j]=sum(km5$cluster==j)
  km5_dg[j]=sum(bsh_postfans[,km5$cluster==j])
}

for(j in 1:nkm)
{
  for(i in 1:nr)
  {if(sum(bsh_postfans[i,km5$cluster==j])>0) 
  {km5_fa[j]=km5_fa[j]+1
   km5_faid=c(km5_faid,i)}
  }
}
plot(km5_po)
plot(km5_dg)
plot(km5_fa)
km5_po
km5_dg
km5_fa

par(mfrow=c(1,1))
plot(km5_faid)

km5_poef=matrix(0,nrow=nr,ncol=nkm)
km5_dgef=matrix(0,nrow=nr,ncol=nkm)

for(i in 1:nr)
{
  for(j in 1:nkm)
  {if(sum(bsh_postfans[i,km5$cluster==j])>0)
  {km5_poef[i,j]=km5_poef[i,j]+sum(bsh_postfans[i,km5$cluster==j]>0)
   km5_dgef[i,j]=km5_dgef[i,j]+sum(bsh_postfans[i,km5$cluster==j])
  }
  }
}

par(mfrow=c(nkm,2))
for(j in 1:nkm)
{hist(km5_poef[km5_poef[,j]>0,j],breaks=nr)
 hist(km5_dgef[km5_dgef[,j]>0,j],breaks=nr)
}

summary(km5_poef[km5_poef[,1]>0,1])
summary(km5_poef[km5_poef[,2]>0,2])
summary(km5_poef[km5_poef[,3]>0,3])
summary(km5_poef[km5_poef[,4]>0,4])
summary(km5_poef[km5_poef[,5]>0,5])
summary(km5_dgef[km5_dgef[,1]>0,1])
summary(km5_dgef[km5_dgef[,2]>0,2])
summary(km5_dgef[km5_dgef[,3]>0,3])
summary(km5_dgef[km5_dgef[,4]>0,4])
summary(km5_dgef[km5_dgef[,5]>0,5])

for(j in 1:nkm)
{hist(km5_dgef[km5_dgef[,j]>15,j],breaks=nr)
}

for (j in 1:nkm)
{plot(km5_poef[km5_poef[,j]>0,j])
 plot(km5_dgef[km5_dgef[,j]>0,j])}

#save(km5_poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5poef.RData")
#save(km5_dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5dgef.RData")
#save(km5_po, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5po.RData")
#save(km5_fa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5fa.RData")
#save(km5_dg, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5dg.RData")
#save(km5_faid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5faid.RData")


Y = model.matrix(~as.factor(bsh_cc)-1)
dim(Y)
head(Y)
yy = solve(t(Y)%*%Y)
a=bsh_postfans
dim(a)
ap = a%*%Y%*%yy
apn =t(apply(ap,1,function(x) return(x/sqrt(sum(x^2)))))

kmap3 = kmeans(apn, 3, nstart = 5)
table(kmap3$clust)
Zhat = sparse.model.matrix(~as.factor(kmap3$clust)-1)
zz = solve(t(Zhat)%*%Zhat)
bhat = zz%*%t(Zhat)%*%a%*%Y%*%yy
round(bhat*length(bsh_cc))

kmap4 = kmeans(apn, 4, nstart = 5)
table(kmap4$clust)
Zhat = sparse.model.matrix(~as.factor(kmap4$clust)-1)
zz = solve(t(Zhat)%*%Zhat)
bhat = zz%*%t(Zhat)%*%a%*%Y%*%yy
round(bhat*length(bsh_cc))

kmap5 = kmeans(apn, 5, nstart = 5)
table(kmap5$clust)
Zhat = sparse.model.matrix(~as.factor(kmap5$clust)-1)
zz = solve(t(Zhat)%*%Zhat)
bhat = zz%*%t(Zhat)%*%a%*%Y%*%yy
round(bhat*length(bsh_cc))


