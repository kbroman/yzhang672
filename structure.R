rm(list = ls())

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallpostfannorm.RData")
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cc.RData')
<<<<<<< HEAD
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/postdates.RData")
#alldates,unidate,mdates

ncluster=8;ndate=length(unidate);npost=length(allpostfans[1,])

par(mfrow=c(1,1))
plot(sf$d,ylab="singular values")
v = sf$v[,1:ncluster]
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))

svbd = sv_bydate(v, mdates, cc, npost, ncluster)
vd = svbd$vd; vdl = svbd$vdl; dfvdl = svbd$dfvdl; daten = svbd$daten
=======
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostdate.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostdatetrue.RData")

plot(sf$d)
dim(sf$u)
dim(sf$v)
v = sf$v[,1:8]
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))

km8 = kmeans(v, centers = 8, nstart = 1000)
#save(km8, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/strkm8.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/strkm8.RData")

u = sf$u[ ,1:8]
u = t(apply(u,1,function(x) return(x/sqrt(sum(x^2)))))
km8u = kmeans(u, centers = 8, nstart = 1000)



id=1:3239
dfvd=data.frame(v,datem,cc,id)
dfvdo=dfvd[order(dfvd$cc,dfvd$datem),]
dfvdo=as.matrix(dfvdo)
vd=dfvdo[,1:8]

dfvdl=dfvd[order(dfvd$datem),]
dfvdl=as.matrix(dfvdl)
vdl=dfvdl[,1:8]

datem_od=dfvdl[,9]
daten=rep(1,53)
for(i in 2:53){daten[i]=sum(datem_od<=(i-1))+1}

late=dfvdl[daten[49]:3239,11]
late=as.vector(late)
csl=colSums(allpostfans[,late])
cs=colSums(allpostfans[,])
summary(cs);summary(csl)
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a

par(mfrow = c(4,1), oma=rep(1,4))
for(i in 1:4) plot(vd[,i], ylab = "", xlab="", col = cc)
for(i in 5:8) plot(vd[,i], ylab = "", xlab="", col = cc)

<<<<<<< HEAD
for(i in 1:4) plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,(ncluster+2)])
for(d in 1:ndate) abline(v=daten[d],col="grey")
abline(v=daten[55]);abline(v=daten[87]);abline(v=daten[101]);
for(i in 5:8) plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,(ncluster+2)])
for(d in 1:ndate) abline(v=daten[d],col="grey")
abline(v=daten[55]);abline(v=daten[87]);abline(v=daten[101]);

km8 = kmeans(v, centers = ncluster, nstart = 1000)
#save(km8, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/strkm8.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/strkm8.RData")
=======
for(i in 1:4) plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,10])
for(i in 5:8) plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,10])
for(d in 1:53) abline(v=daten[d])
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a

par(mfrow=c(1,1))
library(fpc)
plotcluster(v, km8$cluster)

<<<<<<< HEAD
id=1:npost
kmcluster=km8$cluster
df=data.frame(kmcluster,cc,id)
dfo=df[order(df$kmcluster),]
ndfo=rep(0,ncluster)
for (i in 1:ncluster){ndfo[i]=sum(kmcluster==i)}
nmatr=matrix(0,ncol=ncluster,nrow=ncluster)
=======
kmcluster=km8$cluster
df=data.frame(kmcluster,cc,id)
dfo=df[order(df$kmcluster),]
ndfo=rep(0,8)
for (i in 1:8){ndfo[i]=sum(kmcluster==i)}
nmatr=matrix(0,ncol=8,nrow=8)
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
for (i in 1:8)
{
  for (j in 1:8)
  {
    nmatr[i,j]=sum(df$kmcluster==i & df$cc==j)
  }
}
#posts in clusters
<<<<<<< HEAD
rn = c("Pg 1", "Pg 2", "Pg 3", "Pg 4", "Pg 5", "Pg 6", "Pg 7", "Pg 8")
cn = c("lepe", "joly", "bayr", "holl", "mele", "dupo", "sark", "pout")

rownames(nmatr)=rn
colnames(nmatr)=cn

nmatr
par(mfrow=c(1,1))
balloonPlot(nmatr,TRUE, FALSE, "post clusters",cn,rn,4,1)

B0=createB(cc,allpostfans,ncluster)

B1=B0[c(8,3,5,2,1,6,7,4),]
B=matrix(rep(0,64),nrow=8)
for(i in 1:8)
{B[i,]=B1[i,]}
rn=c("Fc 1", "Fc 2", "Fc 3", "Fc 4", "Fc 5", "Fc 6", "Fc 7", "Fc 8")
cn=c("Lepen", "Joly", "Bayrou", "Hollande", "Melenchon", "Dupont", "Sarkozy", "Poutou")
rownames(B)=rn
colnames(B)=cn
B
par(mfrow=c(1,1))
balloonPlot(B,TRUE,FALSE,"fan clusters vs post clusters",cn,rn,4,1)
#expected number of comments of fan in fg i under post in pg j
=======
rownames(nmatr)=c("po_c1","po_c2","po_c3","po_c4",
                  "po_c5","po_c6","po_c7","po_c8")
nmatr
balloonPlot(nmatr)
nomatr=nmatr[c(7,,4,),]
rowSums(nmatr)
colSums(nmatr)

kmucluster=km8u$cluster
fanid=1:92226
dfu=data.frame(kmucluster,fanid)
dfuo=dfu[order(dfu$kmucluster),]
fpmatr=matrix(0,ncol=8,nrow=8)
for (s in 1:8)
{
  for (t in 1:8)
  {
    pid=dfo[dfo$kmcluster==s,3]
    fid=dfuo[dfuo$kmucluster==t,2]
    fpmatr[s,t]=sum(allpostfans[fid,pid])
  }
}
#comments of fan group s in post group t

rownames(fpmatr)=c("fan_g1","fan_g2","fan_g3","fan_g4",
                   "fan_g5","fan_g6","fan_g7","fan_g8")
colnames(fpmatr)=c("po_g1","po_g2","po_g3","po_g4",
                   "po_g5","po_g6","po_g7","po_g8")
fpmatr
fpmatro=fpmatr[c(2,5,4,8,7,6,3,1),]
##################################################################


##################################################################

Y = model.matrix(~as.factor(cc)-1)
dim(Y)
yy = solve(t(Y)%*%Y)
a=allpostfans
dim(a)
ap = a%*%Y%*%yy
apn =t(apply(ap,1,function(x) return(x/sqrt(sum(x^2)))))

kmap = kmeans(apn, 8, nstart = 100)
table(kmap$clust)

Zhat = sparse.model.matrix(~as.factor(kmap$clust)-1)
zz = solve(t(Zhat)%*%Zhat)
bhat = zz%*%t(Zhat)%*%a%*%Y%*%yy

round(bhat*3239)

B1=round(bhat[c(4,7,6,5,1,8,3,2),]*3239)
B=matrix(rep(0,64),nrow=8)
for(i in 1:8)
{B[i,]=B1[i,]}
rownames(B)=c("Fg 1", "Fg 2", "Fg 3",
              "Fg 4", "Fg 5", "Fg 6",
              "Fg 7", "Fg 8")
colnames(B)=c("Pg 1", "Pg 2", "Pg 3",
              "Pg 4", "Pg 5", "Pg 6",
              "Pg 7", "Pg 8")
B
par(mfrow=c(1,1))
balloonPlot(B)
#expected number of comments in clusters
###################################################

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/strkm8.RData")

ns=rep(0,8); for (i in 1:8) {ns[i]=sum(n[1:i])}
ns0=c(0,ns)
bp=matrix(0,nrow=8,ncol=8)
# j candidate, i clusters
for(j in 1:8)
{for (i in 1:8)
{bp[i,j]=sum(km8$cluster[(ns0[j]+1):ns0[j+1]]==i)}
}
rownames(bp)=c("cluster1","cluster2","cluster3","cluster4",
               "cluster5","cluster6","cluster7","cluster8")
colnames(bp)=c("lepen","joly","bayrou","hollande",
               "melenchon","dupont","sarkozy","poutou")
par(mfrow=c(1,1))
balloonPlot(bp)
bp
library(cluster)
library(fpc)
par(mfrow=c(1,1))
clusplot(v, km8$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(v, km8$cluster)

dim(allpostfans)
nr=nrow(allpostfans)
poef=rep(0,nr)
dgef=rep(0,nr)
for(i in 1:nr)
{poef[i]=sum(allpostfans[i,]>0)
 dgef[i]=sum(allpostfans[i,])}
hist(poef,breaks=nr)
hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

nkm=8
km8_po=rep(0,nkm)
km8_fa=rep(0,nkm)
km8_dg=rep(0,nkm)
km8_faid=NULL
for(j in 1:nkm)
{
  km8_po[j]=sum(km8$cluster==j)
  km8_dg[j]=sum(allpostfans[,km8$cluster==j])
}

for(j in 1:nkm)
{
  for(i in 1:nr)
  {if(sum(allpostfans[i,km8$cluster==j])>0) 
  {km8_fa[j]=km8_fa[j]+1
   km8_faid=c(km8_faid,i)}
  }
}
plot(km8_po)
plot(km8_dg)
plot(km8_fa)
km8_po
km8_dg
km8_fa

par(mfrow=c(1,1))
plot(km8_faid)

km8_poef=matrix(0,nrow=nr,ncol=nkm)
km8_dgef=matrix(0,nrow=nr,ncol=nkm)

for(i in 1:nr)
{
  for(j in 1:nkm)
  {if(sum(allpostfans[i,km8$cluster==j])>0)
  {km8_poef[i,j]=km8_poef[i,j]+sum(allpostfans[i,km8$cluster==j]>0)
   km8_dgef[i,j]=km8_dgef[i,j]+sum(allpostfans[i,km8$cluster==j])
  }
  }
}

par(mfrow=c(nkm,2))
for(j in 1:nkm)
{hist(km8_poef[km8_poef[,j]>0,j],breaks=nr)
 hist(km8_dgef[km8_dgef[,j]>0,j],breaks=nr)
}

summary(km8_poef[km8_poef[,1]>0,1])
summary(km8_poef[km8_poef[,2]>0,2])
summary(km8_poef[km8_poef[,3]>0,3])
summary(km8_dgef[km8_dgef[,1]>0,1])
summary(km8_dgef[km8_dgef[,2]>0,2])
summary(km8_dgef[km8_dgef[,3]>0,3])

for(j in 1:nkm)
{hist(km8_dgef[km8_dgef[,j]>15,j],breaks=nr)
}

for (j in 1:nkm)
{plot(km8_poef[km8_poef[,j]>0,j])
 plot(km8_dgef[km8_dgef[,j]>0,j])}

save(poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrpoef.RData")
save(dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrdgef.RData")
save(km8_poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8poef.RData")
save(km8_dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8dgef.RData")
save(km8_po, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8po.RData")
save(km8_fa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8fa.RData")
save(km8_dg, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8dg.RData")
save(km8_faid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8faid.RData")


>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
