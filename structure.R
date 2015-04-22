rm(list = ls())

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallpostfannorm.RData")
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cc.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/postdates.RData")
#alldates,unidate,mdates

ncluster=8;ndate=length(unidate);npost=length(allpostfans[1,])

par(mfrow=c(1,1))
plot(sf$d,ylab="singular values")
v = sf$v[,1:ncluster]
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))

svbd = sv_bydate(v, mdates, cc, npost, ncluster)
vd = svbd$vd; vdl = svbd$vdl; dfvdl = svbd$dfvdl; daten = svbd$daten

par(mfrow = c(4,1), oma=rep(1,4))
for(i in 1:4) plot(vd[,i], ylab = "", xlab="", col = cc)
for(i in 5:8) plot(vd[,i], ylab = "", xlab="", col = cc)

for(i in 1:4) plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,(ncluster+2)])
for(d in 1:ndate) abline(v=daten[d],col="grey")
abline(v=daten[55]);abline(v=daten[87]);abline(v=daten[101]);
for(i in 5:8) plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,(ncluster+2)])
for(d in 1:ndate) abline(v=daten[d],col="grey")
abline(v=daten[55]);abline(v=daten[87]);abline(v=daten[101]);

km8 = kmeans(v, centers = ncluster, nstart = 1000)
#save(km8, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/strkm8.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/strkm8.RData")

par(mfrow=c(1,1))
library(fpc)
plotcluster(v, km8$cluster)

id=1:npost
kmcluster=km8$cluster
df=data.frame(kmcluster,cc,id)
dfo=df[order(df$kmcluster),]
ndfo=rep(0,ncluster)
for (i in 1:ncluster){ndfo[i]=sum(kmcluster==i)}
nmatr=matrix(0,ncol=ncluster,nrow=ncluster)
for (i in 1:8)
{
  for (j in 1:8)
  {
    nmatr[i,j]=sum(df$kmcluster==i & df$cc==j)
  }
}
#posts in clusters
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
