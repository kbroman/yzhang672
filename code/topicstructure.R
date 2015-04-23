
library(Matrix)
library(XML);library(RCurl);library(devtools);library(qdap)
library(stringr);library(tm);library(RWeka);library(wordcloud)

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords.RData")

sf <- svdmatrix(allfanwords)

#save(sf, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallfanwords.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallfanwords.RData")

par(mfrow=c(1,1))
plot(sf$d)
dim(sf$u)
dim(sf$v)
u = sf$u[,1:8]
u = t(apply(u,1,function(x) return(x/sqrt(sum(x^2)))))

par(mfrow = c(3,1), oma=rep(1,4))
for(i in 1:3) plot(u[,i], ylab = "", xlab="")

#########################################################

Y = model.matrix(~as.factor(ccdate)-1)
dim(Y)
yy = solve(t(Y)%*%Y)
a=allfansf
dim(a)
ap = a%*%Y%*%yy
apn =t(apply(ap,1,function(x) return(x/sqrt(sum(x^2)))))

nclus=8
kmap = kmeans(apn, nclus, nstart = 100)
table(kmap$clust)

Zhat = sparse.model.matrix(~as.factor(kmap$clust)-1)
zz = solve(t(Zhat)%*%Zhat)
bhat = zz%*%t(Zhat)%*%a%*%Y%*%yy

round(bhat*53)

B1=round(bhat*53)
B=matrix(rep(0,nclus*11),nrow=nclus)
for(i in 1:nclus)
{B[i,]=B1[i,]}

B
par(mfrow=c(1,1))
balloonPlot(B)


