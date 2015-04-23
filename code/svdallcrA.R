rm(list = ls())

library(Matrix)

load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcrA.RData')

rs=rowSums(allcrA)
cs=colSums(allcrA)
summary(rs); summary(cs)
L = Diagonal(length(rs), 1/sqrt(rs + mean(rs)))%*%
    allcrA%*%Diagonal(length(cs), 1/sqrt(cs + mean(cs)))

%rankMatrix(allcrA,method="qrLINPACK")

library(irlba)
s=irlba(L,nu=20)
sd=s$d[1:20]

sall=irlba(allcrA,nu=20)
salld=sall$d[1:20]

ssark=irlba(crA,nu=20)

save(salld, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdvalueallcrA.RData")
save(sd, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdvalueallcrAnorm.RData")

n=rep(0,8)
n[1]=nrow(lepe);n[2]=nrow(joly);n[3]=nrow(bayr);n[4]=nrow(holl);
n[5]=nrow(mele);n[6]=nrow(dupo);n[7]=nrow(sark);n[8]=nrow(pout)
nc=ncol(allcrA);nr=nrow(allcrA)
psum=rep(0,8);asum=matrix(rep(0,8*nc),ncol=nc);x=rep(0,nr)
dropc=NULL
for (j in 1:nc)
{
x=allcrA[,j]

psum[1]=sum(x[1:n[1]])
for (c in 2:8)
{psum[c]=sum(x[n[c-1]:n[c]])}

for (i in 1:8)
{asum[i,j]=sum(psum)-psum[i]}
if (min(asum[,j])==0)
{dropc=c(dropc,j)}
}

crDropall = allcrA[,-dropc]
cs = colSums(crDropall)
LDropall = Diagonal(length(rs), 1/sqrt(rs + mean(rs)))%*%
    crDropall%*%Diagonal(length(cs), 1/sqrt(cs + mean(cs)))

sdrop=irlba(LDropall,nu=20)
sdropd=sdrop$d[1:20]
sdropall=irlba(crDropall,nu=20)
sdropalld=sdropall$d[1:20]

save(sdropalld, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallcrAdrop.RData")
save(sdropd, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdallcrAdropnorm.RData")






