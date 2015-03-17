###########################################
#createallfansdate


library(Matrix)
load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')
lepe = tables$lepen
joly = tables$joly
bayr = tables$bayrou
holl = tables$hollande
mele = tables$melenchon
dupo = tables$dupont
sark = tables$sarkozy
pout = tables$poutou

allfans=c(lepe$fan_id,joly$fan_id,bayr$fan_id,holl$fan_id,
          mele$fan_id,dupo$fan_id,sark$fan_id,pout$fan_id)

alluniquefan = unique(allfans)

n=length(allfans)
nf=length(alluniquefan)

cfans=match(allfans,alluniquefan)

timeall=c(lepe$utc_time, joly$utc_time, bayr$utc_time,
          holl$utc_time, mele$utc_time, dupo$utc_time,
          sark$utc_time, pout$utc_time)

drop = function(tmp){
  return(substr(tmp, 1, 10))  
}
dateall = lapply(timeall, drop)
alluniquedate=unique(dateall)
dateo=order(as.character(alluniquedate))
date=as.character(alluniquedate[dateo])

cfans=match(allfans,alluniquefan)
cdate=match(dateall,date)


#allcrA = spMatrix(nrow =n , ncol = length(alluniquepost),
#                  i = 1:n,
#                  j = cpost, 
#                  x = rep(1, n))

allfansdate = spMatrix(nrow =nf , ncol = length(alluniquedate),
                       i = cfans,
                       j = cdate, 
                       x = rep(1, n))
#dim(allcrA)
dim(allfansdate)


colnames(allfansdate) = date

save(allfansdate, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfansdate.RData")



######################################################
# right singular vectors
ccdate=NULL
for(i in 1:10){ccdate=c(ccdate,rep(i,5))};ccdate=c(ccdate,rep(11,3))

a=allfansdate

Y = model.matrix(~as.factor(ccdate)-1)
yy = solve(t(Y)%*%Y)
hc=Y%*%yy%*%t(Y)
I=diag(1,length(ccdate))

nc = ncol(a)
cs = colSums(a)
tau = cs/nc
Dcol = Diagonal(n = nc, x = 1/(cs + tau))

Lb=Dcol%*%(I-hc)
L=a%*%Lb

library(irlba)
sdate = irlba(L, nu = 20, nv = 20)
save(sdate, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svddatestructure.RData")

################################################

par(mfrow=c(1,1))
plot(sdate$d)
dim(sdate$u)
dim(sdate$v)
u = sdate$u[,1:8]
u = t(apply(u,1,function(x) return(x/sqrt(sum(x^2)))))

#########################################################

Y = model.matrix(~as.factor(ccdate)-1)
dim(Y)
yy = solve(t(Y)%*%Y)
a=allfansdate
dim(a)
ap = a%*%Y%*%yy
apn =t(apply(ap,1,function(x) return(x/sqrt(sum(x^2)))))

nclus=3
kmap = kmeans(apn, nclus, nstart = 1000)
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
#################################################################

sh_fans=c(holl$fan_id,sark$fan_id)
sh_uniquefan = unique(sh_fans)

n=length(sh_fans)
nf=length(sh_uniquefan)

cfans=match(sh_fans,sh_uniquefan)

sh_time=c(holl$utc_time, sark$utc_time)

drop = function(tmp){
  return(substr(tmp, 1, 10))  
}
sh_date = lapply(sh_time, drop)
sh_uniquedate=unique(sh_date)
sh_dateo=order(as.character(sh_uniquedate))
sh_uniquedate=as.character(sh_uniquedate[sh_dateo])

cfans=match(sh_fans,sh_uniquefan)
cdate=match(sh_date,sh_uniquedate)


#allcrA = spMatrix(nrow =n , ncol = length(alluniquepost),
#                  i = 1:n,
#                  j = cpost, 
#                  x = rep(1, n))

sh_fansdate = spMatrix(nrow =nf , ncol = length(sh_uniquedate),
                       i = cfans,
                       j = cdate, 
                       x = rep(1, n))
#dim(allcrA)
dim(sh_fansdate)


colnames(sh_fansdate) = sh_uniquedate

save(sh_fansdate, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/sh_fansdate.RData")

#################################################################################3

# right singular vectors
ccshdate=NULL
for(i in 1:7){ccshdate=c(ccshdate,rep(i,5))};
ccshdate=c(ccshdate,rep(8,4))
for(i in 9:10){ccshdate=c(ccshdate,rep(i,5))};
ccshdate=c(ccshdate,rep(11,2))

a=sh_fansdate

Y = model.matrix(~as.factor(ccshdate)-1)
yy = solve(t(Y)%*%Y)
hc=Y%*%yy%*%t(Y)
I=diag(1,length(ccshdate))

nc = ncol(a)
cs = colSums(a)
tau = cs/nc
Dcol = Diagonal(n = nc, x = 1/(cs + tau))

Lb=Dcol%*%(I-hc)
L=a%*%Lb

library(irlba)
sshdate = irlba(L, nu = 20, nv = 20)
save(sshdate, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdshdatestructure.RData")

################################################

par(mfrow=c(1,1))
plot(sshdate$d)
dim(sshdate$u)
dim(sshdate$v)
u = sshdate$u[,1:3]
u = t(apply(u,1,function(x) return(x/sqrt(sum(x^2)))))

#########################################################

Y = model.matrix(~as.factor(ccshdate)-1)
dim(Y)
yy = solve(t(Y)%*%Y)
dim(a)
ap = a%*%Y%*%yy
apn =t(apply(ap,1,function(x) return(x/sqrt(sum(x^2)))))

nclus=3
kmap = kmeans(apn, nclus, nstart = 1000)
table(kmap$clust)

Zhat = sparse.model.matrix(~as.factor(kmap$clust)-1)
zz = solve(t(Zhat)%*%Zhat)
bhat = zz%*%t(Zhat)%*%a%*%Y%*%yy

round(bhat*53)

B1=round(bhat[c(3,2,1),]*53)
B=matrix(rep(0,nclus*11),nrow=nclus)
for(i in 1:nclus)
{B[i,]=B1[i,]}

B
par(mfrow=c(1,1))
balloonPlot(B)

