load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdshpostfan.RData")
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cc.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/postdates.RData")

fsh = rownames(sh_postfans); fa = rownames(allpostfans)
fm = match(fa,fsh)
cn = which(fm>0)
ush=sh_sf$u[,4]
u=rep(0,length(fa))
u[cn]=ush

A = allpostfans

time = as.vector(u %*% A)

npost = length(A[1,]);ndate=length(unidate)

od = order(mdates)
timeo = time[od]
cco = cc[od]

#plot(time,col=cc)
plot(timeo,col=cco,xlab="plots by date",ylab="u[,4]A")
d1=sum(mdates<55)+1;d2=sum(mdates<87)+1;d3=sum(mdates<101)+1;
abline(v=d1,col="grey");abline(v=d2,col="grey");abline(v=d3,col="grey")
legend("bottomright",   c("lepen","joly","bayrou","hollande",
                    "melenchon","dupont","sarkozy","poutou"),
       col=1:8,pch=rep(16,8))

nsum=rep(0,9);for(i in 1:8) {nsum[i+1]=sum(n[1:i])}
#for (i in 1:8)
#{
#  ci = which(cco==i)
#  plot(timeo[ci],col=cco[ci],xlab="plots by date",ylab="u[,4]A",main="")
#  xi = mdates[(nsum[i]+1):nsum[i+1]]
#  d1=sum(xi<55)+1;d2=sum(xi<87)+1;d3=sum(xi<101)+1;
#  abline(v=d1,col="grey");abline(v=d2,col="grey");abline(v=d3,col="grey")
#}

i=1
ci = which(cco==i)
plot(timeo[ci],col=cco[ci],xlab="plots by date",ylab="u[,4]A",main="lepen",
     ylim=c(-17,2.5))
xi = mdates[(nsum[i]+1):nsum[i+1]]
d1=sum(xi<55)+1;d2=sum(xi<87)+1;d3=sum(xi<101)+1;
abline(v=d1,col="grey");abline(v=d2,col="grey");abline(v=d3,col="grey")

i=2
ci = which(cco==i)
plot(timeo[ci],col=cco[ci],xlab="plots by date",ylab="u[,4]A",main="joly",
     ylim=c(-17,2.5))
xi = mdates[(nsum[i]+1):nsum[i+1]]
d1=sum(xi<55)+1;d2=sum(xi<87)+1;d3=sum(xi<101)+1;
abline(v=d1,col="grey");abline(v=d2,col="grey");abline(v=d3,col="grey")

i=3
ci = which(cco==i)
plot(timeo[ci],col=cco[ci],xlab="plots by date",ylab="u[,4]A",main="bayrou",
     ylim=c(-17,2.5))
xi = mdates[(nsum[i]+1):nsum[i+1]]
d1=sum(xi<55)+1;d2=sum(xi<87)+1;d3=sum(xi<101)+1;
abline(v=d1,col="grey");abline(v=d2,col="grey");abline(v=d3,col="grey")

i=4
ci = which(cco==i)
plot(timeo[ci],col=cco[ci],xlab="plots by date",ylab="u[,4]A",main="hollande",
     ylim=c(-17,2.5))
xi = mdates[(nsum[i]+1):nsum[i+1]]
d1=sum(xi<55)+1;d2=sum(xi<87)+1;d3=sum(xi<101)+1;
abline(v=d1,col="grey");abline(v=d2,col="grey");abline(v=d3,col="grey")

i=5
ci = which(cco==i)
plot(timeo[ci],col=cco[ci],xlab="plots by date",ylab="u[,4]A",main="melenchon",
     ylim=c(-17,2.5))
xi = mdates[(nsum[i]+1):nsum[i+1]]
d1=sum(xi<55)+1;d2=sum(xi<87)+1;d3=sum(xi<101)+1;
abline(v=d1,col="grey");abline(v=d2,col="grey");abline(v=d3,col="grey")


i=6
ci = which(cco==i)
plot(timeo[ci],col=cco[ci],xlab="plots by date",ylab="u[,4]A",main="dupont",
     ylim=c(-17,2.5))
xi = mdates[(nsum[i]+1):nsum[i+1]]
d1=sum(xi<55)+1;d2=sum(xi<87)+1;d3=sum(xi<101)+1;
abline(v=d1,col="grey");abline(v=d2,col="grey");abline(v=d3,col="grey")

i=7
ci = which(cco==i)
plot(timeo[ci],col=cco[ci],xlab="plots by date",ylab="u[,4]A",main="sarkozy",
     ylim=c(-17,2.5))
xi = mdates[(nsum[i]+1):nsum[i+1]]
d1=sum(xi<55)+1;d2=sum(xi<87)+1;d3=sum(xi<101)+1;
abline(v=d1,col="grey");abline(v=d2,col="grey");abline(v=d3,col="grey")

i=8
ci = which(cco==i)
plot(timeo[ci],col=cco[ci],xlab="plots by date",ylab="u[,4]A",main="poutou",
     ylim=c(-17,2.5))
xi = mdates[(nsum[i]+1):nsum[i+1]]
d1=sum(xi<55)+1;d2=sum(xi<87)+1;d3=sum(xi<101)+1;
abline(v=d1,col="grey");abline(v=d2,col="grey");abline(v=d3,col="grey")

