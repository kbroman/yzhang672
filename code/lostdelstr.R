load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/lostdf.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/nlost.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm5.RData")
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')

ncluster=5
canid=c(rep("lepen",n[1]),rep("joly",n[2]),rep("bayrou",n[3]),
        rep("hollande",n[4]),rep("melenchon",n[5]),
        rep("dupont",n[6]),rep("sarkozy",n[7]),rep("poutou",n[8]))
postid=colnames(allpostfans)
lostid=as.vector(lostdf[,2])
clost=match(postid,lostid)

kmcluster=km5$cluster

npost=rep(0,ncluster)
for (i in 1:ncluster)
{
  npost[i]=sum(kmcluster==i)
}

nsp=rep(0,(ncluster+1))
for (i in 1:ncluster)
{
  nsp[i+1]=sum(npost[1:i])
}

delpost=matrix(0,nrow=3239,ncol=4)

for (i in 1:ncluster)
{
 delpost[(nsp[i]+1):nsp[i+1],1]=rep(i,npost[i])
 delpost[(nsp[i]+1):nsp[i+1],2]=canid[kmcluster==i]
 delpost[(nsp[i]+1):nsp[i+1],3]=postid[kmcluster==i]
 for (j in 1:npost[i])
 {
   cm=(is.na(match(delpost[(nsp[i]+j),3],lostid)))*1
   # cm=1 with text # cm=0 without text
   delpost[(nsp[i]+j),4]=cm
 }
}

colnames(delpost)=c("clusterid","candidate","post url","with(1)/without(0) text")

nlost_c=rep(0,ncluster)
nwith_c=rep(0,ncluster)

for (i in 1:ncluster)
{nlost_c[i]=sum(delpost[(nsp[i]+1):nsp[i+1],4]=="0")
 nwith_c[i]=sum(delpost[(nsp[i]+1):nsp[i+1],4]=="1")}

clusid=c("clus 1","clus 2","clus 3","clus 4","clus 5")

npost_c=cbind(clusid, nlost_c, nwith_c,npost)
npost_c
