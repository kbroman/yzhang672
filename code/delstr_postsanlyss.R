load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allposts.RData")
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delposts.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/del_clus.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delclusalsort.RData")
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')


drop2 = function(tmp){
  return(substr(tmp, start = 21, stop = nchar(tmp)))  
}

postid = colnames(allpostfans)
postur = allposts$posturl
sum(drop2(postid)==postur)

canid=c(rep("lepen",n[1]),rep("joly",n[2]),rep("bayrou",n[3]),
        rep("hollande",n[4]),rep("melenchon",n[5]),
        rep("dupont",n[6]),rep("sarkozy",n[7]),rep("poutou",n[8]))

delpost = data.frame(del_posts[,1], del_posts[,2], del_posts[,3], canid,postid,allposts$posttext,allposts$postdate)
delpost[,7]=as.vector(delpost[,7])

colnames(delpost)= c("cluster_order","post_order","post_order_within_cluster","candidate","posturl","posttext","postdate")

delposto =  delpost[order(delpost[,1],delpost[,3]),]

del_clus
write.csv(delposto, file = "delposts.csv",row.names=FALSE)
plot(del_clus$average_leverage,xlab="cluster id",ylab="average_leverage")
save(delposto, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delposto.RData")


######################################################################################

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delposto.RData")

alldates = delposto$postdate
unidate = as.vector(sort(unique(alldates)))
mdates = match(alldates,unidate)

no = which(is.na(mdates))
no

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostdatetrue.RData")

nclus=5;ndatec=51;timeB=matrix(0,ncol=ndatec,nrow=nclus)
dateg=rep(0,ndatec+1);for (j in 1:ndatec) {dateg[j+1]=2*j}
for (i in 1:nclus)
 { 
  for (j in 1:ndatec)
  {
    ci=which(delposto[,1]==i)
    xi=mdates[ci]
    timeB[i,j]=sum((xi>dateg[j]) & (xi<dateg[j+1]))
  }
}

B = cbind(rowSums(timeB[,1:18]),timeB[,19:51])

B

nr = c("cluster 1","cluster 2","cluster 3","cluster 4","cluster 5")
nc = c("","","","","","","","","","","","","","","","","","","","",
       "","","","","","","","","","","","","","");
nc[11]="3-20";nc[27]="4-22";nc[34]="5-06"
balloonPlot(B,TRUE,FALSE,"time structure",nc,nr,4,1)

unidate[c((18:50)*2,102)]
length(unidate[c((18:50)*2,102)])

unidate
