rm(list = ls())


load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cc.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')

<<<<<<< HEAD
A=allpostfans

Y = model.matrix(~as.factor(cc)-1)
yy = solve(t(Y)%*%Y)
hc = Y%*%yy%*%t(Y)
#I = diag(1,length(cc))
Ahat=A%*%hc
B=A-Ahat

A1=A[1:50000,];A2=A[50001:92226,];#A3=A[60001:92226,];
Ahat1 = as.big.matrix(A1%*%hc); Ahat2 = A2%*%hc; #Ahat3 = A3%*%hc;

Ahat = spMatrix(nrow=92226,ncol=3239,i=c(Ahat1@i+1,Ahat2@i+50001),
                j=c(Ahat1@j+1,Ahat2@j+1),x=c(Ahat1@x,Ahat2@x))
B = A-Ahat


=======
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
#a=allpostfans

#Y = model.matrix(~as.factor(cc)-1)
#yy = solve(t(Y)%*%Y)
#hc=Y%*%yy%*%t(Y)
#I=diag(1,length(cc))

#nc = ncol(a)
#cs = colSums(a)
#tau = cs/nc
#Dcol = Diagonal(n = nc, x = 1/(cs + tau))

#Lb=Dcol%*%(I-hc)
#L=a%*%Lb

#library(irlba)
#sdelete = irlba(L, nu = 20, nv = 20)

#save(sdelete, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svddeletestructure.RData")

<<<<<<< HEAD
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cc.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cc.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svddeletestructure.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/postdates.RData")

#sdelete

ncluster=5;ndate=length(unidate);npost=length(allpostfans[1,])

par(mfrow=c(1,1))
plot(sdelete$d,ylab="singular values",main="svd after deleting cadidate structure")
v = sdelete$v[,1:ncluster]
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))

svbd = sv_bydate(v, mdates, cc, npost, ncluster)
vd = svbd$vd; vdl = svbd$vdl; dfvdl = svbd$dfvdl; daten = svbd$daten
=======
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cc.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svddeletestructure.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostdate.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostdatetrue.RData")

#sdelete

par(mfrow=c(1,1))
plot(sdelete$d)
dim(sdelete$u)
dim(sdelete$v)
v = sdelete$v[,1:5]
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))

u = sdelete$u[ ,1:5]
u = t(apply(u,1,function(x) return(x/sqrt(sum(x^2)))))
km5u = kmeans(u, centers = 5, nstart = 1000)



id=1:3239
dfvd=data.frame(v,datem,cc,id)
dfvdo=dfvd[order(dfvd$cc,dfvd$datem),]
dfvdo=as.matrix(dfvdo)
vd=dfvdo[,1:5]

dfvdl=dfvd[order(dfvd$datem),]
dfvdl=as.matrix(dfvdl)
vdl=dfvdl[,1:5]

datem_od=dfvdl[,6]
daten=rep(1,53)
for(i in 2:53){daten[i]=sum(datem_od<=(i-1))+1}

>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a

par(mfrow = c(3,1), mar = rep(1,4))
par(xpd=TRUE)
i=1; plot(vd[,i], ylab = "", xlab="", col = cc)
legend(3100,0.5,c("lepen","joly","bayrou","hollande",
                    "melenchon","dupont","sarkozy","poutou"),
       col=1:8,pch=rep(16,8))
for(i in 2:3) plot(vd[,i], ylab = "", xlab="", col = cc)
for(i in 4:5) plot(vd[,i], ylab = "", xlab="", col = cc)

par(mfrow = c(3,1), mar = rep(1,4))
i=1; plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,7])
legend(3100,0.5,c("lepen","joly","bayrou","hollande",
                  "melenchon","dupont","sarkozy","poutou"),
       col=1:8,pch=rep(16,8))
for(i in 2:3) plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,7])
for(i in 4:5) plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,7])
par(xpd=FALSE)
i=5;plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,7])
legend("right",   c("lepen","joly","bayrou","hollande",
                  "melenchon","dupont","sarkozy","poutou"),
       col=1:8,pch=rep(16,8))
<<<<<<< HEAD
for(d in 1:102) abline(v=daten[d],col="grey")
abline(v=daten[55]);abline(v=daten[87]);abline(v=daten[101]);
########################################################


=======
for(d in 1:53) abline(v=daten[d])

#npd=rep(0,max(datem))
#for (iday in 1: max(datem))
#{npd[iday] = length(dfvdl[which(dfvdl[,6]==iday),7])}
#par(mfrow=c(1,1))
#plot(npd)
#npd
#day 49,50,51,52,53 melenchon many posts
#date[c(49,50,51,52,53)]
#length(datem)



########################################################
load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')

>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
#km5 = kmeans(v, centers = 5, nstart = 1000)
#save(km5, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm5.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm5.RData")

kmcluster=km5$cluster

<<<<<<< HEAD
vv=solve(t(v)%*%v)
Hv=v%*%vv%*%t(v)
Hii=diag(Hv)
Hiio=sort(Hii,decreasing=TRUE)
Hsort=match(Hii,Hiio)

als=rep(0,ncluster)
for (i in 1:ncluster)
{
  ci=which(kmcluster==i)
  als[i]=mean(Hii[ci])
}

also=sort(als,decreasing=TRUE)
alsort=match(als,also)

clusort=rep(0,npost);clusn=rep(0,ncluster);postoc=rep(0,npost)
for (i in 1:ncluster)
{
  ci=which(kmcluster==i)
  clusort[ci]=rep(alsort[i],length(ci))
  
  clusn[i]=length(which(clusort==i))
  
  #post order within each cluster
  pi=Hii[ci]
  pio=sort(pi,decreasing=TRUE)
  postoc[ci]=match(pi,pio)
}

order=1:5
del_clus=data.frame(order,clusn,also)
colnames(del_clus)=c("order","#posts", "average_leverage")

del_posts=data.frame(clusort,Hsort,postoc)
colnames(del_posts)=c("cluster order","post order","post within cluster order")

save(del_posts, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delposts.RData")
save(del_clus, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/del_clus.RData")
save(alsort, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delclusalsort.RData")

######################################################################
load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')

=======
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
library(XML);library(RCurl);library(devtools);library(qdap)
library(stringr);library(tm);library(RWeka);library(wordcloud)

lepe = tables$lepen;    joly = tables$joly
bayr = tables$bayrou;   holl = tables$hollande
mele = tables$melenchon;dupo = tables$dupont
sark = tables$sarkozy;  pout = tables$poutou

<<<<<<< HEAD
=======
drop2 = function(tmp){
  return(substr(tmp, start = 21, stop = nchar(tmp)))  
}
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
lepe_p = as.character(lapply(lepe$parent_url,drop2))
joly_p = as.character(lapply(joly$parent_url,drop2))
bayr_p = as.character(lapply(bayr$parent_url,drop2))
holl_p = as.character(lapply(holl$parent_url,drop2))
mele_p = as.character(lapply(mele$parent_url,drop2))
dupo_p = as.character(lapply(dupo$parent_url,drop2))
sark_p = as.character(lapply(sark$parent_url,drop2))
pout_p = as.character(lapply(pout$parent_url,drop2))

lepe_c=lepe$text;joly_c=joly$text;bayr_c=bayr$text;holl_c=holl$text
mele_c=mele$text;dupo_c=dupo$text;sark_c=sark$text;pout_c=pout$text

all_p = c(lepe_p,joly_p,bayr_p,holl_p,mele_p,dupo_p,sark_p,pout_p)
all_c = c(lepe_c,joly_c,bayr_c,holl_c,mele_c,dupo_c,sark_c,pout_c)

rm(tables,
   lepe_p,joly_p,bayr_p,holl_p,mele_p,dupo_p,sark_p,pout_p,
   lepe_c,joly_c,bayr_c,holl_c,mele_c,dupo_c,sark_c,pout_c,
   lepe,joly,bayr,holl,mele,dupo,sark,pout)

myStopwords <- c(stopwords("French"),
                 "http","www","facebook","com","le")

 Freqterm=NULL;Freqcount=NULL;#Nfc=rep(0,5)
for(i in 1:5)
{
  
  ci=colnames(allpostfans)[kmcluster==i]
  
  ami=match(all_p,ci)
  ai=ami>0
  api=all_p[ai]
  api=api[!is.na(api)]
  aci=all_c[ai]
  aci=aci[!is.na(aci)]
  
  co_clusi=aci
  
  Nfc[i]=length(co_clusi)

  
<<<<<<< HEAD
  #co_clusi=str_replace_all(co_clusi, "[^[:alnum:]]", " ")
=======
  co_clusi=str_replace_all(co_clusi, "[^[:alnum:]]", " ")
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
  co_clusi=iconv(co_clusi, "latin1", "ASCII", sub="")
  #a=bracketX(co_clusi)
  
  myCorpus <- Corpus(VectorSource(co_clusi))
  
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  
  mytdm=TermDocumentMatrix(myCorpus)
  freqterm=findFreqTerms(mytdm, lowfreq = (length(co_clusi)/50), highfreq = Inf)
  freqc=mytdm[freqterm,]
  freqm=spMatrix(nrow=freqc$nrow,ncol=freqc$ncol,i=freqc$i,j=freqc$j,x=freqc$v)
  freqcount=rowSums(freqm)
  
  freq=data.frame(freqterm,freqcount)
  freqdf=freq[order(freq$freqcount,decreasing=TRUE),]
  
  freqterm=freqdf$freqterm
  freqcount=freqdf$freqcount
  
  Freqterm=list(Freqterm,freqterm)
  Freqcount=list(Freqcount,freqcount)
}
  
  #save(F1, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delF1.RData")
  #save(F2, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delF2.RData")
  #save(F3, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delF3.RData")
  #save(F4, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delF4.RData")
  #save(F5, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delF5.RData")
  
  load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delF1.RData")
  load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delF2.RData")
  load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delF3.RData")
  load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delF4.RData")
  load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delF5.RData")
   

Freqterm=list(F1$freqterm,F2$freqterm,F3$freqterm,F4$freqterm,F5$freqterm)
Freqcount=list(F1$freqcount,F2$freqcount,F3$freqcount,F4$freqcount,F5$freqcount)

#save(Freqterm, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFreqterm.RData")
#save(Freqcount, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFreqcount.RData")
#save(Nfc, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delNfc.RData")

###########################################################

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFreqterm.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFreqcount.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delNfc.RData")

nfq=c(121,120,129,127,105)
Fqt=unlist(Freqterm)
Fqt=as.character(Fqt)

Fqt1=F1$freqterm;Fqt2=F2$freqterm;Fqt3=F3$freqterm;
Fqt4=F4$freqterm;Fqt5=F5$freqterm

Fqc1=F1$freqcount;Fqc2=F2$freqcount;Fqc3=F3$freqcount;
Fqc4=F4$freqcount;Fqc5=F5$freqcount

Fqtu=unique(Fqt)
m1=match(Fqtu,Fqt1);m2=match(Fqtu,Fqt2);
m3=match(Fqtu,Fqt3);m4=match(Fqtu,Fqt4);m5=match(Fqtu,Fqt5)
mq1=match(Fqt1,Fqtu);mq2=match(Fqt2,Fqtu);
mq3=match(Fqt3,Fqtu);mq4=match(Fqt4,Fqtu);mq5=match(Fqt5,Fqtu)
Fqca=rep(0,length(Fqtu))
Fqca[mq1]=Fqc1
Fqca[mq2]=Fqca[mq2]+Fqc2
Fqca[mq3]=Fqca[mq3]+Fqc3
Fqca[mq4]=Fqca[mq4]+Fqc4
Fqca[mq5]=Fqca[mq5]+Fqc5
Fqa=data.frame(Fqtu,Fqca)

Fq1=data.frame(Fqt1,Fqc1,Fqca[mq1])
Fq2=data.frame(Fqt2,Fqc2,Fqca[mq2])
Fq3=data.frame(Fqt3,Fqc3,Fqca[mq3])
Fq4=data.frame(Fqt4,Fqc4,Fqca[mq4])
Fq5=data.frame(Fqt5,Fqc5,Fqca[mq5])
Fq1;Fq2;Fq3;Fq4;Fq5

common=which(m1>0 & m2>0 & m3>0 & m4>0 & m5>0)
Fqt_c=Fqtu[common]
Fqt_d=Fqtu[-common]
Fqt_d1=Fqt_d[which(match(Fqt_d,Fqt1)>0)]
Fqt_d2=Fqt_d[which(match(Fqt_d,Fqt2)>0)]
Fqt_d3=Fqt_d[which(match(Fqt_d,Fqt3)>0)]
Fqt_d4=Fqt_d[which(match(Fqt_d,Fqt4)>0)]
Fqt_d5=Fqt_d[which(match(Fqt_d,Fqt5)>0)]

md1=match(Fqt_d1,Fqt1)
Fqc_d1=Fqc1[md1]
Fq_d1=data.frame(Fqt_d1,Fqc_d1)
Fq_d1=Fq_d1[order(Fq_d1$Fqc_d1,decreasing=TRUE),]

md2=match(Fqt_d2,Fqt2)
Fqc_d2=Fqc2[md2]
Fq_d2=data.frame(Fqt_d2,Fqc_d2)
Fq_d2=Fq_d2[order(Fq_d2$Fqc_d2,decreasing=TRUE),]

md3=match(Fqt_d3,Fqt3)
Fqc_d3=Fqc3[md3]
Fq_d3=data.frame(Fqt_d3,Fqc_d3)
Fq_d3=Fq_d3[order(Fq_d3$Fqc_d3,decreasing=TRUE),]

md4=match(Fqt_d4,Fqt4)
Fqc_d4=Fqc4[md4]
Fq_d4=data.frame(Fqt_d4,Fqc_d4)
Fq_d4=Fq_d4[order(Fq_d4$Fqc_d4,decreasing=TRUE),]

md5=match(Fqt_d5,Fqt5)
Fqc_d5=Fqc5[md5]
Fq_d5=data.frame(Fqt_d5,Fqc_d5)
Fq_d5=Fq_d5[order(Fq_d5$Fqc_d5,decreasing=TRUE),]

#save(Fq1, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFq1.RData")
#save(Fq2, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFq2.RData")
#save(Fq3, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFq3.RData")
#save(Fq4, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFq4.RData")
#save(Fq5, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFq5.RData")
#save(Fqa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqa.RData")
#save(Fq_d1, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqd1.RData")
#save(Fq_d2, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqd2.RData")
#save(Fq_d3, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqd3.RData")
#save(Fq_d4, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqd4.RData")
#save(Fq_d5, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqd5.RData")
#save(Fqt_c, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqtc.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqa.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFq1.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFq2.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFq3.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFq4.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFq5.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqd1.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqd2.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqd3.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqd4.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqd5.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delFqtc.RData")



########################################################
<<<<<<< HEAD
id=1:npost
=======

>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
df=data.frame(kmcluster,cc,id)
dfo=df[order(df$kmcluster),]
ndfo=rep(0,5)
for (i in 1:5){ndfo[i]=sum(kmcluster==i)}
nmatr=matrix(0,ncol=8,nrow=5)
for (i in 1:5)
{
  for (j in 1:8)
  {
    nmatr[i,j]=sum(df$kmcluster==i & df$cc==j)
  }
}
#posts in clusters
<<<<<<< HEAD
rn=c("poclus1","poclus2","poclus3","poclus4",
     "poclus5")
cn=c("Lepen", "Joly", "Bayrou", "Hollande", "Melenchon", "Dupont", "Sarkozy", "Poutou")
rownames(nmatr)=rn
colnames(nmatr)=cn

nmatr[alsort,]
rowSums(nmatr)
colSums(nmatr)
par(mfrow=c(1,1))
balloonPlot(nmatr[alsort,],TRUE, FALSE, "number of posts", cn, rn, 4,1)
=======
rownames(nmatr)=c("po_c1","po_c2","po_c3","po_c4",
                  "po_c5")
nmatr
rowSums(nmatr)
colSums(nmatr)
par(mfrow=c(1,1))
balloonPlot(nmatr)
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a

kmucluster=km5u$cluster
fanid=1:92226
dfu=data.frame(kmucluster,fanid)
dfuo=dfu[order(dfu$kmucluster),]
fpmatr=matrix(0,ncol=5,nrow=5)
for (s in 1:5)
{
  for (t in 1:5)
  {
    pid=dfo[dfo$kmcluster==s,3]
    fid=dfuo[dfuo$kmucluster==t,2]
    fpmatr[s,t]=sum(allpostfans[fid,pid])
  }
}
#comments of fan group s in post group t

fpmatr
#########################################################################

par(mfrow = c(1,1))
library(fpc)
plotcluster(v, km5$cluster)


#choose km5

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm5.RData")
<<<<<<< HEAD
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delclusalsort.RData")
=======
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a

nclus=5;ncan=8
ns=rep(0,8); for (i in 1:8) {ns[i]=sum(n[1:i])}
ns0=c(0,ns)
bp=matrix(0,nrow=nclus,ncol=ncan)
# j candidate, i clusters
for(j in 1:ncan)
{for (i in 1:nclus)
{bp[i,j]=sum(km5$cluster[(ns0[j]+1):ns0[j+1]]==i)}
}
rownames(bp)=c("cluster1","cluster2","cluster3","cluster4","cluster5")
colnames(bp)=c("lepen","joly","bayrou","hollande",
               "melenchon","dupont","sarkozy","poutou")
par(mfrow=c(1,1))
<<<<<<< HEAD
balloonPlot(bp[alsort,],TRUE,FALSE,"",)
=======
balloonPlot(bp)
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
bp
####################################################################



nkm=4
km4_poid=NULL;
for (i in 1:nkm) {km4_poid=c(km4_poid,which(km4$cluster==i))}

ncc=rep(0,8)
for(i in 1:8) {ncc[i]=sum(n[1:i])}
ncc=c(0,ncc)
pcc=rep(0,length(km4_poid))
for(i in 1:length(km4_poid))
{
  for(j in 1:8)
  {
    if(ncc[j]<km4_poid[i] && km4_poid[i]<ncc[j+1]+1)
    {pcc[i]=j}
  }
}
par(mfrow=c(1,1))
plot(km4_poid,col=pcc)


#####################################################################



dim(allpostfans)
nr=nrow(allpostfans)
poef=rep(0,nr)
dgef=rep(0,nr)
for(i in 1:nr)
{poef[i]=sum(allpostfans[i,]>0)
 dgef[i]=sum(allpostfans[i,])}
#hist(poef,breaks=nr)
#hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

nkm=3

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData")

km3_po=rep(0,nkm)
km3_fa=rep(0,nkm)
km3_dg=rep(0,nkm)
km3_faid=NULL
for(j in 1:nkm)
{
  km3_po[j]=sum(km3$cluster==j)
  km3_dg[j]=sum(allpostfans[,km3$cluster==j])
}

for(j in 1:nkm)
{
  for(i in 1:nr)
  {if(sum(allpostfans[i,km3$cluster==j])>0) 
  {km3_fa[j]=km3_fa[j]+1
   km3_faid=c(km3_faid,i)}
  }
}
#plot(km3_po)
#plot(km3_dg)
#plot(km3_fa)
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
  {if(sum(allpostfans[i,km3$cluster==j])>0)
  {km3_poef[i,j]=km3_poef[i,j]+sum(allpostfans[i,km3$cluster==j]>0)
   km3_dgef[i,j]=km3_dgef[i,j]+sum(allpostfans[i,km3$cluster==j])
  }
  }
}

#par(mfrow=c(nkm,2))
#for(j in 1:nkm)
#{hist(km3_poef[km3_poef[,j]>0,j],breaks=nr)
# hist(km3_dgef[km3_dgef[,j]>0,j],breaks=nr)
#}

summary(km3_poef[km3_poef[,1]>0,1])
summary(km3_poef[km3_poef[,2]>0,2])
summary(km3_poef[km3_poef[,3]>0,3])
summary(km3_dgef[km3_dgef[,1]>0,1])
summary(km3_dgef[km3_dgef[,2]>0,2])
summary(km3_dgef[km3_dgef[,3]>0,3])

#for(j in 1:nkm)
#{hist(km3_dgef[km3_dgef[,j]>15,j],breaks=nr)
#}

#for (j in 1:nkm)
#{plot(km3_poef[km3_poef[,j]>0,j])
# plot(km3_dgef[km3_dgef[,j]>0,j])}

#save(poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrpoef.RData")
#save(dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrdgef.RData")
#save(km3_poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3poef.RData")
#save(km3_dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3dgef.RData")
#save(km3_po, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3po.RData")
#save(km3_fa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3fa.RData")
#save(km3_dg, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3dg.RData")
#save(km3_faid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3faid.RData")

