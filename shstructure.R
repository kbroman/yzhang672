rm(list = ls())

#library(Matrix)
#rm(list = ls())

#load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shpostfans.RData')

#rs=rowSums(sh_postfans)
#cs=colSums(sh_postfans)
#summary(rs); summary(cs)
#L = Diagonal(length(rs), 1/sqrt(rs + mean(rs)))%*%
#  sh_postfans%*%Diagonal(length(cs), 1/sqrt(cs + mean(cs)))

#library(irlba)
#sh_sf=irlba(L,nu=20,nv=20)
#sfd=sh_sf$d[1:20]
#plot(sfd)

#save(sh_sf, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdshpostfan.RData")

rm(list = ls())
library(cluster)
library(fpc)

load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shpostfans.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdshpostfan.RData")
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shpostfans.RData')
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostdate.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostdatetrue.RData")


par(mfrow=c(1,1))
plot(sh_sf$d)
dim(sh_sf$u)
dim(sh_sf$v)
v = sh_sf$v[,1:4]
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))


###################################
####plot singular vectors
####################################
cc = c()
for(i in 1:length(n)) cc = c(cc, rep(i,n[i]))
h_cc=cc[(sum(n[1:3])+1):sum(n[1:4])]
s_cc=cc[(sum(n[1:6])+1):sum(n[1:7])]
sh_cc=c(h_cc,s_cc)

h_datem=datem[(sum(n[1:3])+1):sum(n[1:4])]
s_datem=datem[(sum(n[1:6])+1):sum(n[1:7])]
sh_datem=c(h_datem,s_datem)

dfvd=data.frame(v,sh_datem,sh_cc)
dfvdo=dfvd[order(dfvd$sh_cc,dfvd$sh_datem),]
dfvdo=as.matrix(dfvdo)
vd=dfvdo[,1:4]

dfvdl=dfvd[order(dfvd$sh_datem),]
dfvdl=as.matrix(dfvdl)
vdl=dfvdl[,1:4]

datem_od=dfvdl[,5]
daten=rep(1,53)
for(i in 2:53){daten[i]=sum(datem_od<=(i-1))+1}

par(mfrow = c(4,1), oma = rep(1,4))
i=1; plot(vd[,i], ylab = "", xlab="", col = sh_cc,pch=16)
legend("top",c("hollande","sarkozy"),col=c(4,7),pch=c(16,16))
for(i in 2:4) plot(vd[,i], ylab = "", xlab="", col = sh_cc,pch=16)

par(mfrow = c(2,1), mar = rep(1,4))
i=1; plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,6],pch=20)
legend("topright",c("hollande","sarkozy"),col=c(4,7),pch=c(20,20))
i=2; plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,6],pch=20)

i=3; plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,6],pch=20)
#for(d in 1:53) abline(v=daten[d],col="grey")
#legend("top",c("hollande","sarkozy"),col=c(4,7),pch=c(20,20))
i=4; plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,6],pch=20)
for(d in 1:53) abline(v=daten[d],col="grey")
abline(v=daten[36])
abline(v=daten[38])
abline(v=daten[39])
date[36:39]

par(mfrow = c(4,1), oma = rep(1,4))
i=1; plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,6],pch=16)
legend("topright",c("hollande","sarkozy"),col=c(4,7),pch=c(16,16))
for (i in 2:4) plot(vdl[,i], ylab = "", xlab="", col = dfvdl[,6],pch=16)

#km4 = kmeans(v, centers = 4, nstart = 1000)
#save(km4, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4.RData")

kmcluster=km4$cluster

################################################################
###########look at fan text
##################################################################
library(XML)
library(RCurl)
library(devtools)
library(qdap)
library(stringr)
library(tm)
library(RWeka)
library(wordcloud)

holl = tables$hollande
sark = tables$sarkozy

drop2 = function(tmp){
  return(substr(tmp, start = 21, stop = nchar(tmp)))  
}
holl_p = as.character(lapply(holl$parent_url,drop2))
sark_p= as.character(lapply(sark$parent_url,drop2))

holl_c=holl$text
sark_c=sark$text

nwords=100
myStopwords <- c(stopwords("French"),
                 "http","www","facebook","com","le")

Co_clus=NULL; Freqterm=NULL;Freqcount=NULL;Nfc=rep(0,4)
for(i in 1:4)
{
ci=colnames(allpostfans)[kmcluster==i]

hmi=match(holl_p,ci)
hi=hmi>0
hpi=holl_p[hi]
hpi=hpi[!is.na(hpi)]

hci=holl_c[hi]
hci=hci[!is.na(hci)]

smi=match(sark_p,ci)
si=smi>0
spi=sark_p[si]
spi=spi[!is.na(spi)]

sci=sark_c[si]
sci=sci[!is.na(sci)]

co_clusi=c(hci,sci)

co_clusi=str_replace_all(co_clusi, "[^[:alnum:]]", " ")
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

Co_clus=c(Co_clus,co_clusi)
Nfc[i]=length(co_clusi)
Freqterm=list(Freqterm,freqterm)
Freqcount=list(Freqcount,freqcount)
}
save(Freqterm, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFreqterm.RData")
save(Freqcount, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFreqcount.RData")
save(Nfc, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shNfc.RData")
save(Co_clus, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/sh_Co_clus.RData")

###########################################################

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFreqterm.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFreqcount.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shNfc.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/sh_Co_clus.RData")

nfq=c(130,128,112,134)
Fqt=unlist(Freqterm)
Fqt1=Fqt[1:nfq[1]]
Fqt2=Fqt[(nfq[1]+1):sum(nfq[1:2])]
Fqt3=Fqt[(sum(nfq[1:2])+1):sum(nfq[1:3])]
Fqt4=Fqt[(sum(nfq[1:3])+1):sum(nfq[1:4])]
Fqc=unlist(Freqcount)
Fqc1=Fqc[1:nfq[1]]
Fqc2=Fqc[(nfq[1]+1):sum(nfq[1:2])]
Fqc3=Fqc[(sum(nfq[1:2])+1):sum(nfq[1:3])]
Fqc4=Fqc[(sum(nfq[1:3])+1):sum(nfq[1:4])]

Fqtu=unique(Fqt)
m1=match(Fqtu,Fqt1);m2=match(Fqtu,Fqt2);
m3=match(Fqtu,Fqt3);m4=match(Fqtu,Fqt4)
mq1=match(Fqt1,Fqtu);mq2=match(Fqt2,Fqtu);
mq3=match(Fqt3,Fqtu);mq4=match(Fqt4,Fqtu)
Fqca=rep(0,length(Fqtu))
Fqca[mq1]=Fqc1
Fqca[mq2]=Fqca[mq2]+Fqc2
Fqca[mq3]=Fqca[mq3]+Fqc3
Fqca[mq4]=Fqca[mq4]+Fqc4
Fqa=data.frame(Fqtu,Fqca)

Fq1=data.frame(Fqt1,Fqc1,Fqca[mq1])
Fq2=data.frame(Fqt2,Fqc2,Fqca[mq2])
Fq3=data.frame(Fqt3,Fqc3,Fqca[mq3])
Fq4=data.frame(Fqt4,Fqc4,Fqca[mq4])
Fq1;Fq2;Fq3;Fq4;

common=which(m1>0 & m2>0 & m3>0 & m4>0)
Fqt_c=Fqtu[common]
Fqt_d=Fqtu[-common]
Fqt_d1=Fqt_d[which(match(Fqt_d,Fqt1)>0)]
Fqt_d2=Fqt_d[which(match(Fqt_d,Fqt2)>0)]
Fqt_d3=Fqt_d[which(match(Fqt_d,Fqt3)>0)]
Fqt_d4=Fqt_d[which(match(Fqt_d,Fqt4)>0)]

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

#save(Fq1, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFq1.RData")
#save(Fq2, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFq2.RData")
#save(Fq3, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFq3.RData")
#save(Fq4, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFq4.RData")
#save(Fqa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqa.RData")
#save(Fq_d1, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqd1.RData")
#save(Fq_d2, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqd2.RData")
#save(Fq_d3, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqd3.RData")
#save(Fq_d4, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqd4.RData")
#save(Fqt_c, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqtc.RData")


load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqa.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFq1.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFq2.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFq3.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFq4.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqd1.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqd2.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqd3.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqd4.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shFqtc.RData")

words=Fqd1[,1];freq=Fqd1[,2]
library(wordcloud)
wordcloud(words,freq,scale=c(4,.5),min.freq=3,max.words=Inf,
          random.order=FALSE, random.color=FALSE, rot.per=.1,
          ordered.colors=TRUE,use.r.layout=TRUE,
          fixed.asp=TRUE)



options(device="windows")

########################################################################
# number of comments of fan group s in post group t
#########################################################
# expected number of comments of fan group s in post group t


###########################################################
nclus=4;ncan=2
nsh0=c(0,n[4],(n[4]+n[7]))
bp=matrix(0,nrow=nclus,ncol=ncan)
# j candidate, i clusters
for(j in 1:ncan)
{for (i in 1:nclus)
{bp[i,j]=sum(km4$cluster[(nsh0[j]+1):nsh0[j+1]]==i)}
}
rownames(bp)=c("cluster1","cluster2","cluster3","cluster4")
colnames(bp)=c("hollande","sarkozy")
par(mfrow=c(1,1))
balloonPlot(bp)
bp
###########

par(mfrow=c(1,1))
library(fpc)
plotcluster(v, km4$cluster)
#library(cluster)
#clusplot(v, km4$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# how many posts in each cluster? 
# km4_po
# who are the fans in each cluster?
# km4_poid
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

dim(sh_postfans)
nr=nrow(sh_postfans)
poef=rep(0,nr)
dgef=rep(0,nr)
for(i in 1:nr)
{poef[i]=sum(sh_postfans[i,]>0)
 dgef[i]=sum(sh_postfans[i,])}

par(mfrow=c(1,2))
hist(poef,breaks=nr)
hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

nkm=4

km4_poid=NULL;
for (i in 1:nkm) {km4_poid=c(km4_poid,which(km4$cluster==i))}

par(mfrow=c(1,1))
plot(km4_poid)

km4_po=rep(0,nkm)
km4_fa=rep(0,nkm)
km4_dg=rep(0,nkm)

for(j in 1:nkm)
{
km4_po[j]=sum(km4$cluster==j)
km4_dg[j]=sum(sh_postfans[,km4$cluster==j])
}

km4_faid=NULL
for(j in 1:nkm)
{
for(i in 1:nr)
{if(sum(sh_postfans[i,km4$cluster==j])>0) 
   {km4_fa[j]=km4_fa[j]+1
    km4_faid=c(km4_faid,i)}
}
}

par(mfrow=c(2,2))
plot(km4_po,main="posts")
plot(km4_dg,main="degrees")
plot(km4_fa,main="fans")
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
    {if(sum(sh_postfans[i,km4$cluster==j])>0)
     {km4_poef[i,j]=km4_poef[i,j]+sum(sh_postfans[i,km4$cluster==j]>0)
      km4_dgef[i,j]=km4_dgef[i,j]+sum(sh_postfans[i,km4$cluster==j])
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
summary(km4_dgef[km4_dgef[,1]>0,1])
summary(km4_dgef[km4_dgef[,2]>0,2])
summary(km4_dgef[km4_dgef[,3]>0,3])

for (j in 1:nkm)
{plot(km4_poef[km4_poef[,j]>0,j])
 plot(km4_dgef[km4_dgef[,j]>0,j])}

save(poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrpoef.RData")
save(dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrdgef.RData")
save(km4_poef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4poef.RData")
save(km4_dgef, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4dgef.RData")
save(km4_poid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4poid.RData")
save(km4_po, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4po.RData")
save(km4_fa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4fa.RData")
save(km4_dg, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4dg.RData")
save(km4_faid, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm4faid.RData")