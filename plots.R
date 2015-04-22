load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')
lepe = tables$lepen
joly = tables$joly
bayr = tables$bayrou
holl = tables$hollande
mele = tables$melenchon
dupo = tables$dupont
sark = tables$sarkozy
pout = tables$poutou

length(unique(bayr$fan_id))
length(unique(holl$fan_id))
length(unique(sark$fan_id))
length(unique(c(bayr$fan_id,holl$fan_id)))



#shstructure
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shpostfans.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrpoef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrdgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm3poef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm3dgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm3po.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm3fa.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm3dg.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shstrkm3faid.RData")

nkm=3
nr=nrow(sh_postfans)

par(mfrow=c(3,2))
hist(poef,breaks=nr)
hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

plot(km3_po)
plot(km3_dg)
plot(km3_fa)
km3_po
km3_dg
km3_fa


ffcc=c(0,length(unique(holl$fan_id)),
       length(unique(c(holl$fan_id,sark$fan_id))))
fcc=rep(0,length(km3_faid))
for(i in 1:length(km3_faid))
{
  for(j in 1:2)
  {
    if(ffcc[j]<km3_faid[i] && km3_faid[i]<ffcc[j+1]+1)
    {fcc[i]=j}
  }
}

par(mfrow=c(1,1))
plot(km3_faid,col=fcc)

par(mfrow=c(nkm,2))
for(j in 1:nkm)
{hist(km3_poef[km3_poef[,j]>0,j],breaks=nr)
 hist(km3_dgef[km3_dgef[,j]>0,j],breaks=nr)
}

summary(km3_poef[km3_poef[,1]>0,1])
summary(km3_poef[km3_poef[,2]>0,2])
summary(km3_poef[km3_poef[,3]>0,3])
summary(km3_dgef[km3_dgef[,1]>0,1])
summary(km3_dgef[km3_dgef[,2]>0,2])
summary(km3_dgef[km3_dgef[,3]>0,3])

par(mfrow=c(nkm,1))
for(j in 1:nkm)
{hist(km3_dgef[km3_dgef[,j]>15,j],breaks=nr)
}
par(mfrow=c(nkm,2))
for (j in 1:nkm)
{plot(km3_poef[km3_poef[,j]>0,j])
 plot(km3_dgef[km3_dgef[,j]>0,j])}

#bshstructure nkm=3

load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshpostfans.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrpoef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrdgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3poef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3dgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3po.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3fa.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3dg.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm3faid.RData")


nkm=3
nr=nrow(bsh_postfans)

par(mfrow=c(3,2))
hist(poef,breaks=nr)
hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

plot(km3_po)
plot(km3_dg)
plot(km3_fa)
km3_po
km3_dg
km3_fa

ffcc=c(0,length(unique(bayr$fan_id)),length(unique(c(bayr$fan_id,holl$fan_id))),
       length(unique(c(bayr$fan_id,holl$fan_id,sark$fan_id))))
fcc=rep(0,length(km3_faid))
for(i in 1:length(km3_faid))
{
  for(j in 1:3)
  {
    if(ffcc[j]<km3_faid[i] && km3_faid[i]<ffcc[j+1]+1)
    {fcc[i]=j}
  }
}

par(mfrow=c(1,1))
plot(km3_faid,col=fcc)

par(mfrow=c(nkm,2))
for(j in 1:nkm)
{hist(km3_poef[km3_poef[,j]>0,j],breaks=nr)
 hist(km3_dgef[km3_dgef[,j]>0,j],breaks=nr)
}

summary(km3_poef[km3_poef[,1]>0,1])
summary(km3_poef[km3_poef[,2]>0,2])
summary(km3_poef[km3_poef[,3]>0,3])
summary(km3_dgef[km3_dgef[,1]>0,1])
summary(km3_dgef[km3_dgef[,2]>0,2])
summary(km3_dgef[km3_dgef[,3]>0,3])

par(mfrow=c(nkm,1))
for(j in 1:nkm)
{hist(km3_dgef[km3_dgef[,j]>15,j],breaks=nr)
}
par(mfrow=c(nkm,2))
for (j in 1:nkm)
{plot(km3_poef[km3_poef[,j]>0,j])
 plot(km3_dgef[km3_dgef[,j]>0,j])}

#bshstructure nkm=4

load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshpostfans.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrpoef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrdgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4poef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4dgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4po.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4fa.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4dg.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm4faid.RData")


nkm=4
nr=nrow(bsh_postfans)

par(mfrow=c(3,2))
hist(poef,breaks=nr)
hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

plot(km4_po)
plot(km4_dg)
plot(km4_fa)
km4_po
km4_dg
km4_fa

ffcc=c(0,length(unique(bayr$fan_id)),length(unique(c(bayr$fan_id,holl$fan_id))),
         length(unique(c(bayr$fan_id,holl$fan_id,sark$fan_id))))
fcc=rep(0,length(km4_faid))
for(i in 1:length(km4_faid))
{
  for(j in 1:3)
  {
    if(ffcc[j]<km4_faid[i] && km4_faid[i]<ffcc[j+1]+1)
    {fcc[i]=j}
  }
}

par(mfrow=c(1,1))
plot(km4_faid,col=fcc)

par(mfrow=c(2,2))
for(j in 1:nkm)
{hist(km4_poef[km4_poef[,j]>0,j],breaks=nr,ylim=c(0,20000))
# hist(km4_dgef[km4_dgef[,j]>0,j],breaks=nr)
}
par(mfrow=c(2,2))
for(j in 1:nkm)
{#hist(km4_poef[km4_poef[,j]>0,j],breaks=nr)
 hist(km4_dgef[km4_dgef[,j]>0,j],breaks=nr,xlim=c(0,3000),ylim=c(0,60))
}

summary(km4_poef[km4_poef[,1]>0,1])
summary(km4_poef[km4_poef[,2]>0,2])
summary(km4_poef[km4_poef[,3]>0,3])
summary(km4_dgef[km4_dgef[,1]>0,1])
summary(km4_dgef[km4_dgef[,2]>0,2])
summary(km4_dgef[km4_dgef[,3]>0,3])

par(mfrow=c(3,2))
for(j in 1:nkm)
{hist(km4_dgef[km4_dgef[,j]>15,j],breaks=nr,xlim=c(0,3000),ylim=c(0,60))
}
par(mfrow=c(3,2))
for (j in 1:nkm)
{
 #plot(km4_poef[km4_poef[,j]>0,j],xlim=c(0,35000),ylim=c(0,120))
 plot(km4_poef[km4_poef[,j]>0,j])
}
par(mfrow=c(3,2))
for (j in 1:nkm)
{
 #plot(km4_dgef[km4_dgef[,j]>0,j],xlim=c(0,35000),ylim=c(0,2500))
  plot(km4_dgef[km4_dgef[,j]>0,j])
}

#bshstructure nkm=5

load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshpostfans.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrpoef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrdgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5poef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5dgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5po.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5fa.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5dg.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshstrkm5faid.RData")


nkm=5
nr=nrow(bsh_postfans)

par(mfrow=c(3,2))
hist(poef,breaks=nr)
hist(dgef,breaks=nr)
summary(poef)
summary(dgef)

plot(km5_po)
plot(km5_dg)
plot(km5_fa)
km5_po
km5_dg
km5_fa

ffcc=c(0,length(unique(bayr$fan_id)),length(unique(c(bayr$fan_id,holl$fan_id))),
       length(unique(c(bayr$fan_id,holl$fan_id,sark$fan_id))))
fcc=rep(0,length(km5_faid))
for(i in 1:length(km5_faid))
{
  for(j in 1:3)
  {
    if(ffcc[j]<km5_faid[i] && km5_faid[i]<ffcc[j+1]+1)
    {fcc[i]=j}
  }
}

par(mfrow=c(1,1))
plot(km5_faid,col=fcc)

par(mfrow=c(3,2))
for(j in 1:nkm)
{hist(km5_poef[km5_poef[,j]>0,j],breaks=nr,ylim=c(0,20000))
 # hist(km5_dgef[km5_dgef[,j]>0,j],breaks=nr)
}
par(mfrow=c(3,2))
for(j in 1:nkm)
{#hist(km5_poef[km5_poef[,j]>0,j],breaks=nr)
  hist(km5_dgef[km5_dgef[,j]>0,j],breaks=nr,xlim=c(0,3000),ylim=c(0,60))
}

summary(km5_poef[km5_poef[,1]>0,1])
summary(km5_poef[km5_poef[,2]>0,2])
summary(km5_poef[km5_poef[,3]>0,3])
summary(km5_dgef[km5_dgef[,1]>0,1])
summary(km5_dgef[km5_dgef[,2]>0,2])
summary(km5_dgef[km5_dgef[,3]>0,3])

par(mfrow=c(3,2))
for(j in 1:nkm)
{hist(km5_dgef[km5_dgef[,j]>15,j],breaks=nr,xlim=c(0,3000),ylim=c(0,60))
}
par(mfrow=c(3,2))
for (j in 1:nkm)
{
  #plot(km5_poef[km5_poef[,j]>0,j],xlim=c(0,35000),ylim=c(0,120))
  plot(km5_poef[km5_poef[,j]>0,j])
}
par(mfrow=c(3,2))
for (j in 1:nkm)
{
  #plot(km5_dgef[km5_dgef[,j]>0,j],xlim=c(0,35000),ylim=c(0,2500))
  plot(km5_dgef[km5_dgef[,j]>0,j])
}


#delstructure
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrpoef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrdgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3poef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3dgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3po.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3fa.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3dg.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/delstrkm3faid.RData")

nkm=3
nr=nrow(allpostfans)

par(mfrow=c(3,2))
hist(poef,breaks=nr,xlim=c(0,50),ylim=c(0,10000))
hist(dgef,breaks=nr,xlim=c(0,50),ylim=c(0,10000))
summary(poef)
summary(dgef)

plot(km3_po)
plot(km3_dg)
plot(km3_fa)
km3_po
km3_dg
km3_fa


load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ffcc.RData")
ffcc=c(0,ffcc)
fcc=rep(0,length(km3_faid))
for(i in 1:length(km3_faid))
{
  for(j in 1:8)
  {
   if(ffcc[j]<km3_faid[i] && km3_faid[i]<ffcc[j+1]+1)
    {fcc[i]=j}
  }
}


par(mfrow=c(1,1))
plot(km3_faid,col=fcc)

par(mfrow=c(nkm,2))
for(j in 1:nkm)
{
 hist(km3_poef[km3_poef[,j]>0,j],breaks=nr,xlim=c(0,150),ylim=c(0,80))
 hist(km3_dgef[km3_dgef[,j]>0,j],breaks=nr,xlim=c(0,2000),ylim=c(0,100))
}

summary(km3_poef[km3_poef[,1]>0,1])
summary(km3_poef[km3_poef[,2]>0,2])
summary(km3_poef[km3_poef[,3]>0,3])
summary(km3_dgef[km3_dgef[,1]>0,1])
summary(km3_dgef[km3_dgef[,2]>0,2])
summary(km3_dgef[km3_dgef[,3]>0,3])

par(mfrow=c(nkm,2))
for (j in 1:nkm)
{plot(km3_poef[km3_poef[,j]>0,j])
 plot(km3_dgef[km3_dgef[,j]>0,j])}

#structure
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrpoef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrdgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8poef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8dgef.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8po.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8fa.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8dg.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allstrkm8faid.RData")

nkm=8
nr=nrow(allpostfans)

par(mfrow=c(3,2))
hist(poef,breaks=nr,xlim=c(0,50),ylim=c(0,10000))
hist(dgef,breaks=nr,xlim=c(0,50),ylim=c(0,10000))
summary(poef)
summary(dgef)

plot(km8_po,main="post")
plot(km8_dg,main="degree")
plot(km8_fa,main="fan")
km8_po
km8_dg
km8_fa


load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ffcc.RData")
ffcc=c(0,ffcc)
fcc=rep(0,length(km8_faid))
for(i in 1:length(km8_faid))
{
  for(j in 1:8)
  {
    if(ffcc[j]<km8_faid[i] && km8_faid[i]<ffcc[j+1]+1)
    {fcc[i]=j}
  }
}


par(mfrow=c(1,1))
plot(km8_faid,col=fcc)

par(mfrow=c(nkm,2))
for(j in 1:nkm)
{
  hist(km8_poef[km8_poef[,j]>0,j],breaks=nr,xlim=c(0,150),ylim=c(0,80))
  hist(km8_dgef[km8_dgef[,j]>0,j],breaks=nr,xlim=c(0,2000),ylim=c(0,100))
}

summary(km8_poef[km8_poef[,1]>0,1])
summary(km8_poef[km8_poef[,2]>0,2])
summary(km8_poef[km8_poef[,3]>0,3])
summary(km8_dgef[km8_dgef[,1]>0,1])
summary(km8_dgef[km8_dgef[,2]>0,2])
summary(km8_dgef[km8_dgef[,3]>0,3])

par(mfrow=c(nkm,2))
for (j in 1:nkm)
{plot(km8_poef[km8_poef[,j]>0,j])
 plot(km8_dgef[km8_dgef[,j]>0,j])}
