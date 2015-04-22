rm(list = ls())


load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
#allpostfans
nr=nrow(allpostfans)
allpostfans11=matrix(allpostfans[1:10000,]>0,nrow=10000)*1
allpostfans12=matrix(allpostfans[10001:20000,]>0,nrow=10000)*1
allpostfans13=matrix(allpostfans[20001:30000,]>0,nrow=10000)*1
allpostfans14=matrix(allpostfans[30001:40000,]>0,nrow=10000)*1
allpostfans15=matrix(allpostfans[40001:50000,]>0,nrow=10000)*1
allpostfans16=matrix(allpostfans[50001:60000,]>0,nrow=10000)*1


save(allpostfans11, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans11.RData")
save(allpostfans12, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans12.RData")
save(allpostfans13, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans13.RData")
save(allpostfans14, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans14.RData")
save(allpostfans15, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans15.RData")
save(allpostfans16, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans16.RData")

rm(list = ls())

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans11.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans12.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans13.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans14.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans15.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans16.RData")


allpostfans17=matrix(allpostfans[60001:70000,]>0,nrow=10000)*1
allpostfans18=matrix(allpostfans[70001:80000,]>0,nrow=10000)*1
allpostfans19=matrix(allpostfans[80001:92226,]>0,nrow=12226)*1
allpostfans1=rbind(allpostfans11,allpostfans12,allpostfans13,allpostfans14,allpostfans15,
                   allpostfans16,allpostfans17,allpostfans18,allpostfans19)
