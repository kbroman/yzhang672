load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/alluniquepost.RData")
load('C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData')
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/postID.RData")

postid=colnames(allpostfans)
lostpost=which(is.na(match(postid,postID)))
candidates=c(rep("lepen",n[1]),rep("joly",n[2]),rep('bayrou',n[3]),
             rep('hollande',n[4]),rep('melenchon',n[5]),
             rep('dupont',n[6]),rep('sarkozy',n[7]),
             rep('poutou',n[8]))
lostid=alluniquepost[lostpost]
lostcd=candidates[lostpost]
lostdf=data.frame(lostcd,lostid)

nlost=rep(0,8)
nlost[1]=sum((lostcd=='lepen')*1)
nlost[2]=sum((lostcd=='joly')*1)
nlost[3]=sum((lostcd=='bayrou')*1)
nlost[4]=sum((lostcd=='hollande')*1)
nlost[5]=sum((lostcd=='melenchon')*1)
nlost[6]=sum((lostcd=='dupont')*1)
nlost[7]=sum((lostcd=='sarkozy')*1)
nlost[8]=sum((lostcd=='poutou')*1)
nlost
sum(nlost)

#save(lostdf, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/lostdf.RData")
#save(nlost, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/nlost.RData")
