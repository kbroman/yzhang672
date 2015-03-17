rm(list = ls())

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

allparent_url = c(lepe$parent_url,joly$parent_url,bayr$parent_url,
                  holl$parent_url,mele$parent_url,dupo$parent_url,
                  sark$parent_url,pout$parent_url)

alluniquepost = unique(allparent_url)

matchpost=match(allparent_url, alluniquepost)

npost=length(alluniquepost)
mpost=rep(0,npost)
for (i in 1:npost)
{mpost[i]=min(which(matchpost==i))}

timeallcomments=c(lepe$utc_time, joly$utc_time, bayr$utc_time,
                  holl$utc_time, mele$utc_time, dupo$utc_time,
                  sark$utc_time, pout$utc_time)

timeall=timeallcomments[mpost]

drop = function(tmp){
  return(substr(tmp, 1, 10))	
}
dateall = lapply(timeall, drop)

date=unique(dateall)
date=c(do.call("cbind",date)) 
date=sort(date)
length(dateall)
length(date)
datem=match(dateall,date)

save(datem, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostdate.RData")
save(date, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostdatetrue.RData")
save(dateall, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/alldate.RData")

