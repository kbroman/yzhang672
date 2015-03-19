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

allfans=c(lepe$fan_id,joly$fan_id,bayr$fan_id,holl$fan_id,
          mele$fan_id,dupo$fan_id,sark$fan_id,pout$fan_id)

allpostfans=createA(allfans,allparent_url)

save(allpostfans, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData")


