
load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')

library(XLConnect)  # To read Excel sheets and workbooks

setwd(dir="C:/Users/Administrator/Dropbox/myproject/frenchFacebook/dataexcel/")

lp <- loadWorkbook("C:/Users/Administrator/Dropbox/my project/frenchFacebook/dataexcel/lostposts.xlsx")
lp <- readWorksheet(lp, sheet = getSheets(lp))
lp <- lp[,c(2,1,3,4)]

droptime = function(tmp){
  return(substr(tmp, start = 1, stop = 10))  
}

lp[,4] <- droptime(lp[,4])
lp[,2] <- drop2(lp[,2])

wb <- loadWorkbook("C:/Users/Administrator/Dropbox/my project/frenchFacebook/dataexcel/lepenPost.xls")
lepe1 <- readWorksheet(wb, sheet = getSheets(wb))
lepe <- lepe1[,c(7,15,9,2)]
lepe[,2] <- drop1(lepe[,2])
lepe[,4] <- droptime(lepe[,4])

wb <- loadWorkbook("C:/Users/Administrator/Dropbox/my project/frenchFacebook/dataexcel/jolyPost.xls")
joly1 <- readWorksheet(wb, sheet = getSheets(wb))
joly <- joly1[,c(7,15,9,2)]
joly[,2] <- drop1(joly[,2])
joly[,4] <- droptime(joly[,4])

wb <- loadWorkbook("C:/Users/Administrator/Dropbox/my project/frenchFacebook/dataexcel/bayrouPost.xls")
bayr1 <- readWorksheet(wb, sheet = getSheets(wb))
bayr <- bayr1[,c(7,15,9,2)]
bayr[,2] <- drop1(bayr[,2])
bayr[,4] <- droptime(bayr[,4])

wb <- loadWorkbook("C:/Users/Administrator/Dropbox/my project/frenchFacebook/dataexcel/hollandePost.xls")
holl1 <- readWorksheet(wb, sheet = getSheets(wb))
holl <- holl1[,c(7,15,9,2)]
holl[,2] <- drop1(holl[,2])
holl[,4] <- droptime(holl[,4])

wb <- loadWorkbook("C:/Users/Administrator/Dropbox/my project/frenchFacebook/dataexcel/melenchonPost.xls")
mele1 <- readWorksheet(wb, sheet = getSheets(wb))
mele <- mele1[,c(7,15,9,2)]
mele[,2] <- drop1(mele[,2])
mele[,4] <- droptime(mele[,4])

wb <- loadWorkbook("C:/Users/Administrator/Dropbox/my project/frenchFacebook/dataexcel/dupontPost.xls")
dupo1 <- readWorksheet(wb, sheet = getSheets(wb))
dupo <- dupo1[,c(7,15,9,2)]
dupo[,2] <- drop1(dupo[,2])
dupo[,4] <- droptime(dupo[,4])
dupo[,4] <- as.Date(dupo[,4], format = "%d-%m-20%y")
dupo[,4] <- as.character(dupo[,4])

wb <- loadWorkbook("C:/Users/Administrator/Dropbox/my project/frenchFacebook/dataexcel/sarkozyPost.xls")
sark1 <- readWorksheet(wb, sheet = getSheets(wb))
sark <- sark1[,c(7,15,9,2)]
sark[,2] <- drop1(sark[,2])
sark[,4] <- droptime(sark[,4])

wb <- loadWorkbook("C:/Users/Administrator/Dropbox/my project/frenchFacebook/dataexcel/poutouPost.xls")
pout1 <- readWorksheet(wb, sheet = getSheets(wb))
pout <- pout1[,c(7,15,9,2)]
pout[,2] <- drop1(pout[,2])
pout[,4] <- droptime(pout[,4])

colnames(lp)=colnames(lepe)
posts <- rbind(lp,lepe,joly,bayr,holl,mele,dupo,sark,pout)
colnames(posts)=c("candidate","posturl","posttext","postdate")

##################################################################

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
alluniquepost = drop2(alluniquepost)

mpost = match(posts$posturl,alluniquepost)
length(mpost)
sum(is.na(mpost))
dropost = which(is.na(mpost))
posts0=posts
posts=posts0[-dropost,]

mpost = match(posts$posturl,alluniquepost)
length(mpost)
sum(is.na(mpost))

mpost = match(alluniquepost,posts$posturl)
sum((posts[mpost,]$posturl==alluniquepost))
allposts = posts[mpost,]

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/alldate.RData")

dateall = unlist(dateall)
nodate = which(is.na(allposts[,4]))
allposts[nodate,4]=dateall[nodate]

alldates = allposts[,4]
unidate = as.vector(sort(unique(alldates))[2:103])
mdates = match(alldates,unidate)
no = which(is.na(mdates))
alldates[no] = dateall[no]
allposts[,4] = alldates

############################################################3

mmpost = match(drop2(lepe$parent_url),allposts$posturl)
lepe = cbind(lepe,allposts$posttext[mmpost],allposts$postdate[mmpost])
colnames(lepe) = c(colnames(lepe)[1:8],"posttext","postdate")
lepe=lepe[,-1]

mmpost = match(drop2(joly$parent_url),allposts$posturl)
joly = cbind(joly,allposts$posttext[mmpost],allposts$postdate[mmpost])
colnames(joly) = c(colnames(joly)[1:7],"posttext","postdate")

mmpost = match(drop2(bayr$parent_url),allposts$posturl)
bayr = cbind(bayr,allposts$posttext[mmpost],allposts$postdate[mmpost])
colnames(bayr) = c(colnames(bayr)[1:7],"posttext","postdate")

mmpost = match(drop2(holl$parent_url),allposts$posturl)
holl = cbind(holl,allposts$posttext[mmpost],allposts$postdate[mmpost])
colnames(holl) = c(colnames(holl)[1:7],"posttext","postdate")

mmpost = match(drop2(mele$parent_url),allposts$posturl)
mele = cbind(mele,allposts$posttext[mmpost],allposts$postdate[mmpost])
colnames(mele) = c(colnames(mele)[1:7],"posttext","postdate")

mmpost = match(drop2(dupo$parent_url),allposts$posturl)
dupo = cbind(dupo,allposts$posttext[mmpost],allposts$postdate[mmpost])
colnames(dupo) = c(colnames(dupo)[1:7],"posttext","postdate")

mmpost = match(drop2(sark$parent_url),allposts$posturl)
sark = cbind(sark,allposts$posttext[mmpost],allposts$postdate[mmpost])
colnames(sark) = c(colnames(sark)[1:8],"posttext","postdate")
sark = sark[,-1]

mmpost = match(drop2(pout$parent_url),allposts$posturl)
pout = cbind(pout,allposts$posttext[mmpost],allposts$postdate[mmpost])
colnames(pout) = c(colnames(pout)[1:7],"posttext","postdate")

altogether = rbind(lepe,joly,bayr,holl,mele,dupo,sark,pout)

save(nodate, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/nodate.RData")
save(allposts, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allposts.RData")
save(altogether, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/altogether.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/altogether.RData")

#write.csv(altogether, file = "altogether.csv",row.names=FALSE)
