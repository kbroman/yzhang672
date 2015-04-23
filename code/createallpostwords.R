library(Matrix)
library(XML);library(RCurl);library(devtools);library(qdap)
library(stringr);library(tm);library(RWeka);library(wordcloud)

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allposts.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/altogether.RData")


allposturl = colnames(allpostfans)
sum(allposts$posturl==drop2(allposturl))

allposttext = allposts$posttext

allpostwords = freqmatr(textclean(allposttext),allposturl)

# 1/1000
#save(allpostwords, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostwords.RData")

dim(allpostwords)

###########################
allcomtext = altogether$text
allcomid = rownames(altogether)

#allcomwords = textclean(allcomtext,allcomid)

load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')
lepe = tables$lepen
joly = tables$joly
bayr = tables$bayrou
holl = tables$hollande
mele = tables$melenchon
dupo = tables$dupont
sark = tables$sarkozy
pout = tables$poutou

lepetext <- lepe$text;jolytext <- joly$text;bayrtext <- bayr$text;holltext <- holl$text
meletext <- mele$text;dupotext <- dupo$text;sarktext <- sark$text;pouttext <- pout$text

lepecomtxt <- textclean(lepetext);jolycomtxt <- textclean(jolytext)
bayrcomtxt <- textclean(bayrtext);hollcomtxt <- textclean(holltext)
melecomtxt <- textclean(meletext);dupocomtxt <- textclean(dupotext)
sarkcomtxt <- textclean(sarktext);poutcomtxt <- textclean(pouttext)

allcomtxt <- c(lepecomtxt,jolycomtxt,bayrcomtxt,hollcomtxt,
               melecomtxt,dupocomtxt,sarkcomtxt,poutcomtxt)

#save(allcomtxt, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcomtxt.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcomtxt.RData")

allfanid = altogether$fan_id
allcomwords <- freqmatr(allcomtxt,allfanid)

#save(allcomwords, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcomwords.RData")

###########################
allparenturl = altogether$parent_url
allcompost=createA(allcomid,allparenturl)
#save(allcompost, file="C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcompost.RData")

##########################

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostwords.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcompost.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcomwords.RData")

dim(allpostwords)
dim(allcompost)
dim(allcomwords)

Cr1 <- t(allpostwords) %*% t(allcompost) 
Cr <- Cr1 %*% allcomwords 
save(Cr, file="C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/Cr.RData")

###################################

str(allcomwords)

allfanwords = create_fanwords(allcomwords)
#save(allfanwords, file="C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords.RData")

####################################
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostwords.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcompost.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcomwords.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords.RData")

dim(allpostwords)
dim(allcompost)
dim(allcomwords)
dim(allfanwords)

CR = allpostwords %*% t(allpostwords) %*%  t(allcompost) %*% allcomwords %*% t(allfanwords)









A = spMatrix(nrow = 5, ncol = 8,
             i = c(1,1,2,2,2,3,3,4,4,5,5,5),
             j = c(1,2,3,4,5,1,2,3,4,5,6,7),
             x = c(1,1,1,1,1,1,1,1,1,1,1,1))
rownames(A) = c("a","b","c","a","b")
colnames(A) = c("1","2","3","4","5","6","7","8")
create_fanwords(A)





