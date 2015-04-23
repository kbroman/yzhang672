library(XLConnectJars)
library(XLConnect)  # To read Excel sheets and workbooks
library(NLP)
library(tm)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(qdap)
library(Matrix)


setwd("F:/UWMadison/project/YL/data/")

wb <- loadWorkbook("F:/UWMadison/project/French candidates/YL/data/sarkozyPost.xls")
Tables <- readWorksheet(wb, sheet = getSheets(wb))

sarkposts = Tables[, c(9,15)]
#previously it was Tables[-(1:3), c(7,12)]

corp = Corpus(VectorSource(sarkposts[,1]))
corp = tm_map(corp, tolower)
corp = tm_map(corp, removeWords, stopwords("french"))


Etxt = lapply(corp, bag_o_words)
dict = unique(c(do.call("cbind", Etxt)))
E = lapply(Etxt, match, dict)
nel = lapply(E, length)
m = sum(c(do.call("cbind", nel)))
i = rep(0, m); j = i; x=rep(1, m)
tick = 0
for(e in 1:length(E)){
	deg = nel[e][[1]]
	if(deg>0){
		i[(tick + 1):(tick +deg)] = e
		j[(tick + 1):(tick +deg)] = E[[e]]
		tick = tick +  deg
	}
}
A = spMatrix(nrow= max(i), ncol = max(j), i,j,x)
colnames(A) = dict
wc = colSums(A)
good = which(wc > 4 & wc < nrow(A)/2)
# dict[good]
sarkpA = A[,good]

drop = function(tmp){
	return(substr(tmp, start = 26, stop = nchar(tmp)))	
}
postID = lapply(sarkposts[,2], drop)
postID = unique(c(do.call("cbind", postID)))

rownames(sarkpA) = postID
save(sarkpA, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/mysarkPostsBow.RData")


#rm(list = ls())

