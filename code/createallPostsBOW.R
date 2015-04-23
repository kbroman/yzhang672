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

setwd("F:/UWMadison/project/French candidates/YL/data/")

wblepe <- loadWorkbook("F:/UWMadison/project/French candidates/YL/data/lepenPost.xls")
wbjoly <- loadWorkbook("F:/UWMadison/project/French candidates/YL/data/jolyPost.xls")
wbbayr <- loadWorkbook("F:/UWMadison/project/French candidates/YL/data/bayrouPost.xls")
wbholl <- loadWorkbook("F:/UWMadison/project/French candidates/YL/data/hollandePost.xls")
wbmele <- loadWorkbook("F:/UWMadison/project/French candidates/YL/data/melenchonPost.xls")
wbdupo <- loadWorkbook("F:/UWMadison/project/French candidates/YL/data/dupontPost.xls")
wbsark <- loadWorkbook("F:/UWMadison/project/French candidates/YL/data/sarkozyPost.xls")
wbpout <- loadWorkbook("F:/UWMadison/project/French candidates/YL/data/poutouPost.xls")

Tableslepe <- readWorksheet(wblepe, sheet = getSheets(wblepe))
Tablesjoly <- readWorksheet(wbjoly, sheet = getSheets(wbjoly))
Tablesbayr <- readWorksheet(wbbayr, sheet = getSheets(wbbayr))
Tablesholl <- readWorksheet(wbholl, sheet = getSheets(wbholl))
Tablesmele <- readWorksheet(wbmele, sheet = getSheets(wbmele))
Tablesdupo <- readWorksheet(wbdupo, sheet = getSheets(wbdupo))
Tablessark <- readWorksheet(wbsark, sheet = getSheets(wbsark))
Tablespout <- readWorksheet(wbpout, sheet = getSheets(wbpout))

postslepe = Tableslepe[, c(9,15)]
postsjoly = Tablesjoly[, c(9,15)]
postsbayr = Tablesbayr[, c(9,15)]
postsholl = Tablesholl[, c(9,15)]
postsmele = Tablesmele[, c(9,15)]
postsdupo = Tablesdupo[, c(9,15)]
postssark = Tablessark[, c(9,15)]
postspout = Tablespout[, c(9,15)]

posts=rbind(postslepe,postsjoly,postsbayr,postsholl,postsmele,postsdupo,postssark,postspout)

corp = Corpus(VectorSource(posts[,1]))
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
allpA = A[,good]

drop = function(tmp){
	return(substr(tmp, start = 25, stop = nchar(tmp)))	
}
postID = lapply(posts[,2], drop)
postID = unique(c(do.call("cbind", postID)))

rownames(allpA) = postID
save(allpA, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/myallPostsBow.RData")
save(postID, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/postID.RData")


