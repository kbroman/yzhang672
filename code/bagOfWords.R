#rm(list = ls())

load(file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/mysarkPostsBow.RData")
wc = colSums(A)
good = which(wc > 15 & wc < 166423/10)
hist(sqrt(wc[good]))
A = A[,good]

#As = A[,sample(ncol(A), 100)]


tmp = t(As)%*%As


dim(A)
library(irlba)
str(irlba(A))

corp = Corpus(VectorSource(datText2$text[1:500]))
inspect(corp[1:4])
corp = tm_map(corp, tolower); inspect(corp[1:4])
corp = tm_map(corp, removeWords, stopwords("french"))


tmp = TermDocumentMatrix(corp)

#?not understand TermDocumentMatrix


library(qdap)
tmp = bag_o_words(datText2$text[1:500])