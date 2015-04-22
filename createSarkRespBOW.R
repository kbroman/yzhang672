library(tm)

datText2 = read.table("~/Dropbox/frenchFacebook/Data/csvWithText/sarkozy.csv", sep=",", colClasses = c("character", "character", "character", "numeric", "character"), header=  T)

#similar to createSarkPostBOW. Maybe also delete some stopwords

posts = datText2$text
Etxt = lapply(posts, bag.o.words)
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
save(A, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/mysarkRespBow.RData")
rm(list = ls())

load(file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/mysarkRespBow.RData")
wc = colSums(A)
good = which(wc > 15 & wc < 166423/10)
As = A[,good]
save(As, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/mysarkRespSmall.RData")
