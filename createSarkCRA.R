library(Matrix)
load('F:/UWMadison/project/previous work/fordesk00/mydata/tables.RData')
sark = tables$sarkozy
head(sark)
uniquepost = unique(sark$parent_url)
n = nrow(sark)
crA = spMatrix(nrow =n , ncol = length(uniquepost),
	i = 1:n,
	j = match(sark$parent_url, uniquepost), 
	x = rep(1, nrow(sark)))

dim(crA)
rownames(crA) = 1:nrow(sark)

# to match posts, we need to drop some text
drop2 = function(tmp){
	return(substr(tmp, start = 21, stop = nchar(tmp)))	
}
up = lapply(uniquepost,drop2)

colnames(crA) = up
save(crA, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/crA.RData")

library(irlba)
s=irlba(crA,nu=20)
sdsark=s$d

rs=rowSums(crA)
cs=colSums(crA)
summary(rs); summary(cs)
L = Diagonal(length(rs), 1/sqrt(rs + mean(rs)))%*%
    crA%*%Diagonal(length(cs), 1/sqrt(cs + mean(cs)))

sn=irlba(L,nu=20)
sdsarknorm=sn$d
save(sdsark, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdcrAsark.RData")
save(sdsarknorm, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/svdcrAsarknorm.RData")




