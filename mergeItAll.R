
## construct files: 
# source('~/Dropbox/frenchFacebook/code/createSarkCRA.R', chdir = TRUE)
#rm(list = ls())
# source('~/Dropbox/frenchFacebook/code/createSarkPostsBOW.R', chdir = TRUE)
#rm(list = ls())
# source('~/Dropbox/frenchFacebook/code/createSarkRespBow.R', chdir = TRUE)
library(Matrix)

#rm(list = ls())

##load the files.
rm(list = ls())
load('F:/UWMadison/project/French candidates/previous work/data/MyDataFiles/crA.RData')
load('F:/UWMadison/project/French candidates/previous work/data/MyDataFiles/sarkPostsBow.RData')
load('F:/UWMadison/project/French candidates/previous work/data/MyDataFiles/sarkResponseSmallBow.RData')

respId = colnames(crA); callId = rownames(sarkpA)
length(respId)
length(callId)
mat =  match(callId, respId)
bad =  which(is.na(mat))
sarkpA = sarkpA[-bad,]

respId = colnames(crA); callId = rownames(sarkpA)
length(respId)
length(callId)
mat =  match(callId, respId)
crA = crA[,mat]

respId = colnames(crA); callId = rownames(sarkpA)
length(respId)
length(callId)
diff(match(callId, respId))
sum(diff(match(callId, respId))!=1)
rm(bad, callId, mat, respId)
ls()
dim(t(As)); dim(crA); dim(sarkpA)
cr = t(As)%*%crA%*%sarkpA
rs  = rowSums(cr); cs = colSums(cr)
summary(rs); summary(cs)
crDrop = cr[,-which(cs ==0)]
cs = colSums(crDrop)
L = Diagonal(length(rs), 1/sqrt(rs + mean(rs)))%*%crDrop%*%Diagonal(length(cs), 1/sqrt(cs + mean(cs)))

s = svd(L)
plot(s$d)
v = s$v[,1:10]
v = t(apply(v, 1, function(x) return(x/ sqrt(sum(x^2)))))
plot(as.data.frame(v))
u = s$u[,1:10]
u = t(apply(u, 1, function(x) return(x/ sqrt(sum(x^2)))))
vu = rbind(v,u)
km = kmeans(vu , 10, nstart = 1)

nv = nrow(v); nu = nrow(u)
clust = km$clust
vclust = clust[1:nv]; clust = clust[-(1:nv)]
uclust = clust


sdic = colnames(cr)  #sarkozy dcitionary
rdic = rownames(cr)  #responder diciotnary

sclust = list()
for(i in 1:10) sclust[[i]] = sdic[vclust == i]

rclust = list()
for(i in 1:10) rclust[[i]] = rdic[uclust == i] #it was vclust, I changed it.
str(rclust)
