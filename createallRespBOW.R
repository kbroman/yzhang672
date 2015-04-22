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

library(tm)
load('F:/UWMadison/project/previous work/fordesk00/mydata/tables.RData')
lepe = tables$lepen
joly = tables$joly
bayr = tables$bayrou
holl = tables$hollande
mele = tables$melenchon
dupo = tables$dupont
sark = tables$sarkozy
pout = tables$poutou

resplepe = lepe$text
respjoly = joly$text
respbayr = bayr$text
respholl = holl$text
respmele = mele$text
respdupo = dupo$text
respsark = sark$text
resppout = pout$text

resp = c(resplepe,respjoly,respbayr,respholl,respmele,respdupo,respsark,resppout)

Etxt = lapply(resp, bag_o_words)
dict = unique(c(do.call("cbind",Etxt)))
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
