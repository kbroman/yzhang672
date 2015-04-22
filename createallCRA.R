library(Matrix)
load('F:/UWMadison/project/previous work/fordesk00/mydata/tables.RData')
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

n = nrow(lepe)+nrow(joly)+nrow(bayr)+nrow(holl)+
    nrow(mele)+nrow(dupo)+nrow(sark)+nrow(pout)

allcrA = spMatrix(nrow =n , ncol = length(alluniquepost),
	   i = 1:n,
	   j = match(allparent_url, alluniquepost), 
	   x = rep(1, n))

dim(allcrA)
rownames(allcrA) = 1:n

# to match posts, we need to drop some text
drop2 = function(tmp){
	return(substr(tmp, start = 21, stop = nchar(tmp)))	
}
up = lapply(alluniquepost,drop2)

colnames(allcrA) = up
save(allcrA, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcrA.RData")

