rm(list = ls())

library(Matrix)
load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')
#lepe = tables$lepen
#joly = tables$joly
bayr = tables$bayrou
holl = tables$hollande
#mele = tables$melenchon
#dupo = tables$dupont
sark = tables$sarkozy
#pout = tables$poutou

sh_parent_url = c(holl$parent_url,sark$parent_url)

<<<<<<< HEAD
sh_fans=c(holl$fan_id,sark$fan_id)

sh_postfans = createA(sh_fans,sh_parent_url)
=======
sh_uniquepost = unique(sh_parent_url)

sh_fans=c(holl$fan_id,sark$fan_id)

sh_uniquefan = unique(sh_fans)

n=length(sh_fans)
nf=length(sh_uniquefan)

cfans=match(sh_fans,sh_uniquefan)
cpost=match(sh_parent_url, sh_uniquepost)

#allcrA = spMatrix(nrow =n , ncol = length(alluniquepost),
#                  i = 1:n,
#                  j = cpost, 
#                  x = rep(1, n))

sh_postfans = spMatrix(nrow =nf , ncol = length(sh_uniquepost),
                       i = cfans,
                       j = cpost, 
                       x = rep(1, n))
#dim(allcrA)
dim(sh_postfans)

# to match posts, we need to drop some text
drop2 = function(tmp){
  return(substr(tmp, start = 21, stop = nchar(tmp)))  
}
up = lapply(sh_uniquepost,drop2)

colnames(sh_postfans) = up
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
#save(allcrA, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcrA.RData")
save(sh_postfans, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/shpostfans.RData")



bsh_parent_url = c(bayr$parent_url,holl$parent_url,sark$parent_url)

bsh_uniquepost = unique(bsh_parent_url)

bsh_fans=c(bayr$fan_id,holl$fan_id,sark$fan_id)

bsh_uniquefan = unique(bsh_fans)

n=length(bsh_fans)
nf=length(bsh_uniquefan)

cfans=match(bsh_fans,bsh_uniquefan)
cpost=match(bsh_parent_url, bsh_uniquepost)

#allcrA = spMatrix(nrow =n , ncol = length(alluniquepost),
#                  i = 1:n,
#                  j = cpost, 
#                  x = rep(1, n))

bsh_postfans = spMatrix(nrow =nf , ncol = length(bsh_uniquepost),
                       i = cfans,
                       j = cpost, 
                       x = rep(1, n))
#dim(allcrA)
dim(bsh_postfans)

# to match posts, we need to drop some text
up = lapply(bsh_uniquepost,drop2)

colnames(bsh_postfans) = up
#save(allcrA, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allcrA.RData")
save(bsh_postfans, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bshpostfans.RData")
