library(Matrix)

drop2 = function(tmp){
  return(substr(tmp, start = 21, stop = nchar(tmp)))  
}

createA <- function(x,y)
{
  ux = unique(x); uy = unique(y);
  n  = length(x); nx = length(ux); ny = length(uy)
  cx = match(x,ux); cy = match(y,uy);
  A  = spMatrix(nrow = nx , ncol = ny, 
                i = cx , j = cy , x = rep(1,n))
  
  up = lapply(uy,drop2)
  colnames(A) = up
  return(A)
}

load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')
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

allfans=c(lepe$fan_id,joly$fan_id,bayr$fan_id,holl$fan_id,
          mele$fan_id,dupo$fan_id,sark$fan_id,pout$fan_id)

allpostfans=createA(allfans,allparent_url)

save(allpostfans, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allpostfans.RData")


