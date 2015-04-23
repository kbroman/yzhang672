library(dplyr)
library(RSQLite)

drop1 = function(tmp){
  return(substr(tmp, start = 25, stop = nchar(tmp)))  
}

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
  
  #up = lapply(uy,drop2)
  up=uy
  colnames(A) = up
<<<<<<< HEAD
  rownames(A) = ux
=======
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
  return(A)
}

createB <- function(cc,a,ncluster)
{
  Y = model.matrix(~as.factor(cc)-1)
  yy = solve(t(Y)%*%Y)
  ap = a%*%Y%*%yy
  apn =t(apply(ap,1,function(x) return(x/sqrt(sum(x^2)))))
  
  kmap = kmeans(apn, ncluster, nstart = 1000)
  
  Zhat = sparse.model.matrix(~as.factor(kmap$clust)-1)
  zz = solve(t(Zhat)%*%Zhat)
  bhat = zz%*%t(Zhat)%*%a%*%Y%*%yy
  
  B0=round(bhat*3239)
  
  return(B0)
  
}


create_fanwords <- function(A)
{
  fan = A %>% rownames %>% unique
  cfan = match(A %>% rownames,fan)
  nx=length(fan);ny=length(A[1,])
<<<<<<< HEAD
  B = spMatrix(nrow = nx , ncol = ny, 
               i = cfan[A@i+1]  , j = A@j+1 , x = A@x ) 
  rownames(B) = fan
  colnames(B) = colnames(A)
  return(B)
}
  
  
=======
  
  Bi = NULL; Bj = NULL; Bx = NULL;
  for (i in 1:nx)
  {
    cf = which(cfan==i); 
    
    ci = which(A@i %in% (cf-1)); uj = A@j[ci]; ni = length(ci);
    Bi = c(Bi,rep(i,ni)); Bj = c(Bj, uj); Bx = c(Bx,A@x[ci])
  }
  
    B = spMatrix(nrow = nx , ncol = ny, 
                 i = Bi  , j = Bj+1 , x = Bx )  
   
  colnames(B)=colnames(A)
  rownames(B)=fan
  return(B)
}
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a

combind_fanwords <- function(A1,A2)
{
  rn1 = A1 %>% rownames; rn2 = A2 %>% rownames
  cn1 = A1 %>% colnames; cn2 = A2 %>% colnames
  
  fan = c(rn1, rn2) %>% unique
  cfan1 = match(rn1,fan)
  cfan2 = match(rn2,fan)
  
  words = c(cn1, cn2) %>% unique
  cword1 = match(cn1,words)
  cword2 = match(cn2,words)
  
  nx=length(fan);ny=length(words)
  
  Bi = NULL; Bj = NULL; Bx = NULL;
  for (i in 1:nx)
  {
    cf = which(cfan1==i);     
    ci = which(A1@i %in% (cf-1)); uj = cword1[(A1@j[ci]+1)]; ni = length(ci);
    Bi = c(Bi,rep(i,ni)); Bj = c(Bj, uj); Bx = c(Bx,A1@x[ci])
    
    cf = which(cfan2==i);    
    ci = which(A2@i %in% (cf-1)); uj = cword2[(A2@j[ci]+1)]; ni = length(ci);
    Bi = c(Bi,rep(i,ni)); Bj = c(Bj, uj); Bx = c(Bx,A2@x[ci])
  }
  
  B = spMatrix(nrow = nx , ncol = ny, 
               i = Bi  , j = Bj , x = Bx )  
  
  colnames(B)=words
  rownames(B)=fan
  return(B)
}




library(irlba)

svdmatrix <- function(A)
{
  rs=rowSums(A);cs=colSums(A)
  L = Diagonal(length(rs),1/sqrt(rs + mean(rs)))%*%
    A%*%Diagonal(length(cs),1/sqrt(cs + mean(cs)))
  sf=irlba(L,nu=20,nv=20)
  return(sf)
}

sv_bydate <- function(v , datem, cc, npost, ncluster)
{
  id=1:npost
  dfvd=data.frame(v,datem,cc,id)
  dfvdo=dfvd[order(dfvd$cc,dfvd$datem),]
  dfvdo=as.matrix(dfvdo)
  vd=dfvdo[,1:ncluster]
  
  dfvdl=dfvd[order(dfvd$datem),]
  dfvdl=as.matrix(dfvdl)
  vdl=dfvdl[,1:ncluster]
  
  datem_od=dfvdl[,(ncluster+1)]
  daten=rep(1,ndate)
  for(i in 2:ndate){daten[i]=sum(datem_od<=(i-1))+1}
  return(list(vd=vd,vdl=vdl,dfvdl=dfvdl,daten=daten))
}

myStopwords <- c(stopwords("French"),
                 "http","www","facebook","com","le")

<<<<<<< HEAD
textclean <- function (x)
{
  myCorpus <- x %>% VectorSource %>% Corpus
  myCorpus <- myCorpus %>% tm_map(removeWords,myStopwords) %>% tm_map(removeNumbers)
  mytdm <- myCorpus %>% TermDocumentMatrix
  return(mytdm)
}


freqmatr <- function(mytdm,rname)
{
  
  freqterm <- findFreqTerms(mytdm, lowfreq = (length(mytdm$ncol)/1000), highfreq = Inf)
=======
textclean <- function(x,rname)
{
  myCorpus <- x %>% VectorSource %>% Corpus
  myCorpus <- myCorpus %>% tm_map(removeWords,myStopwords) %>% tm_map(removeNumbers)
 
  mytdm <- myCorpus %>% TermDocumentMatrix
  freqterm <- findFreqTerms(mytdm, lowfreq = (length(x)/1000), highfreq = Inf)
>>>>>>> b5021e41b90ad74fbf1c3b4d4ad43ef0d4eb731a
  
  freqc <- mytdm[freqterm,]
  freqm <- spMatrix(nrow=freqc$ncol,ncol=freqc$nrow,i=freqc$j,j=freqc$i,x=freqc$v)
  colnames(freqm) <- freqterm
  rownames(freqm) <- rname
  #freqcount <- colSums(freqm)  
  #freq <- data.frame(freqterm,freqcount)
  #freqdf <-freq[order(freq$freqcount,decreasing=TRUE),]
  return(freqm)
}



balloonPlot = function(M, logTran, sqrtTran, main, namesx, namesy, margin, mult){
  # M is a matrix. want to plot each element as a dot.  
  # dot size corresponds to magnitude.
  # dot color black/red corresponds to sign.
  
  n = nrow(M)
  d = ncol(M)
  
  scaleItMean = mean(abs(M))
  
  if(logTran){
    scaleItMean = mean(log(abs(M) +1))
  }
  if(sqrtTran){
    scaleItMean = mean(sqrt(abs(M)))
  }
  par(mar = c(margin,margin,2,2))
  plot(c(.5,d+.5), c(.5,n+.5), col = "white", main = main, axes = F, ylab = "", xlab ="")
  if(length(namesx) !=F)  	axis(1, at = 1:d, labels = namesx, lty = 0, las = 2)
  if(length(namesy) !=F)    axis(2, at = n:1, labels = namesy, lty = 0, las = 2)
  
  
  for(i in 1:n){
    for(j in 1:d){
      dotSize = abs(M[i,j])/scaleItMean
      if(logTran) dotSize = log(dotSize + 1)/scaleItMean
      if(sqrtTran) dotSize = sqrt(dotSize)/scaleItMean
      dotColor = "black"
      if(M[i,j] < 0) dotColor = "red"
      points(j,n+1 - i, pch = 19, cex = dotSize*mult, col = dotColor)
      
    }
  }
}
