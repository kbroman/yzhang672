#' take out the substring of the url of "https://www.facebook.com/123456789/posts/12345678"
#' @param str the complete string of url
#' @export
#' @return the substring of url
#' @keywords string substring


drop = function(str){
  sstr = substr(str, start = 25, stop = nchar(str))
  return(sstr)  
}

#' create adjacency matrix
#' @param x the subjects to construct rows 
#' @param y the subjects to construct columns
#' @export
#' @return the adjacency matrix of x and y
#' @keywords adjacency matrix 

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
  
  rownames(A) = ux
  
  return(A)
}

#' create the matrix of all users by words
#' users are the row subjects
#' words are the words post by users
#' @param A adjacency matrix of users by posts
#' @export
#' @return the matrix of all users by words
#' @keywords adjacency matrix 


create_fanwords <- function(A)
{
  fan = A %>% rownames %>% unique
  cfan = match(A %>% rownames,fan)
  nx=length(fan);ny=length(A[1,])
  
  B = spMatrix(nrow = nx , ncol = ny, 
               i = cfan[A@i+1]  , j = A@j+1 , x = A@x ) 
  rownames(B) = fan
  colnames(B) = colnames(A)
  return(B)
}

#' combine two sparse matrix
#' @param A1 sparse matrix 1
#' @param A2 sparse matrix 2
#' @export
#' @return the large matrix which is the combination of A1 and A2
#' @keywords combine sparse matrix


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

#' Do SVD to matrix with normalization
#' @param A adjacency matrix
#' @export
#' @return top 20 singular values and singular vectors
#' @keywords SVD normalization

svdmatrix <- function(A)
{
  rs=rowSums(A);cs=colSums(A)
  L = Diagonal(length(rs),1/sqrt(rs + mean(rs)))%*%
    A%*%Diagonal(length(cs),1/sqrt(cs + mean(cs)))
  sf=irlba(L,nu=20,nv=20)
  return(sf)
}

#' do svd sorted by time
#' @param v data to do SVD
#' @param datem time of data
#' @param cc membership vector of data (which post belongs to which candidate)
#' @param npost number of posts
#' @param ncluster number of clusters

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

#' clean text
#' remove stopwords
#' remove numbers
#' @param x original text
#' @export
#' @return TermDocumentMatrix after cleaning
#' @keywords text clean TermDocumentMatrix

textclean <- function (x)
{
  myStopwords <- c(stopwords("French"),
                   "http","www","facebook","com","le")
  myCorpus <- x %>% VectorSource %>% Corpus
  myCorpus <- myCorpus %>% tm_map(removeWords,myStopwords) %>% tm_map(removeNumbers)
  mytdm <- myCorpus %>% TermDocumentMatrix
  return(mytdm)
}

#' find frequent words in TermDocumenMatrix
#' @param mytdm the TermDocumentMatrix
#' @param rname rownames of the matrix
#' @export 
#' @return matrix of word frequency

freqmatr <- function(mytdm,rname)
{
  
  freqterm <- findFreqTerms(mytdm, lowfreq = (length(mytdm$ncol)/1000), highfreq = Inf)
  
  textclean <- function(x,rname)
  {
    myCorpus <- x %>% VectorSource %>% Corpus
    myCorpus <- myCorpus %>% tm_map(removeWords,myStopwords) %>% tm_map(removeNumbers)
    
    mytdm <- myCorpus %>% TermDocumentMatrix
    freqterm <- findFreqTerms(mytdm, lowfreq = (length(x)/1000), highfreq = Inf)
    
    freqc <- mytdm[freqterm,]
    freqm <- spMatrix(nrow=freqc$ncol,ncol=freqc$nrow,i=freqc$j,j=freqc$i,x=freqc$v)
    colnames(freqm) <- freqterm
    rownames(freqm) <- rname
    return(freqm)
  }
}
  
#' draw balloon plot
#' @param M data
#' @param logTran do log transformation or not
#' @param sqrtTran do squareroot transformation or not
#' @param main name of this balloon plot
#' @param namesx column names
#' @param namesy row names
#' @param margin number of margin of this balloon plot
#' @param mult number of repetition of this balloon plot
#' @export
#' @return a balloon Plot

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
  if(length(namesx) !=F)    axis(1, at = 1:d, labels = namesx, lty = 0, las = 2)
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


