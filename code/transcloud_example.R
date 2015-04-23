terms<-list(I=c("i","i'm"),mal=qcv(stinks,dumb,distrust),articles=qcv(the,a,an),pronoun=qcv(we,you))
with(DATA,trans_cloud(state,person),target.words=terms,
     stopwords=exlude(with(DATA,unique(bag.o.words(state))),unique(unlist(terms))),
     cloud.colors=qcv(red, green, blue, balck, gray65),
     exand.target=FALSE,proportional=TRUE,legend=c(names(terms)))
options(device="windows")


library(XML)
library(RCurl)
library(devtools)
library(qdap)
url_dl("pres.deb1.docx")  #downloads a docx file of the debate to wd
# the read.transcript function allows reading in of docx file 
# special thanks to Bryan Goodrich for his work on this
dat <- read.transcript("pres_deb1.docx", col.names=c("person", "dialogue"))
truncdf(dat)
left.just(dat)
dat$dialogue <- bracketX(dat$dialogue)  #removes brackets (non dialogue)
dat$dialogue <- symbol_change(dat$dialogue)  #changes symbols to words (ie % = percent)
dat$dialogue <- num_replace(dat$dialogue)  #changes numerbers to word form (compliments of John Fox)
dat$dialogue <- scrubber(gsub("-", " ", dat$dialogue)) #removes dashes
# sentSplit splits turns of talk into sentences
# special thanks to Dason Kurkiewicz for his work on this
dat2 <- sentSplit(dat, "dialogue", stem.col=FALSE)  
htruncdf(dat2)   #view a truncated version of the data (see also truncdf)

#first put a unique character between words we want to keep together
dat2$dia2 <- mgsub(c("Governor Romney", "President Obama", "middle class"), 
                   c("Governor~Romney", "President~Obama", "middle~class"), dat2$dialogue)
#the word cloud by grouping variable function
with(dat2, trans_cloud(dia2, person, proportional = TRUE,
                       target.words = list(health=c("health", "insurance", "medic", "obamacare", "hospital"), 
                                           economic = c("econom", "jobs", "unemploy", "business", "banks", 
                                                        "budget", "market", "paycheck"),
                                           foreign = c("war ", "terror", "foreign"),
                                           class = c("middle~class", "poor", "rich"),
                                           oponent = c("romney ", "obama")),
                       cloud.colors = c("red", "blue", "black", "orange", "purple", "gray45"),
                       legend = c("health", "economic", "foreign", "class", "oponent"),
                       stopwords=exclude(Top25Words, "he", "I"), char2space = "~"))

