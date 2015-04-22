library(XML)
library(RCurl)
library(devtools)
library(qdap)
library(stringr)
library(tm)
library(RWeka)
library(wordcloud)
load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')

nwords=100
#iconv(stopwords("French"),"latin1","ASCII",sub="")
myStopwords <- c(stopwords("French"),
                 "http","www","facebook","com","le")

#lepen
fancomments=tables$lepe$text
fancomments=str_replace_all(fancomments, "[^[:alnum:]]", " ")
fancomments=iconv(fancomments, "latin1", "ASCII", sub="")
#a=bracketX(fancomments)

myCorpus <- Corpus(VectorSource(fancomments))
#myCorpus <- tm_map(myCorpus,tolower)
#frenchstopwords=read.table("french_stopwords.txt")
#frenchstopwords=as.character(frenchstopwords)


myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, removeNumbers)

#mytdm=TermDocumentMatrix(myCorpus)
#str(mytdm)
#freqterm=findFreqTerms(mytdm, lowfreq = 10, highfreq = Inf)
#str(freqterm)
#freqterm

dataframe<-data.frame(text=unlist(sapply(myCorpus, "content")), 
                      stringsAsFactors=FALSE)
fancomments=dataframe$text
candidate=rep("lepen",length(fancomments))
fan_lepe=data.frame(candidate,fancomments)
fan_lepe$fancomments=as.character(fan_lepe$fancomments)

lepe_fwl=with(fan_lepe, word_list(fancomments, candidate,cut.n=nwords))

#with(fan_lepe,trans_cloud(fancomments,candidate,proportional=TRUE))
#options(device="windows")
save(lepe_fwl, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/lepe_fanworldlist.RData")


#joly
fancomments=tables$joly$text
fancomments=str_replace_all(fancomments, "[^[:alnum:]]", " ")
fancomments=iconv(fancomments, "latin1", "ASCII", sub="")

myCorpus <- Corpus(VectorSource(fancomments))
#myCorpus <- tm_map(myCorpus,tolower)
#frenchstopwords=read.table("french_stopwords.txt")
#frenchstopwords=as.character(frenchstopwords)
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, removeNumbers)

#mytdm=TermDocumentMatrix(myCorpus)
#str(mytdm)
#freqterm=findFreqTerms(mytdm, lowfreq = 10, highfreq = Inf)
#str(freqterm)
#freqterm

dataframe<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), 
                      stringsAsFactors=FALSE)
fancomments=dataframe$text
candidate=rep("joly",length(fancomments))
fan_joly=data.frame(candidate,fancomments)
fan_joly$fancomments=as.character(fan_joly$fancomments)

joly_fwl=with(fan_joly, word_list(fancomments, candidate,cut.n=nwords))

#with(fan_joly,trans_cloud(fancomments,candidate,proportional=TRUE))
#options(device="windows")
save(joly_fwl, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/joly_fanworldlist.RData")

#bayrou
fancomments=tables$bayr$text
fancomments=str_replace_all(fancomments, "[^[:alnum:]]", " ")
fancomments=iconv(fancomments, "latin1", "ASCII", sub="")

myCorpus <- Corpus(VectorSource(fancomments))
#myCorpus <- tm_map(myCorpus,tolower)
#frenchstopwords=read.table("french_stopwords.txt")
#frenchstopwords=as.character(frenchstopwords)
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, removeNumbers)

#mytdm=TermDocumentMatrix(myCorpus)
#str(mytdm)
#freqterm=findFreqTerms(mytdm, lowfreq = 10, highfreq = Inf)
#str(freqterm)
#freqterm

dataframe<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), 
                      stringsAsFactors=FALSE)
fancomments=dataframe$text
candidate=rep("bayrou",length(fancomments))
fan_bayr=data.frame(candidate,fancomments)
fan_bayr$fancomments=as.character(fan_bayr$fancomments)

bayr_fwl=with(fan_bayr, word_list(fancomments, candidate,cut.n=nwords))

#with(fan_bayr,trans_cloud(fancomments,candidate,proportional=TRUE))
#options(device="windows")
save(bayr_fwl, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/bayr_fanworldlist.RData")

#hollande
fancomments=tables$holl$text
fancomments=str_replace_all(fancomments, "[^[:alnum:]]", " ")
fancomments=iconv(fancomments, "latin1", "ASCII", sub="")

myCorpus <- Corpus(VectorSource(fancomments))
#myCorpus <- tm_map(myCorpus,tolower)
#frenchstopwords=read.table("french_stopwords.txt")
#frenchstopwords=as.character(frenchstopwords)
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, removeNumbers)

#mytdm=TermDocumentMatrix(myCorpus)
#str(mytdm)
#freqterm=findFreqTerms(mytdm, lowfreq = 10, highfreq = Inf)
#str(freqterm)
#freqterm

dataframe<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), 
                      stringsAsFactors=FALSE)
fancomments=dataframe$text
candidate=rep("hollande",length(fancomments))
fan_holl=data.frame(candidate,fancomments)
fan_holl$fancomments=as.character(fan_holl$fancomments)

holl_fwl=with(fan_holl, word_list(fancomments, candidate,cut.n=nwords))

#with(fan_holl,trans_cloud(fancomments,candidate,proportional=TRUE))
#options(device="windows")
save(holl_fwl, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/holl_fanworldlist.RData")

#melechon
fancomments=tables$mele$text
fancomments=str_replace_all(fancomments, "[^[:alnum:]]", " ")
fancomments=iconv(fancomments, "latin1", "ASCII", sub="")

myCorpus <- Corpus(VectorSource(fancomments))
#myCorpus <- tm_map(myCorpus,tolower)
#frenchstopwords=read.table("french_stopwords.txt")
#frenchstopwords=as.character(frenchstopwords)
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, removeNumbers)

#mytdm=TermDocumentMatrix(myCorpus)
#str(mytdm)
#freqterm=findFreqTerms(mytdm, lowfreq = 10, highfreq = Inf)
#str(freqterm)
#freqterm

dataframe<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), 
                      stringsAsFactors=FALSE)
fancomments=dataframe$text
candidate=rep("melenchon",length(fancomments))
fan_mele=data.frame(candidate,fancomments)
fan_mele$fancomments=as.character(fan_mele$fancomments)

mele_fwl=with(fan_mele, word_list(fancomments, candidate,cut.n=nwords))

#with(fan_mele,trans_cloud(fancomments,candidate,proportional=TRUE))
#options(device="windows")
save(mele_fwl, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/mele_fanworldlist.RData")


#dupont
fancomments=tables$dupo$text
fancomments=str_replace_all(fancomments, "[^[:alnum:]]", " ")
fancomments=iconv(fancomments, "latin1", "ASCII", sub="")

myCorpus <- Corpus(VectorSource(fancomments))
#myCorpus <- tm_map(myCorpus,tolower)
#frenchstopwords=read.table("french_stopwords.txt")
#frenchstopwords=as.character(frenchstopwords)
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, removeNumbers)

#mytdm=TermDocumentMatrix(myCorpus)
#str(mytdm)
#freqterm=findFreqTerms(mytdm, lowfreq = 10, highfreq = Inf)
#str(freqterm)
#freqterm

dataframe<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), 
                      stringsAsFactors=FALSE)
fancomments=dataframe$text
candidate=rep("dupont",length(fancomments))
fan_dupo=data.frame(candidate,fancomments)
fan_dupo$fancomments=as.character(fan_dupo$fancomments)

dupo_fwl=with(fan_dupo, word_list(fancomments, candidate,cut.n=nwords))

#with(fan_dupo,trans_cloud(fancomments,candidate,proportional=TRUE))
#options(device="windows")
save(dupo_fwl, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/dupo_fanworldlist.RData")

#sarkozy
fancomments=tables$sark$text
fancomments=str_replace_all(fancomments, "[^[:alnum:]]", " ")
fancomments=iconv(fancomments, "latin1", "ASCII", sub="")

myCorpus <- Corpus(VectorSource(fancomments))
#myCorpus <- tm_map(myCorpus,tolower)
#frenchstopwords=read.table("french_stopwords.txt")
#frenchstopwords=as.character(frenchstopwords)
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, removeNumbers)

#mytdm=TermDocumentMatrix(myCorpus)
#str(mytdm)
#freqterm=findFreqTerms(mytdm, lowfreq = 10, highfreq = Inf)
#str(freqterm)
#freqterm

dataframe<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), 
                      stringsAsFactors=FALSE)
fancomments=dataframe$text
candidate=rep("sarkozy",length(fancomments))
fan_sark=data.frame(candidate,fancomments)
fan_sark$fancomments=as.character(fan_sark$fancomments)

sark_fwl=with(fan_sark, word_list(fancomments, candidate,cut.n=nwords))

#with(fan_sark,trans_cloud(fancomments,candidate,proportional=TRUE))
#options(device="windows")
save(sark_fwl, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/sark_fanworldlist.RData")

#poutou
fancomments=tables$pout$text
fancomments=str_replace_all(fancomments, "[^[:alnum:]]", " ")
fancomments=iconv(fancomments, "latin1", "ASCII", sub="")

myCorpus <- Corpus(VectorSource(fancomments))
#myCorpus <- tm_map(myCorpus,tolower)
#frenchstopwords=read.table("french_stopwords.txt")
#frenchstopwords=as.character(frenchstopwords)
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, removeNumbers)

#mytdm=TermDocumentMatrix(myCorpus)
#str(mytdm)
#freqterm=findFreqTerms(mytdm, lowfreq = 10, highfreq = Inf)
#str(freqterm)
#freqterm

dataframe<-data.frame(text=unlist(sapply(myCorpus, "content")), 
                      stringsAsFactors=FALSE)
fancomments=dataframe$text
candidate=rep("poutou",length(fancomments))
fan_pout=data.frame(candidate,fancomments)
fan_pout$fancomments=as.character(fan_pout$fancomments)

pout_fwl=with(fan_pout, word_list(fancomments, candidate,cut.n=nwords))

#with(fan_pout,trans_cloud(fancomments,candidate,proportional=TRUE))
#options(device="windows")
save(pout_fwl, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/pout_fanworldlist.RData")


load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/lepe_fanworldlist.RData")
