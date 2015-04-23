
library(Matrix)
library(XML);library(RCurl);library(devtools);library(qdap)
library(stringr);library(tm);library(RWeka);library(wordcloud)

load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')
lepe = tables$lepen
joly = tables$joly
bayr = tables$bayrou
holl = tables$hollande
mele = tables$melenchon
dupo = tables$dupont
sark = tables$sarkozy
pout = tables$poutou

allcomments = c(lepe$text,joly$text,bayr$text,holl$text,
                mele$text,dupo$text,sark$text,pout$text)

alluniquefans=unique(c(lepe$fan_id,joly$fan_id,bayr$fan_id,holl$fan_id,
                       mele$fan_id,dupo$fan_id,sark$fan_id,pout$fan_id))
#allfanwords = textclean(allcomments)
#save(allfanwords, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords.RData")

freq_lepe = textclean(lepe$text,lepe$fan_id);
freq_joly = textclean(joly$text,joly$fan_id);
freq_bayr = textclean(bayr$text,bayr$fan_id);
freq_holl = textclean(holl$text,holl$fan_id);
freq_mele = textclean(mele$text,mele$fan_id);
freq_dupo = textclean(dupo$text,dupo$fan_id);
freq_sark = textclean(sark$text,sark$fan_id);
freq_pout = textclean(pout$text,pout$fan_id)

save(freq_lepe, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_lepe.RData")
save(freq_joly, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_joly.RData")
save(freq_bayr, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_bayr.RData")
save(freq_holl, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_holl.RData")
save(freq_mele, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_mele.RData")
save(freq_dupo, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_dupo.RData")
save(freq_sark, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_sark.RData")
save(freq_pout, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_pout.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_lepe.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_joly.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_bayr.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_holl.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_mele.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_dupo.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_sark.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/freq_pout.RData")

fanwords_lepe <- create_fanwords(freq_lepe)
fanwords_joly <- create_fanwords(freq_joly)
fanwords_bayr <- create_fanwords(freq_bayr)
fanwords_holl <- create_fanwords(freq_holl)
fanwords_mele <- create_fanwords(freq_mele)
fanwords_dupo <- create_fanwords(freq_dupo)
fanwords_sark <- create_fanwords(freq_sark)
fanwords_pout <- create_fanwords(freq_pout)

save(fanwords_lepe, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_lepe.RData")
save(fanwords_joly, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_joly.RData")
save(fanwords_bayr, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_bayr.RData")
save(fanwords_holl, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_holl.RData")
save(fanwords_mele, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_mele.RData")
save(fanwords_dupo, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_dupo.RData")
save(fanwords_sark, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_sark.RData")
save(fanwords_pout, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_pout.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_lepe.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_joly.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_bayr.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_holl.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_mele.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_dupo.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_sark.RData")
load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/fanwords_pout.RData")

allfanwords1 <- fanwords_pout %>% combind_fanwords(fanwords_joly) 
allfanwords2 <-  allfanwords1 %>% combind_fanwords(fanwords_dupo)
allfanwords3 <-  allfanwords2 %>% combind_fanwords(fanwords_bayr)
save(allfanwords3, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords3.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords3.RData")
allfanwords4 <-  allfanwords3 %>% combind_fanwords(fanwords_mele) 
allfanwords5 <-  allfanwords4 %>% combind_fanwords(fanwords_lepe) 
save(allfanwords5, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords5.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords5.RData")
allfanwords6 <-  allfanwords5 %>% combind_fanwords(fanwords_sark) 
save(allfanwords6, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords6.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords6.RData")
allfanwords  <-  allfanwords6 %>% combind_fanwords(fanwords_holl)

save(allfanwords, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/allfanwords.RData")





