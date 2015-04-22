load('F:/UWMadison/project/French candidates/previous work/fordesk00/mydata/tables.RData')
lepe = tables$lepen
joly = tables$joly
bayr = tables$bayrou
holl = tables$hollande
mele = tables$melenchon
dupo = tables$dupont
sark = tables$sarkozy
pout = tables$poutou
n=rep(0,8)
n[1]=length(unique(lepe$parent_url));n[2]=length(unique(joly$parent_url));
n[3]=length(unique(bayr$parent_url));n[4]=length(unique(holl$parent_url));
n[5]=length(unique(mele$parent_url));n[6]=length(unique(dupo$parent_url));
n[7]=length(unique(sark$parent_url));n[8]=length(unique(pout$parent_url));
n
sum(n)
cc = c()
for(i in 1:length(n)) cc = c(cc, rep(i,n[i]))

m=rep(0,8)
m[1]=length(unique(lepe$fan_id));m[2]=length(unique(joly$fan_id));
m[3]=length(unique(bayr$fan_id));m[4]=length(unique(holl$fan_id));
m[5]=length(unique(mele$fan_id));m[6]=length(unique(dupo$fan_id));
m[7]=length(unique(sark$fan_id));m[8]=length(unique(pout$fan_id));
m

ma=rep(0,8)
ma[1]=length(lepe$fan_id);ma[2]=length(joly$fan_id);
ma[3]=length(bayr$fan_id);ma[4]=length(holl$fan_id);
ma[5]=length(mele$fan_id);ma[6]=length(dupo$fan_id);
ma[7]=length(sark$fan_id);ma[8]=length(pout$fan_id);
ma

save(cc, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cc.RData")
save(n, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/n.RData")

ncom=rep(0,8)
ncom[1]=length(lepe$parent_url);ncom[2]=length(joly$parent_url);
ncom[3]=length(bayr$parent_url);ncom[4]=length(holl$parent_url);
ncom[5]=length(mele$parent_url);ncom[6]=length(dupo$parent_url);
ncom[7]=length(sark$parent_url);ncom[8]=length(pout$parent_url);
ncom
sum(ncom)

# or
#ncom[1]=length(lepe$text);ncom[2]=length(joly$text);
#ncom[3]=length(bayr$text);ncom[4]=length(holl$text);
#ncom[5]=length(mele$text);ncom[6]=length(dupo$text);
#ncom[7]=length(sark$text);ncom[8]=length(pout$text);
#ncom
save(ncom, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ncom.RData")


ffcc=rep(0,8)
ffcc[1]=length(unique(lepe$fan_id))
ffcc[2]=length(unique(c(lepe$fan_id,joly$fan_id)))
ffcc[3]=length(unique(c(lepe$fan_id,joly$fan_id,bayr$fan_id)))
ffcc[4]=length(unique(c(lepe$fan_id,joly$fan_id,bayr$fan_id,holl$fan_id)))
ffcc[5]=length(unique(c(lepe$fan_id,joly$fan_id,bayr$fan_id,holl$fan_id,mele$fan_id)))
ffcc[6]=length(unique(c(lepe$fan_id,joly$fan_id,bayr$fan_id,holl$fan_id,mele$fan_id,dupo$fan_id)))
ffcc[7]=length(unique(c(lepe$fan_id,joly$fan_id,bayr$fan_id,holl$fan_id,mele$fan_id,dupo$fan_id,sark$fan_id)))
ffcc[8]=length(unique(c(lepe$fan_id,joly$fan_id,bayr$fan_id,holl$fan_id,mele$fan_id,dupo$fan_id,sark$fan_id,pout$fan_id)))
save(ffcc, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ffcc.RData")

cadg=rep(0,8)
cadg[1]=length(lepe$fan_id)
cadg[2]=length(joly$fan_id)
cadg[3]=length(bayr$fan_id)
cadg[4]=length(holl$fan_id)
cadg[5]=length(mele$fan_id)
cadg[6]=length(dupo$fan_id)
cadg[7]=length(sark$fan_id)
cadg[8]=length(pout$fan_id)
cadg
save(cadg, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cadg.RData")

cafa=rep(0,8)
cafa[1]=length(unique(lepe$fan_id))
cafa[2]=length(unique(joly$fan_id))
cafa[3]=length(unique(bayr$fan_id))
cafa[4]=length(unique(holl$fan_id))
cafa[5]=length(unique(mele$fan_id))
cafa[6]=length(unique(dupo$fan_id))
cafa[7]=length(unique(sark$fan_id))
cafa[8]=length(unique(pout$fan_id))
cafa
save(cafa, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cafa.RData")

load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/cc.RData")
ccdrop=cc[wc] #wc in svddroppostfans (create allfansdrop part)
length(ccdrop)
save(ccdrop, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ccdrop.RData")

ndrop=rep(0,8)
for (i in 1:8)
{ndrop[i]=sum(ccdrop==i)}
save(ndrop, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ndrop.RData")


load("C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ffcc.RData")
ffccdrop=rep(0,8);dropleth=rep(0,8)
dropleth[1]=length(drop1);dropleth[2]=length(drop2);
dropleth[3]=length(drop3);dropleth[4]=length(drop4);
dropleth[5]=length(drop5);dropleth[6]=length(drop6);
dropleth[7]=length(drop7);dropleth[8]=length(drop8);
for (i in 1:8)
{ffccdrop[i]=ffcc[i]-sum(dropleth[1:i])}
save(ffccdrop, file = "C:/Users/Administrator/Dropbox/my project/frenchFacebook/data/ffccdrop.RData")


