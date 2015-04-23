#make a data frame
index = 1:2120
candidate = c(rep("lepen", length(1:91)), rep("joly", length(92:114)),
              rep("bayrou",length(115:913)),rep("hollande",length(914:970)), 
              rep("melenchon",length(971:2024)),rep("dupont", length(2025:2043)),
              rep("sarkozy",length(2044:2105)),rep("poutou", length(2106:2120)))
table=data.frame(cbind(index,candidate))
table$index = as.integer(index)
View(table)

#take samples
x = matrix(rep(0,160),nrow=8)
rownames(x)=levels(table$candidate)
for(i in 1:length(levels(table$candidate))){
  set.seed(1)
  if(length(table$index[table$candidate==levels(table$candidate)[i]])>20){ #if there are more than 20 entries
    x[i,] = sample(size=20,x = table$index[table$candidate==levels(table$candidate)[i]])
  }else{ #if there are less then 20 entries, add each entry to the table and leave the remaining entries in the row as 0
    for(j in 1:length(table$index[table$candidate==levels(table$candidate)[i]])){
      x[i,j] = table$index[table$candidate==levels(table$candidate)[i]][j] 
    }
  }
}

#add info to data frame for the sampled posts
#Classification = 0 if candidate made the post
#Classification = 1 if someone else made the post
#Classification = 2 if the link was broken
table$classification=rep(500,length(table$index))
table$URL = rep(500,length(table$index))

#Bayrou
table$URL[327]="http://facebook.com/25134344760/posts/10150689942644761"
table$classification[327]=1

table$URL[411]="http://facebook.com/25134344760/posts/10150735000789761"
table$classification[411]=2

table$URL[571]="http://facebook.com/25134344760/posts/10150761022584761"
table$classification[571]= 1 

table$URL[837]="http://facebook.com/25134344760/posts/10150725790099761"
table$classification[837]=1

table$URL[275]="http://facebook.com/25134344760/posts/10150703979189761"
table$classification[275]=1

table$URL[828]="http://facebook.com/25134344760/posts/10150752046244761"
table$classification[828]=1  

table$URL[864]="http://facebook.com/25134344760/posts/10150761021234761"
table$classification[864]=1  

table$URL[638]="http://facebook.com/25134344760/posts/10150866635669761"
table$classification[638]=2  

table$URL[612]="http://facebook.com/25134344760/posts/10150704339569761"
table$classification[612]=1

table$URL[163]="http://facebook.com/25134344760/posts/10150744869934761"
table$classification[163]=2

table$URL[277]="http://facebook.com/25134344760/posts/194549423995298"
table$classification[277]=2

table$URL[x[1,12]]="http://facebook.com/25134344760/posts/10150725230449761"
table$classification[x[1,12]]=1  

table$URL[x[1,13]]="http://facebook.com/25134344760/posts/10150874557829761"
table$classification[x[1,13]]=2  

table$URL[x[1,14]]="http://facebook.com/25134344760/posts/10150756900094761"
table$classification[x[1,14]]=1

table$URL[x[1,15]]="http://facebook.com/25134344760/posts/196871537099227"
table$classification[x[1,15]]=1

table$URL[x[1,16]]="http://facebook.com/25134344760/posts/10150725623179761"
table$classification[x[1,16]]=1

table$URL[x[1,17]]="http://facebook.com/25134344760/posts/10150722849664761"
table$classification[x[1,17]]=2

table$URL[x[1,18]]="http://facebook.com/25134344760/posts/331929510194158"
table$classification[x[1,18]]=2

table$URL[x[1,19]]="http://facebook.com/25134344760/posts/225615857545655"
table$classification[x[1,19]]=1

table$URL[x[1,20]]="http://facebook.com/25134344760/posts/10150782966299761"
table$classification[x[1,20]]=1

#Dupont
table$URL[x[2,1]]= "http://facebook.com/90458392990/posts/274127609322157"
table$classification[x[2,1]]=0

table$URL[x[2,2]]="http://facebook.com/90458392990/posts/10150623348267991"
table$classification[x[2,2]]= 0

table$URL[x[2,3]]="http://facebook.com/90458392990/posts/2456741276231"
table$classification[x[2,3]]=2

table$URL[x[2,4]]="http://facebook.com/90458392990/posts/290131734392111"
table$classification[x[2,4]]=0

table$URL[x[2,5]]="http://facebook.com/90458392990/posts/380667181951857"
table$classification[x[2,5]]= 0

table$URL[x[2,6]]="https://www.facebook.com/90458392990/posts/188813774565040?_rdr"
table$classification[x[2,6]]= 0

table$URL[x[2,7]]="http://facebook.com/90458392990/posts/322113117838574"
table$classification[x[2,7]]= 0

table$URL[x[2,8]]=" http://facebook.com/90458392990/posts/130856173709796"
table$classification[x[2,8]]= 0

table$URL[x[2,9]]="http://facebook.com/90458392990/posts/10150570689912991"
table$classification[x[2,9]]= 2

table$URL[x[2,10]]="http://facebook.com/90458392990/posts/192915697487945"
table$classification[x[2,10]]= 0

table$URL[x[2,11]]="http://facebook.com/90458392990/posts/10150621853437991"
table$classification[x[2,11]]= 0

table$URL[x[2,12]]="http://facebook.com/90458392990/posts/196861073751544"
table$classification[x[2,12]]= 0

table$URL[x[2,13]]="http://facebook.com/90458392990/posts/10150620076442991"
table$classification[x[2,13]]=0

table$URL[x[2,14]]="http://facebook.com/90458392990/posts/10150618389197991"
table$classification[x[2,14]]=0

table$URL[x[2,15]]="http://facebook.com/90458392990/posts/10150623400782991"
table$classification[x[2,15]]=0

table$URL[x[2,16]]="http://facebook.com/90458392990/posts/10150612097152991"
table$classification[x[2,16]]=2

table$URL[x[2,17]]="http://facebook.com/90458392990/posts/10150608713947991"
table$classification[x[2,17]]=0

table$URL[x[2,18]]="http://facebook.com/90458392990/posts/10150563114747991"
table$classification[x[2,18]]=2

table$URL[x[2,19]]="http://facebook.com/90458392990/posts/203718543065303"
table$classification[x[2,19]]=2

#Hollande
table$URL[x[3,1]]="http://facebook.com/116439482501/posts/360808620613307"
table$classification[x[3,1]]=0

table$URL[x[3,2]]="http://facebook.com/116439482501/posts/10150712715417502"
table$classification[x[3,2]]= 0

table$URL[x[3,3]]="http://facebook.com/116439482501/posts/10150617215792502"
table$classification[x[3,3]]=2

table$URL[x[3,4]]="http://facebook.com/116439482501/posts/10150680674557502"
table$classification[x[3,4]]=0

table$URL[x[3,5]]="http://facebook.com/116439482501/posts/10150710144552502"
table$classification[x[3,5]]= 0

table$URL[x[3,6]]="http://facebook.com/116439482501/posts/10150722151602502"
table$classification[x[3,6]]= 0

table$URL[x[3,7]]="http://facebook.com/116439482501/posts/376126382399949"
table$classification[x[3,7]]= 0

table$URL[x[3,8]]="http://facebook.com/116439482501/posts/10150621855927502"
table$classification[x[3,8]]= 2

table$URL[x[3,9]]=" http://facebook.com/116439482501/posts/101024953356476"
table$classification[x[3,9]]= 0

table$URL[x[3,10]]="http://facebook.com/116439482501/posts/10150707664957502"
table$classification[x[3,10]]= 2

table$URL[x[3,11]]="http://facebook.com/116439482501/posts/334744413242296"
table$classification[x[3,11]]= 0

table$URL[x[3,12]]="http://facebook.com/116439482501/posts/10150723133967502"
table$classification[x[3,12]]= 0

table$URL[x[3,13]]="http://facebook.com/116439482501/posts/10150741612827502"
table$classification[x[3,13]]=2

table$URL[x[3,14]]="http://facebook.com/116439482501/posts/288495614556626"
table$classification[x[3,14]]=0

table$URL[x[3,15]]="http://facebook.com/116439482501/posts/10150728769447502"
table$classification[x[3,15]]=2

table$URL[x[3,16]]="http://facebook.com/116439482501/posts/10150739721702502"
table$classification[x[3,16]]=0

table$URL[x[3,17]]="http://facebook.com/116439482501/posts/264937246919665"
table$classification[x[3,17]]=0

table$URL[x[3,18]]="http://facebook.com/116439482501/posts/143156429139592"
table$classification[x[3,18]]=0

table$URL[x[3,19]]="http://facebook.com/116439482501/posts/10150728380962502"
table$classification[x[3,19]]=2

table$URL[x[3,20]]="http://facebook.com/116439482501/posts/320262788032668"
table$classification[x[3,20]]=0

#Joly
table$URL[x[4,1]]= "http://facebook.com/113472051997706/posts/388506404494268"
table$classification[x[4,1]]=2

table$URL[x[4,2]]="http://facebook.com/113472051997706/posts/195814167191093"
table$classification[x[4,2]]= 0

table$URL[x[4,3]]="http://facebook.com/113472051997706/posts/387471427931099"
table$classification[x[4,3]]=0

table$URL[x[4,4]]="http://facebook.com/113472051997706/posts/118454524946022"
table$classification[x[4,4]]=0

table$URL[x[4,5]]="http://facebook.com/113472051997706/posts/381278765217032"
table$classification[x[4,5]]= 0

table$URL[x[4,6]]="http://facebook.com/113472051997706/posts/203042896468758"
table$classification[x[4,6]]=0

table$URL[x[4,7]]="http://facebook.com/113472051997706/posts/385369904807918"
table$classification[x[4,7]]=0

table$URL[x[4,8]]="http://facebook.com/113472051997706/posts/387386687939573"
table$classification[x[4,8]]=2

table$URL[x[4,9]]="http://facebook.com/113472051997706/posts/252118818206144"
table$classification[x[4,9]]= 0

table$URL[x[4,10]]="http://facebook.com/113472051997706/posts/383369148341327"
table$classification[x[4,10]]= 2

table$URL[x[4,11]]="http://facebook.com/113472051997706/posts/350714974961315"
table$classification[x[4,11]]= 0

table$URL[x[4,12]]="http://facebook.com/113472051997706/posts/362349897119069"
table$classification[x[4,12]]=0

table$URL[x[4,13]]="http://facebook.com/113472051997706/posts/315037305217493"
table$classification[x[4,13]]=0

table$URL[x[4,14]]="http://facebook.com/113472051997706/posts/383338958344346"
table$classification[x[4,14]]=0

table$URL[x[4,15]]="http://facebook.com/113472051997706/posts/384628014882107"
table$classification[x[4,15]]=0

table$URL[x[4,16]]="http://facebook.com/113472051997706/posts/383209095023999"
table$classification[x[4,16]]=2

table$URL[x[4,17]]="http://facebook.com/113472051997706/posts/267739559970193"
table$classification[x[4,17]]=0

table$URL[x[4,18]]="http://facebook.com/113472051997706/posts/319860031394920"
table$classification[x[4,18]]=0

table$URL[x[4,19]]="http://facebook.com/113472051997706/posts/390060927672149"
table$classification[x[4,19]]=0

table$URL[x[4,20]]="http://facebook.com/113472051997706/posts/379946015350307"
table$classification[x[4,20]]=2

#Lepen
table$URL[x[5,1]]="http://facebook.com/123316887684644/posts/379982748684722"
table$classification[x[5,1]]=2

table$URL[x[5,2]]="http://facebook.com/123316887684644/posts/403615942988069"
table$classification[x[5,2]]= 0

table$URL[x[5,3]]="http://facebook.com/123316887684644/posts/386633181347423"
table$classification[x[5,3]]= 0

table$URL[x[5,4]]="http://facebook.com/123316887684644/posts/386992411317089"
table$classification[x[5,4]]=0

table$URL[x[5,5]]="http://facebook.com/123316887684644/posts/403618956321101"
table$classification[x[5,5]]= 0

table$URL[x[5,6]]="http://facebook.com/123316887684644/posts/307246119328714"
table$classification[x[5,6]]= 0

table$URL[x[5,7]]="http://facebook.com/123316887684644/posts/109622742501446"
table$classification[x[5,7]]=0

table$URL[x[5,8]]="http://facebook.com/123316887684644/posts/201525806621429"
table$classification[x[5,8]]= 0

table$URL[x[5,9]]="http://facebook.com/123316887684644/posts/391567310859599"
table$classification[x[5,9]]= 0

table$URL[x[5,10]]="http://facebook.com/123316887684644/posts/403806772968986"
table$classification[x[5,10]]= 0

table$URL[x[5,11]]="http://facebook.com/123316887684644/posts/403613572988306"
table$classification[x[5,11]]= 0

table$URL[x[5,12]]="http://facebook.com/123316887684644/posts/380389721977358"
table$classification[x[5,12]]= 2

table$URL[x[5,13]]="http://facebook.com/123316887684644/posts/381946661821664"
table$classification[x[5,13]]=2

table$URL[x[5,14]]="http://facebook.com/123316887684644/posts/401547756528221"
table$classification[x[5,14]]=2

table$URL[x[5,15]]="http://facebook.com/123316887684644/posts/177508142365609"
table$classification[x[5,15]]=0

table$URL[x[5,16]]="http://facebook.com/123316887684644/posts/382206661795664"
table$classification[x[5,16]]=0

table$URL[x[5,17]]="http://facebook.com/123316887684644/posts/338189022884876"
table$classification[x[5,17]]=0

table$URL[x[5,18]]="http://facebook.com/123316887684644/posts/377315565618107"
table$classification[x[5,18]]=0

table$URL[x[5,19]]="http://facebook.com/123316887684644/posts/339016886141876"
table$classification[x[5,19]]=0

table$URL[x[5,20]]="http://facebook.com/123316887684644/posts/204212243019310"
table$classification[x[5,20]]=0


#Melenchon
table$URL[x[6,1]]="http://facebook.com/11450328749/posts/10150803983213750"
table$classification[x[6,1]]=1

table$URL[x[6,2]]="http://facebook.com/11450328749/posts/10150808716018750"
table$classification[x[6,2]]= 1

table$URL[x[6,3]]="http://facebook.com/11450328749/posts/3970871709392"
table$classification[x[6,3]]=2

table$URL[x[6,4]]="http://facebook.com/11450328749/posts/3970871709392"
table$classification[x[6,4]]=2

table$URL[x[6,5]]="http://facebook.com/11450328749/posts/165549153573857"
table$classification[x[6,5]]= 1

table$URL[x[6,6]]="http://facebook.com/11450328749/posts/10150832962698750"
table$classification[x[6,6]]= 1

table$URL[x[6,7]]="http://facebook.com/11450328749/posts/247004032074550"
table$classification[x[6,7]]= 1

table$URL[x[6,8]]="http://facebook.com/11450328749/posts/10150824940413750"
table$classification[x[6,8]]= 1

table$URL[x[6,9]]="http://facebook.com/11450328749/posts/106744236129622"
table$classification[x[6,9]]= 2

table$URL[x[6,10]]="http://facebook.com/11450328749/posts/360598370664211"
table$classification[x[6,10]]= 2

table$URL[x[6,11]]="http://facebook.com/11450328749/posts/196270450493935"
table$classification[x[6,11]]= 2

table$URL[x[6,12]]="http://facebook.com/11450328749/posts/10150798865113750"
table$classification[x[6,12]]= 1

table$URL[x[6,13]]="http://facebook.com/11450328749/posts/10150824876023750"
table$classification[x[6,13]]=1

table$URL[x[6,14]]="http://facebook.com/11450328749/posts/10150808556363750"
table$classification[x[6,14]]=2

table$URL[x[6,15]]="http://facebook.com/11450328749/posts/10150826830673750"
table$classification[x[6,15]]=1

table$URL[x[6,16]]="http://facebook.com/11450328749/posts/125798014221554"
table$classification[x[6,16]]=1

table$URL[x[6,17]]="http://facebook.com/11450328749/posts/10150826176498750"
table$classification[x[6,17]]=2

table$URL[x[6,18]]="http://facebook.com/11450328749/posts/10150840401548750"
table$classification[x[6,18]]=2

table$URL[x[6,19]]="http://facebook.com/11450328749/posts/10150808383183750"
table$classification[x[6,19]]=2

table$URL[x[6,20]]="http://facebook.com/11450328749/posts/10150829020943750"
table$classification[x[6,20]]=2



#Poutou
table$URL[x[7,1]]="http://facebook.com/180203352032870/posts/237316986368593"
table$classification[x[7,1]]=2

table$URL[x[7,2]]="http://facebook.com/180203352032870/posts/443092152373846"
table$classification[x[7,2]]= 2

table$URL[x[7,3]]="http://facebook.com/180203352032870/posts/283337661719438"
table$classification[x[7,3]]=0

table$URL[x[7,4]]="http://facebook.com/180203352032870/posts/249168878502012"
table$classification[x[7,4]]=0

table$URL[x[7,5]]="http://facebook.com/180203352032870/posts/315114628544719"
table$classification[x[7,5]]= 0

table$URL[x[7,6]]="http://facebook.com/180203352032870/posts/305629199490638"
table$classification[x[7,6]]= 0

table$URL[x[7,7]]="http://facebook.com/180203352032870/posts/340239149362622"
table$classification[x[7,7]]= 2

table$URL[x[7,8]]="http://facebook.com/180203352032870/posts/341176995935504"
table$classification[x[7,8]]= 2

table$URL[x[7,9]]="http://facebook.com/180203352032870/posts/223209694461926"
table$classification[x[7,9]]= 2

table$URL[x[7,10]]="http://facebook.com/180203352032870/posts/373738662663143"
table$classification[x[7,10]]= 2

table$URL[x[7,11]]="http://facebook.com/180203352032870/posts/342045232515347"
table$classification[x[7,11]]= 2

table$URL[x[7,12]]="http://facebook.com/180203352032870/posts/342170095836194"
table$classification[x[7,12]]= 2

table$URL[x[7,13]]="http://facebook.com/180203352032870/posts/276545729080765"
table$classification[x[7,13]]=2

table$URL[x[7,14]]="http://facebook.com/180203352032870/posts/327390017324392"
table$classification[x[7,14]]=2

table$URL[x[7,15]]="http://facebook.com/180203352032870/posts/272290156183588"
table$classification[x[7,15]]=2

#Sarkozy
table$URL[x[8,1]]="http://facebook.com/7766361077/posts/10150491414576078"
table$classification[x[8,1]]=0

table$URL[x[8,2]]="http://facebook.com/7766361077/posts/10150465354676078"
table$classification[x[8,2]]= 2

table$URL[x[8,3]]="http://facebook.com/7766361077/posts/125993217526414"
table$classification[x[8,3]]=0

table$URL[x[8,4]]="http://facebook.com/7766361077/posts/10150577950021078"
table$classification[x[8,4]]=0

table$URL[x[8,5]]="http://facebook.com/7766361077/posts/10150594945911078"
table$classification[x[8,5]]= 0

table$URL[x[8,6]]="http://facebook.com/7766361077/posts/10150548538326078"
table$classification[x[8,6]]= 0

table$URL[x[8,7]]="http://facebook.com/7766361077/posts/10150582313891078"
table$classification[x[8,7]]= 2

table$URL[x[8,8]]="http://facebook.com/7766361077/posts/310948205627563"
table$classification[x[8,8]]= 0

table$URL[x[8,9]]="http://facebook.com/7766361077/posts/249681425119654"
table$classification[x[8,9]]= 0

table$URL[x[8,10]]="http://facebook.com/7766361077/posts/10150581540856078"
table$classification[x[8,10]]= 0

table$URL[x[8,11]]="http://facebook.com/7766361077/posts/10150590528916078"
table$classification[x[8,11]]= 0

table$URL[x[8,12]]="http://facebook.com/7766361077/posts/10150566619566078"
table$classification[x[8,12]]= 2

table$URL[x[8,13]]="http://facebook.com/7766361077/posts/247811201969082"
table$classification[x[8,13]]=0

table$URL[x[8,14]]="http://facebook.com/7766361077/posts/10150455072351078"
table$classification[x[8,14]]=0

table$URL[x[8,15]]="http://facebook.com/7766361077/posts/10150579814356078"
table$classification[x[8,15]]=0

table$URL[x[8,16]]="http://facebook.com/7766361077/posts/10150453627581078"
table$classification[x[8,16]]=2

table$URL[x[8,17]]="http://facebook.com/7766361077/posts/10150560471321078"
table$classification[x[8,17]]=0

table$URL[x[8,18]]="http://facebook.com/7766361077/posts/257885510952606"
table$classification[x[8,18]]=0

table$URL[x[8,19]]="http://facebook.com/7766361077/posts/385238521487935"
table$classification[x[8,19]]=0

table$URL[x[8,20]]="http://facebook.com/7766361077/posts/10150573331841078"
table$classification[x[8,20]]=2

tableSample = table[table$URL!=500,]
