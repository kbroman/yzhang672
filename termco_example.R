# termco examples:
  data=raj.act.1[1:50,]
  term <- c("the ", "she", " wh")
  (out <- with(data,  termco(dialogue, person, term)))
  plot(out)
  
 with(data,trans_cloud(dialogue, person))
  
  scores(out)
  plot(scores(out))
  counts(out)
  plot(counts(out))
  proportions(out)
  plot(proportions(out))
  
# General form for match.list as themes
   
    ml <- list(
        cat1 = c(),
        cat2 = c(),
        catn = c()
    )
  
  ml <- list(
      cat1 = c(" the ", " a ", " an "),
      cat2 = c(" I'" ),
      "good",
      the = c("the", " the ", " the", "the")
  )
  
  (dat <- with(raj.act.1,  termco(dialogue, person, ml)))
  scores(dat)  # useful for presenting in tables
  counts(dat)  # prop and raw counts are useful for performing calculations
  proportions(dat)
  datb <- with(raj.act.1, termco(dialogue, person, ml,
      short.term = FALSE, elim.old=FALSE))
  ltruncdf(datb, 20, 6)
  
  (dat2 <- data.frame(dialogue=c("@bryan is bryan good @br",
      "indeed", "@ brian"), person=qcv(A, B, A)))
  
  ml2 <- list(wrds=c("bryan", "indeed"), "@", bryan=c("bryan", "@ br", "@br"))
  
  with(dat2, termco(dialogue, person, match.list=ml2))
  
  with(dat2, termco(dialogue, person, match.list=ml2, percent = FALSE))
  
  DATA$state[1] <- "12 4 rgfr  r0ffrg0"
  termco(DATA$state, DATA$person, '0', digit.remove=FALSE)
  DATA <- qdap::DATA
  
   Using with term_match and exclude
  exclude(term_match(DATA$state, qcv(th), FALSE), "truth")
  termco(DATA$state, DATA$person, exclude(term_match(DATA$state, qcv(th),
      FALSE), "truth"))
  MTCH.LST <- exclude(term_match(DATA$state, qcv(th, i)), qcv(truth, stinks))
  termco(DATA$state, DATA$person, MTCH.LST)
  
  syns <- synonyms("doubt")
  syns[1]
  termco(DATA$state, DATA$person, unlist(syns[1]))
  synonyms("doubt", FALSE)
  termco(DATA$state, DATA$person, list(doubt = synonyms("doubt", FALSE)))
  termco(DATA$state, DATA$person, syns)
  
  # termco_d examples:
  termco_d(DATA$state, DATA$person, c(" the", " i'"))
  termco_d(DATA$state, DATA$person, c(" the", " i'"), ignore.case=FALSE)
  termco_d(DATA$state, DATA$person, c(" the ", " i'"))
  
  #  termco2mat example:
  MTCH.LST <- exclude(term_match(DATA$state, qcv(a, i)), qcv(is, it, am, shall))
  termco_obj <- termco(DATA$state, DATA$person, MTCH.LST)
  termco2mat(termco_obj)
  plot(termco_obj)
  plot(termco_obj, label = TRUE)
  plot(termco_obj, label = TRUE, text.color = "red")
  plot(termco_obj, label = TRUE, text.color="red", lab.digits=3)
  
     REVERSE TERMCO (return raw words found per variable)
  df <- data.frame(x=1:6,
      y = c("the fluffy little bat" , "the man was round like a ball",
          "the fluffy little bat" , "the man was round like a ball",
          "he ate the chair" , "cough, cough"),
      stringsAsFactors=FALSE)
  
  l <- list("bat" ,"man", "ball", "heavy")
  z <- counts(termco(df$y, qdapTools::id(df), l))[, -2]
  
  counts2list(z[, -1], z[, 1])
  
 #  politness
  politness <- c("please", "excuse me", "thank you", "you welcome",
      "you're welcome", "i'm sorry", "forgive me", "pardon me")
  
  with(pres_debates2012, termco(dialogue, person, politness))
  with(hamlet, termco(dialogue, person, politness))
  
     Term Use Percentage per N Words
  dat <- with(raj, chunker(dialogue, person, n.words = 100, rm.unequal = TRUE))
  dat2 <- list2df(dat, "Dialogue", "Person")
  dat2[["Duration"]] <- unlist(lapply(dat, id, pad=FALSE))
  dat2 <- qdap_df(dat2, "Dialogue")
  
  Top5 <- sapply(split(raj$dialogue, raj$person), wc, FALSE) %>%
      sort(decreasing=TRUE) %>%
      list2df("wordcount", "person") %>%
      `[`(1:5, 2)
  
  propdat <- dat2 %&%
      termco(list(Person, Duration), as.list(Top25Words[1:5]), percent = FALSE) %>%
      proportions %>%
      colsplit2df %>%
      reshape2::melt(id=c("Person", "Duration", "word.count"), variable="Word") %>%
      dplyr::filter(Person %in% Top5)
  
  head(propdat)
  
  ggplot(propdat, aes(y=value, x=Duration, group=Person, color=Person)) +
      geom_line(size=1.25) +
      facet_grid(Word~., scales="free_y") +
      ylab("Percent of Word Use")  +
      xlab("Per 100 Words") +
      scale_y_continuous(labels = percent)
  
  ggplot(propdat, aes(y=value, x=Duration, group=Word, color=Word)) +
      geom_line(size=1.25) +
      facet_grid(Person~.) +
      ylab("Percent of Word Use")  +
      xlab("Per 100 Words") +
      scale_y_continuous(labels = percent)
  
  ggplot(propdat, aes(y=value, x=Duration, group=Word)) +
      geom_line() +
      facet_grid(Word~Person, scales="free_y") +
      ylab("Percent of Word Use")  +
      xlab("Per 100 Words") +
      scale_y_continuous(labels = percent) +
      ggthemes::theme_few()