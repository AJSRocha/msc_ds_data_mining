# seguindo exemplo do lab 4

corpus = Corpus(VectorSource(df$description))

dtm = DocumentTermMatrix(corpus)

inspect(corpus[[1]])


nDocs(dtm)
nTerms(dtm)
Terms(dtm)
inspect(dtm)

dtm.bin = weightBin(dtm)
inspect(dtm.bin)

dtm.tfidf = weightTfIdf(dtm)
inspect(dtm.tfidf)

temp = tm_map(corpus, content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(removePunctuation,
         preserve_intra_word_contractions = TRUE,
         preserve_intra_word_dashes = TRUE)

dtm = DocumentTermMatrix(temp)
tdm = TermDocumentMatrix(temp)
inspect(dtm)
inspect(tdm)

Terms(dtm)