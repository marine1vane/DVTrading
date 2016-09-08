packs = c(
  "tm",                         # Text mining
  "tm.plugin.webmining",        # Web-source plugin for text mining
  "SnowballC",                  # Stemmer
  "RColorBrewer",               # Colors for visualisation
  "ggplot2",                    # Plotting
  "wordcloud",                  # Draw wordclouds
  "openNLP"                     # Split text into sentences.
)
setwd("C:/Users/Duke/Documents/GitHub/DVTrading2")  #for finding my sentiment lexicon
sapply(packs, require, character.only=TRUE)   # Load the packages.

ToSentences = function(text, language="en") {
  # Splits text into sentences using an Apache OpenNLP sentence detector.
  
  # Arguments:
  # "text" the text to be processed (character)
  # "lang" ISO-639 code of the language of the text (character)
  
  # Returns:
  # sentences of the text (character vector)
  if(length(text) ==0)      {return("")}
  if(nchar(text) == 0)   {return("")}   # Cover special case 0-character text.
  
  # Convert text to String object; allows for splitting by index.
  text = as.String(text)
  
  # Discover the sentence markers in the text (specify NLP as
  # source of annotate because there is also an annotate function in ggplot2)
  markers = NLP::annotate(
    text,
    Maxent_Sent_Token_Annotator(language=language)   # Annotator from OpenNLP
  )
  
  # Return sentences by splitting the text at the boundaries.
  text[markers]
}
# ^ figure out how do discard the zero text option

CorpusToSentences = function(corpora) {
  # Split every document in the corpus into sentences and return a new corpus
  # with all the sentences as individual documents.
  
  # Extract the text from each document in the corpus.
  text <- lapply(corpora, "[[", "content")
  
  # Basically convert the text
  docs <- lapply(text, ToSentences)
  
  docs <- as.vector(unlist(docs))
  
  # Return a corpus with sentences as documents.
  Corpus(VectorSource(docs))
}

#Reads in a string of the ticker, preferably with the exchange prefix
#i.e. "NASDAQ:MSFT"
#ticker <- "NASDAQ:MSFT"
Stock_Sentiment <- function(ticker) {
  # Download the corpora and insert them in a named list.
  # Probably a way to make it more eficient and keep it in a WebCorpus without the list
  corpora = list(WebCorpus(GoogleFinanceSource(ticker)))
  # Save the corpora list. ADD Date and TICKER
  save(corpora, file=paste("data/", Sys.Date(), ticker, "corpora.Rdat"))
  
  # Create a new corpus which merges existing corpora after splitting them
  # into sentences.
  corpus = Reduce(c, lapply(corpora, CorpusToSentences))
  
  # Process the corpora contents.
  corpus = tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, stripWhitespace)
  
  dtm = DocumentTermMatrix(corpus)
  
  # Remove terms which are not contained in any of the documents.
  dtm = dtm[ , colSums(as.matrix(dtm)) > 0]
  
  # ACQUIRING AND PROCESSING THE LEXICON.
  
  # Load the sentiment lexicon (saved down in working directory as a
  # comma separated value file).
  lex = read.csv("DVstockbasic.csv", stringsAsFactors=FALSE)
  
  # Collapse words with multiple entries into one entry. These are marked
  # with a trailing #1, #2, ...
  
  # Remove #1 tags. 
  lex$Entry = gsub("#1", "", lex$Entry)
  
  # Remove entries that are still numbered (i.e. two or higher)
  lex = lex[!grepl("#", lex$Entry), ]
  
  # Extract the positive and negative words from the lexicon.
  neg.lex = tolower(lex$Entry[lex$Negativ != ""])
  pos.lex = tolower(lex$Entry[lex$Positiv != ""])
  
  terms = colnames(dtm)
  
  # Find the positive and negative terms using the lexicons.
  neg.terms = terms[terms %in% neg.lex]
  pos.terms = terms[terms %in% pos.lex]
  
  # Specify positive terms which may be quiestionable.
  #pos.terms.adj = setdiff(pos.terms, c("equity", "share", "consensus"))
  
  # Calculate the negative and positive sentence scores ("document scores").
  neg.scores = rowSums(as.matrix(dtm[ , neg.terms]))
  pos.scores = rowSums(as.matrix(dtm[ , pos.terms]))
  
  document.scores = pos.scores - neg.scores
  
  # Calulate the document signs ("sentence signs").
  document.signs = sign(document.scores)
  
  # Calculate the sentiment score
  sentiment.score = sum(document.signs == 1) / sum(document.signs !=0)
  
  sum(document.scores)
  
  return(sentiment.score)
}
