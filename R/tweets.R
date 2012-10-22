
## function reading a csv file into a data frame

readTweets <- function(file) {
  table <- read.csv(file, colClasses = "character")
  
  ## tweet time
  table$time <-strptime(table$time, "%d/%m/%Y %H:%M:%S")
  hist(table$time, breaks = "hour", # breaks = "day"
       main = "Tweet Frequency",
       xlab = "Time")
  
  ## filter RT tweets
  rt_i <- grepl("RT @.+:", table$text)
  table <- table[!rt_i, ]
  
  # remove hashtags and user ids
  table$text <- gsub("[#@]([A-Za-z0-9_]+)", "", table$text)
  # remove urls
  table$text <- gsub("http://(.*)( |$)", "", table$text)
  # remove &amp; (ampersand &)
  table$text <- gsub("&amp;", "", table$text)
  
  table
}

## function that takes a character vector and create
## a word cloud from it, and export to pdf or png

makeWordCloud <- function(textVec, output = "pdf") {
  require(tm)
  require(wordcloud)
  require(RColorBrewer)
  
  ap.corpus <- Corpus(DataframeSource(data.frame(textVec)))
  ap.corpus <- tm_map(ap.corpus, tolower)
  ap.corpus <- tm_map(ap.corpus, function(x) {
    removeWords(x, append(stopwords("english"), c("via")))
  })
  ap.corpus <- tm_map(ap.corpus, removePunctuation)
  ap.tdm <- TermDocumentMatrix(ap.corpus)
  ap.m <- as.matrix(ap.tdm)
  ap.v <- sort(rowSums(ap.m), decreasing=TRUE)
  ap.d <- data.frame(word = names(ap.v), freq=ap.v)
  table(ap.d$freq)
  pal2 <- brewer.pal(8, "Dark2")
  
  if (output == "png") {
    png("wordcloud.png", width = 800, height = 600)
  } else if (output == "pdf") {
    pdf("wordcloud.pdf")
  }
  
  wordcloud(ap.d$word, ap.d$freq, 
            scale=c(8, .2), min.freq = 3, 
            max.words = Inf, random.order = FALSE, 
            rot.per = .15, colors = pal2)
  dev.off()
  
  TRUE # return TRUE
}
