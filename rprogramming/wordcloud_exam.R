setwd("D:/rprogramming")
getwd()

# Install required packages (run once)
install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer"))

#load the libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

text <- readLines("feedback.txt", encoding = "UTF-8", warn = FALSE)# Read the file into R using readLines()
text <- paste(text, collapse = " ")# Combines all lines into one long text string separated by spaces
corpus <- Corpus(VectorSource(text))# Create a corpus from the imported text

# Text cleaning
corpus <- tm_map(corpus, content_transformer(tolower))#lowercase all letters
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z ]", " ", x)))#removes everything except alphabetic letters and spaces
corpus <- tm_map(corpus, removeNumbers)#removes all numbers
corpus <- tm_map(corpus, stripWhitespace)#removes unnecessary extra spaces
corpus <- tm_map(corpus, removeWords, stopwords("english"))#removes common english words(the, is, and, to)

tdm <- TermDocumentMatrix(corpus)#Create term-document matrix
m <- as.matrix(tdm)#Converts the TDM to a regular matrix
word_freq <- sort(rowSums(m), decreasing = TRUE)#Computes the total frequency of each word and sorts from highest → lowest
df <- data.frame(word = names(word_freq), freq = word_freq)#Creates a data frame with two columns: word, frequency

#Show the top 10 words
head(df, 10)

word_freq_rev <- df[order(df$freq, decreasing = FALSE), ]#Reverse for word cloud

#Generate reversed word cloud
set.seed(1234)
wordcloud(
  words = word_freq_rev$word,
  freq = word_freq_rev$freq,
  min.freq = 2,
  max.words = 1000,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
#Save word cloud as PNG
png("wordcloud_exam.png", width = 800, height = 600)
set.seed(1234)
wordcloud(
  words = word_freq_rev$word,
  freq = word_freq_rev$freq,
  min.freq = 2,
  max.words = 1000,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
dev.off()#Closes the graphics device — finalizes and saves the PNG file.

rare_df <- df[df$freq == 1, ]#Identify words with frequency = 1
rare_df5 <- head(rare_df, 5)#Take only the first 5 least frequent words

#Save word cloud as PNG
png("wordcloud_rare.png", width = 800, height = 600)
wordcloud(
  words = rare_df5$word,
  freq = rare_df5$freq,
  min.freq = 1,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
dev.off()#Closes the graphics device — finalizes and saves the PNG file.