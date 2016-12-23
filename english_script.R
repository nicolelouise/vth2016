Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(Needed, dependencies=TRUE)   

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("SnowballC")

cname_english <- file.path("H:", "english")
cname_english
dir(cname_english)

#Loading libraries
library(tm)
library(SnowballCC)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(fpc)
library(ggplot2)

#This is where the corpus is
docs_english <- Corpus(DirSource(cname_english))
summary(docs_english)

inspect(docs_english)

#Removal op stopwords to make useful freq's
docs_english <- tm_map(docs_english, removeWords, stopwords("english"))

#Unnecessary whitespaces
docs_english <- tm_map(docs_english, stripWhitespace)

#Some sort of summary
dtm_english <- DocumentTermMatrix(docs_english)
dtm_english

#Same as dtm
tdm_english <- TermDocumentMatrix(docs_english)   
tdm_english

freq_english <- colSums(as.matrix(dtm_english))   
length(freq_english)
orderedfreq_english <- order(freq_english)

#Really weird matrix with frequencies
matrixorderedfreq <- as.matrix(dtm_english)   
dim(matrixorderedfreq)   
write.csv(matrixorderedfreq, file="matrixorderedfreq.csv")

#Frequencies of all the words, not as a df
freq_english <- sort(colSums(as.matrix(dtm_english)), decreasing=TRUE)
head(freq_english, 15)

#Viewing the head and tail of freq_english
freq_english[head(orderedfreq_english)]
freq_english[tail(orderedfreq_english)]

#Makes a dataframe of freq_english
freq_english_df <- data.frame(word=names(freq_english), freq=freq_english)

#Words with freq's over 30, 20, 10
findFreqTerms(dtm_english, lowfreq=50)
findFreqTerms(dtm_english, lowfreq=40)
findFreqTerms(dtm_english, lowfreq=30)
findFreqTerms(dtm_english, lowfreq=20)
findFreqTerms(dtm_english, lowfreq=10)

#saved last two searches as vectors
words_freq_over_10 <- findFreqTerms(dtm_english, lowfreq=10)
words_freq_over_20 <- findFreqTerms(dtm_english, lowfreq=20)
words_freq_over_40 <- findFreqTerms(dtm_english, lowfreq=40)
words_freq_over_50 <- findFreqTerms(dtm_english, lowfreq=50)

#Plotting freq_english_df
p <- ggplot(subset(freq_english_df, freq>50), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Loading the pos and neg words lists
neg_words = read.table("negative-words2.csv", header=FALSE, sep=",")
pos_words = read.csv("positive-words2.csv", header=FALSE, sep=",")

#Loading in the texts
englisharticles <- read.table("englisharticles2.txt", sep=";", header=FALSE)

#Turning it into a corpus with tm library
englisharticles_corpus <-  Corpus(VectorSource(englisharticles))

#Trying the qdap/sentimentr package
install.packages("sentimentr")
library(sentimentr)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(sentimentr)

#AND THEN
sentimentr::sentiment(text.var=englisharticles_corpus,
	polarity_dt=sentimentr::polarity_table, valence_shifters_dt =
	sentimentr::valence_shifters_table, hyphen="",
	amplifier.weight = 0.8, n.before=5, n.after=2,
	question.weight = 1)

#Export as .csv
write.table(sentiment_englisharticles, file="sentiment_englisharticles.csv", sep= ",")

#Trying RSentiment package
install.packages("RSentiment")
library(RSentiment)


