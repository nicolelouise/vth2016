#Calculating the means from the sentiment df's
setwd("G:/VTHproject")
sentimentfrench <- read.csv(file="french/sentiment_frencharticles.csv", sep=",", header=TRUE)
mean_sentimentfrench <- mean(sentimentfrench$sentiment.1.1.1.79.0.2.1.2.32.0.3.1.3.3.0.4.1.4.64.0.element_id..number.sentence_id.word_count.sentiment)
sentimentenglish <- read.csv(file="english/sentiment_englisharticles.csv", sep=",", header=TRUE)
mean_sentimentenglish <- mean(sentimentenglish$sentiment)
sentimentspanish <- read.csv(file="spanish/sentiment_spanisharticles.csv", sep=",", header=TRUE)
mean_sentimentspanish <- mean(sentimentspanish$sentiment)

#Calculating the means from the wordcounts
mean_wordcountSPA <- mean(sentimentspanish$word_count)
mean_wordcountENG <- mean(sentimentenglish$word_count)
mean_wordcountFRE <- mean(sentimentfrench$word_count)

#Calculating the variance from sentiment's
var_FRE <- var(sentimentfrench$sentiment.1.1.1.79.0.2.1.2.32.0.3.1.3.3.0.4.1.4.64.0.element_id..number.sentence_id.word_count.sentiment)
var_ENG <- var(sentimentenglish$sentiment)
var_SPA <- var(sentimentspanish$sentiment)

#Calculating the sd's from sentiment's
sd_FRE <- sd(sentimentfrench$sentiment.1.1.1.79.0.2.1.2.32.0.3.1.3.3.0.4.1.4.64.0.element_id..number.sentence_id.word_count.sentiment)
sd_ENG <- sd(sentimentenglish$sentiment)
sd_SPA <- sd(sentimentspanish$sentiment)

#Descriptive statistics matrix
descr_stat_matrix <- matrix(c(mean_wordcountENG,mean_wordcountFRE,mean_wordcountSPA,mean_sentimentenglish,mean_sentimentfrench,mean_sentimentspanish,var_ENG,var_FRE,var_SPA,sd_ENG,sd_FRE,sd_SPA), nrow=3, ncol=4)
descr_stat_df <- data.frame(descr_stat_matrix)
colnames(descr_stat_matrix) <- c("mean_wordcount", "mean_sentiment", "mean_variance", "mean_sd")
rownames(descr_stat_matrix) <- c("English","French","Spanish")
descr_stat_df$lang <- c("eng", "fre","spa")

#Before ANOVA: test for normal distribution
shapiro.test(sentimentfrench$sentiment.1.1.1.79.0.2.1.2.32.0.3.1.3.3.0.4.1.4.64.0.element_id..number.sentence_id.word_count.sentiment)
shapiro.test(sentimentenglish$sentiment)
shapiro.test(sentimentspanish$sentiment)
##Not normal at all, but big samples

