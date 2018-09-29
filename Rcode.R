library(tm)
library(wordcloud)
library(RWeka)
library(stringi)
library(stringr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(data.table)
setwd("D:/test/Coursera/Capstone_final project")

#####LOADING 
blogs = readLines("final/en_US/en_US.blogs.txt", skipNul = T, encoding="UTF-8")
news = readLines("final/en_US/en_US.news.txt",skipNul = T, encoding="UTF-8")
twitter = readLines("final/en_US/en_US.twitter.txt",skipNul = T, encoding="UTF-8")

#####GET DATA SAMPLE
set.seed(324)
sample.size=1000

sample.blog=sample(blogs,sample.size)
sample.new=sample(news,sample.size)
sample.twitter=sample(twitter,sample.size)

#####CLEANING

sample=c(sample.blog,sample.new,sample.twitter)

##lower case
sample=tolower(sample)

##remove website link and twitter @
sample=gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", " ", sample)
sample=gsub("@[^\\s]+"," ",sample)

##remove latin1 words
latin.sym=grep("[^NOT_ASCII](NOT_ASCII){2}[^NOT_ASCII]",iconv(sample, "latin1", "ASCII", sub="NOT_ASCII"))
sample[latin.sym]=stri_trans_general(sample[latin.sym], "latin-ascii")
sample=str_replace_all(sample, "[[:punct:]]", "'")
sample=gsub('[^\x20-\x7E]', "'", sample)

##remove stopwords
sample=removeWords(sample,stopwords("en"))

##remove punctuations
sample=removePunctuation(sample)

##remove numbers
sample=removeNumbers(sample)

##remove profinity
swear.words = read.table(file ="swearWords.txt", stringsAsFactors=F)
sample=removeWords(sample,swear.words[,1])

##remove extra space
sample=stripWhitespace(sample)

corpus = VCorpus(VectorSource(sample))
corpus = tm_map(corpus, PlainTextDocument)

##### FREQUENCY TABLE
getFreq = function(tdm,ngram) {
  gram=function(x) NGramTokenizer(x,Weka_control(min=ngram,max=ngram))
  tdm = TermDocumentMatrix(tdm,control= list(tokenizer=gram))
  freq1 = sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  freq=data.frame(word = names(freq1), freq = freq1)
  freq$word=as.character(freq$word)
  return(freq)
}

uni.freq = getFreq(corpus,1)
bi.freq = getFreq(corpus,2)
tri.freq= getFreq(corpus,3)
quad.freq = getFreq(corpus,4)

unigram = setDT(uni.freq)
save(unigram,file="./NextWord_Prediction/unigram.Rda")
bigram = setDT(bi.freq)
save(bigram,file="./NextWord_Prediction/bigram.Rda")
trigram = setDT(tri.freq)
save(trigram,file="./NextWord_Prediction/trigram.Rda")
quadgram=setDT(quad.freq)
save(quadgram,file="./NextWord_Prediction/quadgram.Rda")

