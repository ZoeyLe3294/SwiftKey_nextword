library(tm)
library(RWeka)
library(stringi)
library(stringr)
library(knitr)
library(qdap)
library(data.table)
setwd("D:/test/Coursera/Capstone_final project")
memory.limit(size=56000)

#####LOADING 
blogs = readLines("final/en_US/en_US.blogs.txt", skipNul = T, encoding="UTF-8")
news = readLines("final/en_US/en_US.news.txt",skipNul = T, encoding="UTF-8")
twitter = readLines("final/en_US/en_US.twitter.txt",skipNul = T, encoding="UTF-8")

#####GET DATA SAMPLE
set.seed(123)
sample.size=1000

sample.blog=sample(blogs,sample.size)
sample.new=sample(news,sample.size)
sample.twitter=sample(twitter,sample.size)

#####CLEANING

sample=c(sample.blog,sample.new,sample.twitter)

##remove website link and twitter @
sample=gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", " ", sample)
sample=gsub("@[^\\s]+"," ",sample)

# Remove text within brackets
sample=bracketX(sample)

##remove latin1 words
latin.sym=grep("[^NOT_ASCII](NOT_ASCII){2}[^NOT_ASCII]",iconv(sample, "latin1", "ASCII", sub="NOT_ASCII"))
sample[latin.sym]=stri_trans_general(sample[latin.sym], "latin-ascii")
sample=gsub('[^\x20-\x7E]', "'", sample)

##replace abbreviate words with their full terms
sample=replace_abbreviation(sample)

##replace contractions with their base words
sample=replace_contraction(sample)

##lower case
sample=tolower(sample)

##remove stopwords
#sample=removeWords(sample,stopwords("en"))
sample=gsub("'[A-z]+", " ", sample)

##remove punctuations
sample=gsub("[[:punct:]]", " ", sample)

##remove numbers
sample=removeNumbers(sample)

##remove profinity
swear.words = read.table(file ="swearWords.txt", stringsAsFactors=F)
sample=removeWords(sample,swear.words[,1])

##remove extra space
sample=stripWhitespace(sample)

corpus = VCorpus(VectorSource(sample))
corpus = tm_map(corpus, PlainTextDocument)

rm(sample.twitter,sample.blog,sample.new)
rm(swear.words,latin.sym)
rm(sample.size)
##### FREQUENCY TABLE
getFreq = function(tdm,ngram) {
  gram=function(x) NGramTokenizer(x,Weka_control(min=ngram,max=ngram))
  tdm = TermDocumentMatrix(tdm,control= list(tokenizer=gram))
  freq1 = sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  freq=data.frame(word = names(freq1), freq = freq1)
  freq$word=as.character(freq$word)
  return(freq)
}
gc()
memory.limit(size=56000)
uni.freq = getFreq(corpus,1)
bi.freq = getFreq(corpus,2)
tri.freq= getFreq(corpus,3)
quad.freq = getFreq(corpus,4)

unigram = setDT(uni.freq)
save(unigram,file="./NextWord_Prediction/unigram_nostop.Rda")
rm(unigram,uni.freq)
bigram = setDT(bi.freq)
save(bigram,file="./NextWord_Prediction/bigram_nostop.Rda")
trigram = setDT(tri.freq)
save(trigram,file="./NextWord_Prediction/trigram_nostop.Rda")
quadgram=setDT(quad.freq)
save(quadgram,file="./NextWord_Prediction/quadgram_nostop.Rda")

