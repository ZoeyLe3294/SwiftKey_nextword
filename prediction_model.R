setwd("D:/test/Coursera/Capstone_final project/NextWord_Prediction")
load("unigram.Rda")
load("bigram.Rda")
load("trigram.Rda")
load("quadgram.Rda")
library(stringr)
library(stringi)
library(tm)
library(NLP)
wordproc <- function(sentence){
  found=c()
  sentence <- removeNumbers(sentence)
  sentence <- removePunctuation(sentence)
  sentence <- tolower(sentence)
  
  ##Match last words to ngram data
  wordPred=function(nword,ngram){
    last.words=word(sentence,-nword,-1)
    foundlist = ngram[grep(paste("^",last.words," ",sep=""),ngram$word),]
    found=foundlist[word(foundlist$word,2,-1)!=last.words,]
    if(nrow(found)!=0){
      result=head(word(found$word,-1),4)
    }else{
      result=c()
    }
    return(as.vector(result))
  }
  
  ##CHECK WITH 4-GRAM THEN 3 THEN 2 THEN 1 IF NECESSARY  
  if(stri_count_words(sentence)>=3){
    found=wordPred(3,quadgram)
    mess="Next word is predicted using quadra-gram."
  }
  if(length(found)==0||stri_count_words(sentence)==2){
    found=wordPred(2,trigram)
    mess="Next word is predicted using tri-gram."
  } 
  if(length(found)==0||stri_count_words(sentence)==1){
    found=wordPred(1,bigram)
    mess="Next word is predicted using bi-gram."
  } 
  if(length(found)==0){
    found=head(unigram$word,4)
    mess="No match found. The most common words are returned"
  }
  #######################
  
  return(c(found,mess))
}



  
  
  
  
  
  
  
  