library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(XML)
cardic <- c()
url1 <- "http://terms.naver.com/list.nhn?cid=42330&categoryId=42330&page="
for (row in 1:686){
  url <- str_c(url1,as.character(row))
  t <- htmlTreeParse(url,useInternalNodes=TRUE,trim=TRUE)
  minutes <- xpathSApply(t,"//dt",xmlValue)
  wordsraw <- minutes[2:15]
  words<-str_replace_all(str_extract(str_replace_all(str_replace_all(wordsraw," \\[","")," ","_"),"[가-히|_]{1,}"),"_"," ")
  cardic<-c(cardic,words)
}


