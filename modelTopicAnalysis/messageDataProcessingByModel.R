# message Processing as Model
library(stringr)
library(rJava) # 로딩
library(KoNLP) # 세종 사전
library(tm) # 영문 텍스트 마이닝
library(wordcloud) # 단어 구름 사진
library(HMM)
library(arules) 
library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(XML)
library(ggplot2)
library(ggthemes)
library(extrafont)
################################ Cardictionary 준비 ############################################
cardic2 <- read.csv("cardic.csv", header = T, stringsAsFactors = F, encoding = "UTF-8")
cardic2 <- cardic2$V
cardic2
useSejongDic()
mergeUserDic(data.frame(cardic2, c("ncn"))) 
###################################################################################################
setwd('/Users/yunwoonam/Desktop/cartalks')
commentdata <- read.table("commentdataFin.csv", encoding = "UTF-8", stringsAsFactors = F, header = T)
carModelList <- unique(commentdata$category)
DataFrameList = list()
idx <- 1 
for (i in carModelList){
  DataFrameList[[idx]] = commentdata[commentdata[3] == i,]
  idx <- idx +1 
}
top5list <- data.frame('word'=c(),'amount'=c(),'category'=c())
for (i in c(1:length(DataFrameList))){
  category <- DataFrameList[[i]]$category[1]
  messages <-DataFrameList[[i]]$message
  messages
  messages <- str_replace_all(messages,"[ㄱ-ㅎ|ㅏ-ㅣ]"," ")
  messages <- str_replace_all(messages,"이차"," ")
  messages <- str_replace_all(messages,"견적"," ")
  messages <- str_replace_all(messages,"부탁"," ")
  messages <- str_replace_all(messages,"드립니다"," ")
  comment_corpus <- Corpus(VectorSource(messages))
  comment_corpus[is.na(comment_corpus)]   <- " "
  exNouns <- function(x) { 
    out <- tryCatch(
      {
        paste(extractNoun(as.character(x)), collapse=" ")
      },
      error=function(cond) {
        message(paste("error part:", x))
      },
      warning=function(cond) {
        message(paste("caused a warning:", x))
        message("Here's the original warning message:")
        message(cond)
      },
      finally={
      }
    )    
    return(out)
  }
  
  comment_nouns <- sapply(comment_corpus, exNouns)
  myCorpusComment <- Corpus(VectorSource(comment_nouns)) 
  myCorpusComment <- tm_map(myCorpusComment, removePunctuation) # 문장부호 제거
  myCorpusComment <- tm_map(myCorpusComment, removeNumbers) # 수치 제거
  myCorpusComment <- tm_map(myCorpusComment, content_transformer(tolower)) # 소문자 변경
  myCorpusComment <-tm_map(myCorpusComment, removeWords, stopwords('english')) # 불용어제거
  
  myCorpusComment_txt <- tm_map(myCorpusComment, PlainTextDocument)
  myCorpusComment_txt <- TermDocumentMatrix(myCorpusComment, control=list(wordLengths=c(2,Inf)))
  library(slam)
  myCorpusComment.df <- as.data.frame(row_sums(myCorpusComment_txt, na.rm = T))
  dim(myCorpusComment.df)
  wordResult <- sort(rowSums(myCorpusComment.df), decreasing=TRUE) # 빈도수로 내림차순 정렬
  carrelatedDF <- data.frame(c(0),c(""))
  names(carrelatedDF) <- c("amount", "word")
  for (i in cardic2){
    amount <- as.numeric(wordResult[names(wordResult) == i])[1]
    word <- i
    if (!is.na(amount)){
      carrelatedDF <- rbind(carrelatedDF,data.frame(amount,word))
    }
    
  }
  carrelatedDF <-carrelatedDF[-1,]
  carrelatedDF <- carrelatedDF[order(carrelatedDF$amount, decreasing = T),]
  carrelatedDF[1:100,]
  pal <- brewer.pal(12,"Paired")
  df <-carrelatedDF[1:30,]
  top5 <-data.frame(carrelatedDF[1:5,]$word,carrelatedDF[1:5,]$amount,c(category,category,category,category,category))
  top5list <- rbind(top5list,top5)
  ggplot(data = df, mapping = aes(word,amount)) + geom_bar(stat = "identity") + ggtitle(str_c(category,' 1~50 Topic'))
  ggsave(filename = str_c(category,'.png'))
}

theme_yunwoo<- function(base_size = 12, base_family = "NanumGothic"){
  (theme_foundation(base_size = base_size, base_family = base_family) +
     theme(line = element_line(colour = "black"), rect = element_rect(fill = ggthemes_data$fivethirtyeight["ltgray"],
                                                                      linetype = 0, colour = NA), text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]),
           axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
           axis.title = element_text(), axis.text = element_text(),
           axis.ticks = element_blank(), axis.line = element_blank(),
           legend.background = element_rect(), legend.position = "bottom",
           legend.direction = "horizontal", legend.box = "vertical",
           panel.grid = element_line(colour = NULL), panel.grid.major = element_line(colour = ggthemes_data$fivethirtyeight["medgray"]),
           panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0,
                                                                         size = rel(1.5), face = "bold"), plot.margin = grid::unit(c(1,
                                                                                                                                     1, 0.5, 0.5), "lines"), strip.background = element_rect(), panel.margin.x=NULL, panel.margin.y=NULL))
}

theme_set(theme_yunwoo())