### topic per detailModel
setwd('/Users/yunwoonam/Desktop/archive')
commentdata <- read.table("commentdataFin.csv", encoding = "UTF-8", stringsAsFactors = F, header = T)
maindatadf <- read.table("maindatadf.csv", encoding = "UTF-8", stringsAsFactors = F, header = T)
modelsFromCommentData <- unique(commentdata$model)
modelsFromMaindataDF <- maindatadf$modelName
##########################################################
### 1. 과정생략
### modelName 겹치는 것만 추출, price 0 인것제외
##########################################################
commentDF <- read.table("commentDF.csv", encoding = "UTF-8", stringsAsFactors = F, header = T)
mainDF <- read.table("mainDF.csv", encoding = "UTF-8", stringsAsFactors = F, header = T)

# 228개의 관측치 대상, 디자인 언급비율, 가격대 비교 

# 0. dictionary 준비
################################ Cardictionary 준비 ############################################
library(stringr)
library(rJava) # 로딩
library(KoNLP) # 세종 사전
library(tm) # 영문 텍스트 마이닝
library(wordcloud) # 단어 구름 사진
cardic2 <- read.csv("cardic.csv", header = T, stringsAsFactors = F, encoding = "UTF-8")
cardic2 <- cardic2$V
cardic2
useSejongDic()
mergeUserDic(data.frame(cardic2, c("ncn"))) 
###################################################################################################
# 전체 commentdata 분석
messages <- commentDF$message
messages <- str_replace_all(messages,"[ㄱ-ㅎ|ㅏ-ㅣ]"," ")
messages <- str_replace_all(messages,"이차"," ")
messages <- str_replace_all(messages,"견적"," ")
messages <- str_replace_all(messages,"부탁"," ")
messages <- str_replace_all(messages,"드립니다"," ")
comment_corpus <- Corpus(VectorSource(messages))
inspect(comment_corpus)
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
pal <- brewer.pal(12,"Paired")
wordcloud(carrelatedDF$word[1:200], carrelatedDF$amount[1:200],scale=c(4,0.8), min.freq=3, random.order=F,rot.per=.1,colors = pal,family="AppleGothic")


###################################################################################################
# 1. commentDF을 관측 대상별로 list화 
DataFrameList <- list()
for (i in c(1:length(mainDF$modelName))){
  DataFrameList[[i]] <- commentDF[commentDF$model==mainDF$modelName[i],]
}

# 2. DataFrame - (model, price, "디자인"언급비율, 연비) 생성 
modelDesginPrice_DF = data.frame()
for (i in c(1:length(DataFrameList))){
  model <- (DataFrameList[[i]]$model)[1]
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
  tag <- FALSE
  for (w in carrelatedDF$word){
    if(w == "디자인"){
      tag <- TRUE
    }
  }
  tag2 <- FALSE
  for (w in carrelatedDF$word){
    if(w == "연비"){
      tag2 <- TRUE
    }
  }
  if (tag == TRUE){
    prop <- ((carrelatedDF[carrelatedDF$word=='디자인',]$amount)*100)/sum(carrelatedDF$amount)
  }else{
    prop <- 0.0
  }
  if (tag2 == TRUE){
    fuelE <- ((carrelatedDF[carrelatedDF$word=='연비',]$amount)*100)/sum(carrelatedDF$amount)
  }else{
    fuelE <- 0.0
  }
  price <- mainDF[mainDF$modelName==model,]$price
  tempDF <-data.frame(model,price,prop,fuelE)
  modelDesginPrice_DF <- rbind(modelDesginPrice_DF,tempDF)

}
modelDesginPrice_DF$year <- as.numeric(str_replace_all(str_extract(modelDesginPrice_DF$model,'[0-9]\\w{4}'),'_',""))
modelDesginPrice_DF
names(modelDesginPrice_DF) <- c('model','price','design','fuele','year')

### price VS Design 산점도 그려보기
library(ggplot2)
ggplot(data = modelDesginPrice_DF,mapping = aes(price,design)) + geom_point(size=1) + ggtitle('각격대별 "디자인"언급 비율')
ggsave(filename = 'withOutlier_Design.png')


# 아웃라이어 제거
mDF<- modelDesginPrice_DF[modelDesginPrice_DF$price <= 5000.0,]
mDF <- mDF[mDF$design != 0.0,]

cor(mDF$price,mDF$design)

# 183개의 관측치
ggplot(data = mDF,mapping = aes(price,design)) + geom_point(size=1) + ggtitle('각격대별 "디자인"언급 비율')
ggsave(filename = 'withoutOutlier_Design.png')


### price VS 연비 산점도 그려보기

# 아웃라이어 제거
mDF<- modelDesginPrice_DF[modelDesginPrice_DF$price <= 5000.0,]
mDF <- mDF[mDF$fuele != 0.0,]
mDF <- mDF[mDF$fuele < 12.0,]
cor(mDF$price,mDF$fuele)
library(ggplot2)
ggplot(data = mDF,mapping = aes(price,fuele)) + geom_point(size=1) + ggtitle('가격대별 "연비"중요도') + geom_smooth(method='lm',formula=y~x) 
ggsave(filename = 'withoutOutlier_fuele.png')

y = mDF$fuele # 종속변수
x = mDF$price # 독립변수
result.lm <- lm(formula=y ~ x, data=mDF)
# 0.77886: 절편 0.73928:
## y = 0.77886 + 0.73928*X
x[1:5] #[[1] 4 3 4 2 2]
y[1:5]
# (2) 선형회귀 분석 결과 보기
summary(result.lm) 

result.lm

### year VS 디자인 산점도 그려보기

# 아웃라이어 제거
mDF <- modelDesginPrice_DF[modelDesginPrice_DF$design != 0.0,]
ggplot(data = mDF,mapping = aes(year,design)) + geom_point(size=1) + ggtitle('연도별 "디자인"언급 비율')
ggsave(filename = 'withoutOutlier_year_Design.png')

head(modelDesginPrice_DF)


### year VS 연비 산점도 그려보기

# 아웃라이어 제거
mDF <- modelDesginPrice_DF[modelDesginPrice_DF$fuele != 0.0,]
ggplot(data = mDF,mapping = aes(year,fuele)) + geom_point(size=1) + ggtitle('연도별 "연비"언급 비율')
ggsave(filename = 'withoutOutlier_year_fuele.png')


### 디자인 VS 연비 산점도 그려보기

# 아웃라이어 제거
mDF <- modelDesginPrice_DF[modelDesginPrice_DF$design != 0.0,]
mDF <- mDF[mDF$fuele != 0.0,]
cor(mDF$design,mDF$fuele)
ggplot(data = mDF,mapping = aes(year,fuele)) + geom_point(size=1) + ggtitle('연도별 "연비"언급 비율')
ggsave(filename = 'withoutOutlier_year_fuele.png')




