# carData
rm(list=ls(all=TRUE))
library(rvest)
library(stringr)

cardata <- data.frame(c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),
                      c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),
                      c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),
                      c(""),c(""),c(""),stringsAsFactors = FALSE)

names(cardata) <- c('modelName','brand','nationality','sizeClass','modelfuelEffi','fuel','market','thumnail',
                    'line_Model','line_Price','line_engineType','line_scType','line_displacement','line_fuel','line_fEffi',
                    'line_passenger','line_DriveType','line_transmission','lime_MaxRpm','line_MaxTorque','line_CO2','line_length',
                    'line_width','line_height','line_base','line_weight','line_fWheel','line_rWheel','line_fwSuspension','line_rwSuspension',
                    'line_fwbrake','line_rwbrake','line_steering')

maindatadf <- data.frame(c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),c(""),stringsAsFactors = FALSE)
names(maindatadf) <-c('modelName','brand','nationality','sizeClass','modelfuelEffi','fuel','market','thumnail','price')

####################################################
##### 브랜드별 모델 url 가져오기
####################################################
getCardata <- function(address){
  library(rvest)
  library(stringr)
  library(plyr)
  library(dplyr)
  modelUrls <- c()
  lineUrls <- c()
  talkUrls <- c()
  idx <- 1

  for (urlString in address){
    url <- str_c(urlString,"1")
    Carhtml <- read_html(url)
    carInfos <- html_nodes(Carhtml, css='.model_ct')
    for (row in carInfos){
      modelUrls[idx] <- str_c("http://auto.naver.com/car/main.nhn?yearsId=",str_extract(row,"[0-9]{5}"))
      lineUrls[idx] <- str_c("http://auto.naver.com/car/lineup.nhn?yearsId=", str_extract(row,"[0-9]{5}"))
      idx <- idx + 1
    }
    pn <- 2
    while (length(carInfos) != 0){
      url <- str_c(urlString,as.character(pn))
      Carhtml <- read_html(url) #해당 url 페이지의 html tag를 가져와서 parsing함.
      carInfos <- html_nodes(Carhtml, css='.model_ct')
      if (length(carInfos) ==  0){
        break
      }
      for (row in carInfos){
        modelUrls[idx] <- str_c("http://auto.naver.com/car/main.nhn?yearsId=",str_extract(row,"[0-9]{5}"))
        idx <- idx + 1
      }
      pn <- pn + 1 
    }
  }
  modelUrls
  modelUrlsMain <-unique(modelUrls)
  modelUrlsMain
  
  
  for (mu in modelUrlsMain) {
    url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
    detailurl <- read_html(mu)
    carnameraw <- html_nodes(detailurl, css='.end_model')
    details <- html_nodes(detailurl, css='.detail_lst')
    head(details)
    priceraw <- html_nodes(detailurl, css='.sale')
    priceraw2 <- str_replace_all(str_replace_all(str_extract_all(str_replace_all(priceraw,'"',""),"class=price>[0-9|,|~]{1,}"),"class=price>",""),",","")
    price <- as.character(mean(as.numeric(unlist(str_extract_all(priceraw2,"[0-9]{1,}")))))
    carname <- str_replace_all(str_extract_all(str_replace_all(carnameraw," ","_"),'<h3>\\w{1,}'),"<h3>","")
    fuelEffi <- str_replace_all(unlist(str_extract_all(details,"<dd>[0-9|~|ℓ|km|_|.]{1,}")),"<dd>","")
    details <- str_replace_all(details,"[[:punct:]]","")
    details <- str_replace_all(details," ","_")
    brand <- str_replace_all(str_extract_all(details,"class=>[가-히|A-Z|a-z]{1,}"),"class=>","")
    type <- str_replace_all(unlist(str_extract_all(details,"event>[가-히]{1,}")),"event>","")
    etc <- str_replace_all(unlist(str_extract_all(details,"<dd>[^'13'|<a_href|^km][가-히|a-z|A-Z|_]{1,}")),"<dd>","")
    imageUrl <- str_extract(html_nodes(detailurl, css='.main_img'),url_pattern)
    maindata <- c(carname,brand,type,fuelEffi,etc,imageUrl,price)
    if (length(maindata) == 9){
      maindatadf[nrow(maindatadf)+1,] <- maindata
    }else{
      cat('Maindataerror',maindata[1],"\n")
    }
    
    
    
    
    lineurl <- read_html(str_replace_all(mu,"main","lineup"))
    maincardata <- c(carname,brand,type,fuelEffi,etc,imageUrl)
    linethums <- html_nodes(lineurl, css='.thumb')
    head(linethums)
    lineAmount <- 0
    idx <- 1
    for (ii in linethums){
      result1 = str_extract_all(ii,'[가-히]{5}')
      if (result1 == "등급모델이"){
        
      }else{
        lineAmount = lineAmount + 1
        
      }
      idx <- idx + 1
    }
    
    linedetails <- html_nodes(lineurl, css='.lineup_btm_td')
    linedetails <- str_replace_all(str_replace_all(str_replace_all(linedetails,'\t',''),'\n',''),'&#13;','')
    priceandLinename <- html_nodes(lineurl, css='.price_section')
    linenames <- str_replace_all(str_extract_all(str_replace_all(priceandLinename," ","_"),"<dt>[0-9|A-Z|a-z|가-히|,|.|(|)|_|~|/|ℓ]{1,}"),"<dt>",'')
    prices <- as.numeric(str_replace_all(str_replace_all(str_extract(str_replace_all(priceandLinename," ","_"),"<strong>[0-9|,]{1,}"),"<strong>",''),",",""))
    
    for (ln in c(1:lineAmount)){
      linedetailarray <- str_replace_all(unlist(str_extract_all(str_replace_all(linedetails[ln]," ","_"),"<li>[0-9|A-Z|a-z|가-히|,|.|(|)|_|~|/|ℓ|β]{1,}")),"<li>",'')
      if (linedetailarray[1] == '정보없음' & linedetailarray[2] != "정보없음"){
        cardataRow <- c(maincardata,linenames[ln],prices[ln],c('정보없음',linedetailarray)[-c(9,10,11,12,15,16,17,18,24,25,27,30,31)])
      }else if (length(linedetailarray) == 37) {
        cardataRow <- c(maincardata,linenames[ln],prices[ln],linedetailarray[-c(9,10,11,14,15,16,17,18,19,25,26,28,31,32)])
      }else if (length(linedetailarray) == 35 & linedetailarray[1] != '정보없음'& linedetailarray[2] != '정보없음'){
        cardataRow <- c(maincardata,linenames[ln],prices[ln],linedetailarray[-c(9,10,11,14,15,16,17,23,24,26,29,30)])
      }else if (linedetailarray[1] == '정보없음'& linedetailarray[2] == '정보없음'){
        if (linenames[1] == "EV" | linenames[1] == "레이_EV"  ){
          cardataRow <- c(maincardata,linenames[ln],prices[ln],c('정보없음',linedetailarray)[-c(9,10,11,12,15,16,17,18,24,25,27,30,31)])
        }else{
          cardataRow <- c(maincardata,linenames[ln],prices[ln],linedetailarray[-c(11,14,15,16,17,23,24,26,29,30)])
        } 
      }else{
        cardataRow <- c(maincardata,
                        'error','error','error','error','error','error','error',
                        'error','error','error','error','error','error','error',
                        'error','error','error','error','error','error','error',
                        'error','error','error','error')
        break
      }
      if (length(cardataRow) == 33){
        cardata[nrow(cardata)+1,] <- cardataRow
      }else{
        cat('detaildata error',cardataRow[1],"\n")
      }
    }
    
  }
  #return(cardata) #모델*라인 detail data가 필요할 경우 사용
  return(maindatadf)
}

urlStrings <- c("http://auto.naver.com/company/main.nhn?mnfcoNo=12&modelType=OS&&order=0&importYn=N&page=",
                "http://auto.naver.com/company/main.nhn?mnfcoNo=12&modelType=DC&order=0&importYn=N&page=")

setwd('/Users/yunwoonam/Desktop/archive')
cardata <- getCardata(urlStrings)
maindatadf<- getCardata(urlStrings)
cardata <- cardata[-1,]
maindatadf <- maindatadf[-1,]
write.table(maindatadf,"maindatadf.csv", fileEncoding = "UTF-8",col.names = T,row.names = F)
write.table(cardata,"cardata.csv", fileEncoding = "UTF-8",col.names = T,row.names = F)
head(cardata)
head(maindatadf)
# Maindataerror 2004_모하비_컨셉트
# detaildata error 2004_모하비_컨셉트
# Maindataerror 2012_트랙스터
# Maindataerror 2008_보레고_FCEV_컨셉트
# detaildata error 2008_보레고_FCEV_컨셉트
# Maindataerror 2013_캅
# Maindataerror 2011_GT_컨셉트
# detaildata error 2010_씨드

getwd()
cardata$modelName
library(stringr)
cardata <- cardata[-1,]

for (col in 1:33){
  cardata[,col] <- as.factor(cardata[,col])
}

for (col in 1:33){
  if (col != 8){
    cardata[,col] <- str_replace_all(cardata[,col],"_","")
  }
}

cardata[252,] # 2016스파크EV
cardata[95,]   #2015쏘울EV
cardata[174,]  # 2016아이오닉일렉트릭
cardata[175,] # 2016아이오닉일렉트릭
cardata[278,] <- cardataRow# 2017쉐보레볼트EV
cardata[cardata$line_fEffi == '4인승']
cardata<-cardata[-1,]
str(cardata)
data <- read.csv('cardataOnchanging.csv')
cardata <- data.frame(lapply(cardata, as.character), stringsAsFactors=FALSE)
write.csv(cardata,file='CarsinMarket.csv')
table(data$nationality)
columns <- c('modelName','brand','nationality','sizeClass','modelfuelEffi','fuel','market','thumnail',
             'line_Model','line_Price','line_engineType','line_scType','line_displacement','line_fuel','line_fEffi',
             'line_passenger','line_DriveType','line_transmission','lime_MaxRpm','line_MaxTorque','line_CO2','line_length',
             'line_width','line_height','line_base','line_weight','line_fWheel','line_rWheel','line_fwSuspension','line_rwSuspension',
             'line_fwbrake','line_rwbrake','line_steering')


