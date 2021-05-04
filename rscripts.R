setwd("C:/pp")
library(dplyr)
library(ggplot2)
library(scales)
library(ggsci)
data = read.csv("profit.csv",header = TRUE)
data2 = read.csv("pop_by_year.csv",header = TRUE)
data <- cbind(data, data2[,2:4])
data$전체<-gsub(",","",data$전체)
data$우대권.사용자 <-gsub(",","",data$우대권.사용자)
data$적자현황 <-gsub(",","",data$적자현황)
data$우대권.손실금 <-gsub(",","",data$우대권.손실금)

data$전체 <- as.numeric(data$전체)
data$우대권.사용자 <- as.numeric(data$우대권.사용자)
data$적자현황 <- as.numeric(data$적자현황)
data$적자현황<-abs(data$적자현황)/10000000000
data$우대권.손실금 <- as.numeric(data$우대권.손실금)/10000000000
우대인구비율 <- (data$우대/data$총인구수)*100
우대권사용자비율 <- (data$우대권.사용자/data$전체)*100
data <- cbind(data,우대인구비율,우대권사용자비율)
data_org <- data
data<-data[0:12,]         #코로나로인한 2020년 데이터 제외
data
#고령인구와 우대권사용자비율의 상관관계분석
out=lm(우대권사용자비율~우대인구비율,data)
summary(out)

cor(data$우대권.손실금,data$적자현황)
out2 = lm(우대권.손실금~적자현황,data)
summary(out2)


myhead <- c("연도","인구","고령여부")
data_old <- cbind(data$연도,(data$우대),"O")
colnames(data_old) <- myhead
data_old <- data.frame(data_old)

myhead <- c("연도","인구","고령여부")
data_tot <- cbind(data$연도,(data$비우대),"X")
colnames(data_tot) <- myhead
data_tot <- data.frame(data_tot)

data_pop <- rbind(data_old,data_tot)
colnames(data_pop) <- c("연도","총인구","고령")
data_pop <- data.frame(data_pop)
data_pop$총인구<- as.numeric(as.character(data_pop$총인구))/10000
data_pop$연도<- as.numeric(as.character(data_pop$연도))

#draw graph
options(scipen = 0)
summary(data_pop)

#고령인구  그래프
g<-ggplot(data_pop,aes(연도,총인구,fill=고령))+
  geom_bar(stat='identity',position='dodge')+
  ylab('인구')
ggsave("청년층과 고령인구.pdf")
plot(g)

#고령인구 비율과 우대권 사용자 비율
g<-ggplot(data,aes(x=연도,y=우대인구비율))+geom_line(color='red')+
  geom_line(aes(x=연도,y=우대권사용자비율), color='blue')
ggsave("고령인구비율과 우대권 사용자비율.pdf")
plot(g)

g<-ggplot(data,aes(x=연도,y=적자현황))+geom_line(color='blue')+
  geom_line(aes(x=연도,y=우대권.손실금),color='red')+
  ylab('적자현황(단위 : 백 억)')
ggsave("적자 대비 우대권 손실금.pdf")
plot(g)
