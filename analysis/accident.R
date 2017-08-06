accdidentA1<-read.csv("D:/asiahack/105年度A1類交通事故資料.csv",header=TRUE)
View(accdidentA1)
#切出年月日時間的欄位
accdidentA1$year<-lapply(strsplit(as.character(accdidentA1$發生時間),"年"),"[",1)
accdidentA1$monthday<-lapply(strsplit(as.character(accdidentA1$發生時間),"年"),"[",2)
accdidentA1$month_day_time
accdidentA1$month<-lapply(strsplit(as.character(accdidentA1$monthday),"月"),"[",1)
accdidentA1$day_time<-lapply(strsplit(as.character(accdidentA1$monthday),"月"),"[",2)
accdidentA1$day_time
accdidentA1$day<-lapply(strsplit(as.character(accdidentA1$day_time),"日"),"[",1)
accdidentA1$time<-lapply(strsplit(as.character(accdidentA1$day_time),"日"),"[",2)
#切出死亡受傷人數
accdidentA1$death<-lapply(strsplit(as.character(accdidentA1$死亡受傷人數),";"),"[",1)
accdidentA1$injured<-lapply(strsplit(as.character(accdidentA1$死亡受傷人數),";"),"[",2)

accdidentA1$death<-lapply(strsplit(as.character(accdidentA1$death),"亡"),"[",2)
accdidentA1$injured<-lapply(strsplit(as.character(accdidentA1$injured),"傷"),"[",2)


accdidentA1$country<-lapply(strsplit(as.character(accdidentA1$發生地點),"鄉"),"[",1)
accdidentA1$country<-lapply(strsplit(as.character(accdidentA1$country),"鎮"),"[",1)
accdidentA1$country_region<-lapply(strsplit(as.character(accdidentA1$country),"縣"),"[",2)
accdidentA1$country_region<-lapply(strsplit(as.character(accdidentA1$country_region),"市"),"[",1)
accdidentA1$city<-lapply(strsplit(as.character(accdidentA1$country),"區"),"[",1)
accdidentA1$country<-lapply(strsplit(as.character(accdidentA1$country),"縣"),"[",1)


accdidentA1$country<-lapply(strsplit(as.character(accdidentA1$country),"縣"),"[",1)
accdidentA1$city<-lapply(strsplit(as.character(accdidentA1$city),"市"),"[",2)


accdidentA1_1 <- data.frame(lapply(accdidentA1, as.character))
accdidentA1_1

View(accdidentA1)
write.csv(accdidentA1_1,"accdidentA1.csv")




#------------------A2

A2 <-read.csv("C:/Users/CJS/Documents/105年度A2類交通事故資料.csv")
View(A2)

names(A2)
A2$year<-lapply(strsplit(as.character(A2$all.),"年"),"[",1)
typeof(A2$year)
A2$year <- as.numeric(A2$year)
View(A2)

A2$monthday<-lapply(strsplit(as.character(A2$all.),"年"),"[",2)

View(A2)

A2$month<-lapply(strsplit(as.character(A2$monthday),"月"),"[",1)
A2$day_time<-lapply(strsplit(as.character(A2$monthday),"月"),"[",2)


View(A2)

A2$day<-lapply(strsplit(as.character(A2$day_time),"日"),"[",1)
A2$all3<-lapply(strsplit(as.character(A2$day_time),"日"),"[",2)

View(A2)

A2$time<-lapply(strsplit(as.character(A2$all3),"分"),"[",1)
A2$all4<-lapply(strsplit(as.character(A2$all3),"分"),"[",2)
View(A2)
A2$year<-as.numeric(A2$year)
A2$month<-as.numeric(A2$month)
A2$day<-as.numeric(A2$day)


A2 <- data.frame(lapply(A2), as.numeric)
write.csv(A2,"accdidentA2_105.csv")
#-----------------------------------------------








help("write.csv")









accdidentA1$發生地點
fb_page_test$new_message <- sapply(strsplit(fb_page_test$message,sep),"[",1)


ip_split <- strsplit(ip, ".", fixed = TRUE)
ip_split[1]

strsplit(as.character(x), ",")







