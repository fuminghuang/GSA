data<-read.csv("D:/asiahack/Target2016.csv",header=TRUE,sep=",",encoding = "utf8")
colname<-c("num","station","place","address","city","region","date","subject","a00",'a01',
           "a02",'a03','a04','a05','a06','a07','a08','a09','a10','a11','a12','a13','a14','a15','a16','a17',
           'a18','a19','a20','a21','a22','a23')
colnames(data) <- colname
View(data)

#切出年月日時間的欄位

data$year<-sapply(strsplit(as.character(data$date),"/"),"[[",1)
data$month<-sapply(strsplit(as.character(data$date),"/"),"[",2)
data$day<-sapply(strsplit(as.character(data$date),"/"),"[[",3)
as.numeric(data$day)
as.numeric(data$month)
as.numeric(data$year)


data01<-data.frame()
data01<-subset(data,month=="01")
View(data01)
#北部三個市的資料分別
new_taipei <- subset(data01,city == "新北市")
taipei <- subset(data01,city == "台北市")
keelong <- subset(data01,city == "基隆市")

#把PM先拉出來
new_taipei_pm25<-subset(new_taipei,subject == "RH")
taipei_pm25<-subset(taipei,city == "RH")
keelong_pm25<-subset(keelong,city == "RH")


#要用的資料先存成一個檔案
test<-data.frame(new_taipei_pm25$city,new_taipei_pm25$region,new_taipei_pm25$year,new_taipei_pm25$month,new_taipei_pm25$day,
                 new_taipei_pm25$a00,new_taipei_pm25$a01)
colname_t<-c("city","region","year","month","day","p00","p01")
colnames(test) <- colname_t
test<-data.frame(subset(test,day == "01"))
View(test)

#從新北找出要用的1/1指標
new_taipei
new_taipei_01<-data.frame(subset(new_taipei,day == "01"))
View(new_taipei_01)

test_turn<-data.frame(new_taipei_01$day,new_taipei_01$subject,new_taipei_01$a00,
                      new_taipei_01$a01,new_taipei_01$a02,new_taipei_01$a03,
                      new_taipei_01$a04,new_taipei_01$a05,new_taipei_01$a06,
                      new_taipei_01$a07,new_taipei_01$a08,new_taipei_01$a09,
                      new_taipei_01$a10,new_taipei_01$a11,new_taipei_01$a12,
                      new_taipei_01$a13,new_taipei_01$a14,new_taipei_01$a15,
                      new_taipei_01$a16,new_taipei_01$a17,new_taipei_01$a18,
                      new_taipei_01$a19,new_taipei_01$a20,new_taipei_01$a21,
                      new_taipei_01$a22,new_taipei_01$a23)
colname_t_t<-c("day","subject","t00",'t01',
               "t02",'t03','t04','t05','t06','t07','t08','t09','t10','t11','t12','t13','t14','t15','t16','t17',
               't18','t19','t20','t21','t22','t23')
colnames(test_turn) <- colname_t_t
t(test_turn)
View(test_turn)
#將資料表何在一起
test_all<-merge(test_turn,test,by="day", all = TRUE)

write.csv(test_all,"test_rh.csv")
#台北市PM2.5的模型
test_all<-read.csv("D:/asiahack/test_rh.csv")
as.numeric(test_all$p00)
as.numeric(test_all$t00)
as.numeric(test_all$t01)
as.numeric(test_all$t02)
as.numeric(test_all$t03)
as.numeric(test_all$t04)
as.numeric(test_all$t05)
as.numeric(test_all$t06)
as.numeric(test_all$t07)
as.numeric(test_all$t08)
as.numeric(test_all$t09)
as.numeric(test_all$t10)
as.numeric(test_all$t11)
as.numeric(test_all$t12)
as.numeric(test_all$t13)
as.numeric(test_all$t14)
as.numeric(test_all$t15)
as.numeric(test_all$t16)
as.numeric(test_all$t17)
as.numeric(test_all$t18)
as.numeric(test_all$t19)
as.numeric(test_all$t20)
as.numeric(test_all$t21)
as.numeric(test_all$t22)
as.numeric(test_all$t23)
test_all<-na.omit(test_all)


View(test_all)

pca<-prcomp(formula =~ p00+t00+t01+t02+t03+t04+t05+t06+t07+t08+t09+t10+t11+t12+t13+t14+t15+t16+t17+t18+t19+t20+t21+t22+t23,
            data = test_all,
            center=TRUE,
            scale = TRUE)
pca

pca2=-test_all$t00* 0.2024477-test_all$t01* 0.2058645-test_all$t02* 0.2081298-
  test_all$t03* 0.2093446-test_all$t04* 0.2098870-test_all$t05* 0.2102670-
  test_all$t06* 0.2100047-test_all$t07* 0.2108601-test_all$t08* 0.2116416-
  test_all$t09* 0.2108709-test_all$t10* 0.2075753-test_all$t11*0.2089898-
  test_all$t12*0.2085088-test_all$t13*0.2098421-test_all$t14*0.2086684-
  test_all$t15*0.2105651-test_all$t16*0.2097204-test_all$t17*0.2018287-
  test_all$t18*0.2028864-test_all$t19*0.1961098-test_all$t20*0.1883569-
  test_all$t21*0.1904224-test_all$t22*0.1810162-test_all$t23*0.1800132

pca2=-data$a00* 0.2024477-data$a01* 0.2058645-data$a02* 0.2081298-
  data$a03* 0.2093446-data$a04* 0.2098870-data$a05* 0.2102670-
  data$a06* 0.2100047-data$a07* 0.2108601-data$a08* 0.2116416-
  data$a09* 0.2108709-data$a10* 0.2075753-data$a11*0.2089898-
  data$a12*0.2085088-data$a13*0.2098421-data$a14*0.2086684-
  data$a15*0.2105651-data$a16*0.2097204-data$a17*0.2018287-
  data$a18*0.2028864-data$a19*0.1961098-data$a20*0.1883569-
  data$a21*0.1904224-data$a22*0.1810162-data$a23*0.1800132
pca2
data$a00
model=lm(data$a00~pca2) 
summary(model) 
plot(pca2,test_all$p00) 
abline(lm(test_all$p00~pca2))




