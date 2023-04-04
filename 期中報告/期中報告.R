# 期中報告

memory.size()
memory.limit()
gc()
 
install.packages("rjson")
library(rjson)
library(base)  #!!
searowdata = fromJSON(file = "C:/Users/88692/Desktop/課程/R語言/C-B0048-001.json")
seadata = searowdata$Cwbopendata$dataset$location

seadata[[1111]][[4]]

#sea_dataframe2 = lapply(seadata,function(j){as.data.frame(j)})

#dataunit =  'cm' 

#因為原本的檔案格式無法直接轉DF (or其實有方法我查不到?)
#先建立一個新的DF 再將資料要的擷取進去
sea_dataframe=t(data.frame(seadata[[1]][[4]]))
colnames(sea_dataframe)=c(seadata[[1]][[3]])
row.names(sea_dataframe) = NULL
#sea_dataframe = data.frame(sea_dataframe)  #???

#seadata$SiteId = '1486' 高雄站
i=1
for (i in c(1:3264)) {
  if (seadata[[i]]$SiteId == '1486') {
    newdata = seadata[[i]][[4]]
    sea_dataframe = rbind(sea_dataframe,newdata)
  }
  i=i+1
}

#將原本用來建立DF的資料刪掉
row.names(sea_dataframe) = NULL
sea_dataframe = sea_dataframe[-1,]

rm(searowdata)
rm(newdata)

#---------------------------------------
#讀取csv檔

# install.packages('readr')
# library(readr)
# dataset = read.csv('C:/Users/88692/Desktop/課程/R語言/C-B0048-001.csv')
# #讀不進去??
# 
# #excel剪貼
# readtable.test=function()
# {
#   x=read.table(file = "clipboard",sep = "\t",header = TRUE)
#   print(x) 
# }
# dataset_1486 = readtable.test()
# colnames(dataset_1486) = NULL
# dataname = as.vector(unlist(dataset_1486[1,3:6]))
# colnames(dataset_1486[,7:10]) = c(dataname)
# 
# class(dataname)


#---------------------------------------------
#去除年平均資料 (00月的資料)
yeardata = 13* c(1:20)
sea_df_year = sea_dataframe[yeardata,]
sea_dataframe = sea_dataframe[-yeardata,]



#dataset = list(seadata[[1]]$SiteName,seadata[[1]]$SiteId,sea_dataframe)

#==========================================================
#基本敘述統計  -  計算平均潮位

library('MASS')
#class(sea_dataframe[,2])
tide_level = unlist(sea_dataframe[,2])
tide_level = as.numeric(tide_level)  
#list無法容納double 所以將潮位資料單獨拉出來
#轉成numeric就可以做mean,quantile  ???

library('Hmisc')
attach(sea_df)
summary(mean_tide_level)
describe(mean_tide_level)

mean(mean_tide_level)
median(mean_tide_level)
sd(mean_tide_level)
var(mean_tide_level)
range(mean_tide_level)
quantile(mean_tide_level)

#直方圖 hist
hist(x=tide_level,main = "Histogram of tide level")


#-------------------------------------------------
#處理資料

#歷年下來的潮位變化  sea_df

sea_df = data.frame(sea_dataframe[,1:2])
names(sea_df)=c('period','mean_tide_level')

#要將時間的數據轉為連續的數值
sea_df[,1] = as.numeric(sea_df[,1])
del = c(0)  #將有NULL值的列號存入一個vector
j=1
p = length(sea_df[,1])
for (j in c(1:p) ) {
  sea_df[j,1] = 2000+(j-1)/12
  if(is.null(sea_df[[j,2]][1]) == TRUE) del = append(del, j);
  #if(is.null(sea_df[[j,2]][1]) == TRUE) sea_df[[j,2]][1] = 100;
  j=j+1
}

#刪除有NULL的資料列
del = del[-1]
sea_df = sea_df[-del,]
#將潮位資料轉為numeric  (list不能包含double??)
sea_df[,2] = unlist(sea_df[,2]) 
sea_df[,2] = as.numeric(sea_df[,2])


#若是直接採用年平均資料  (資料太少)


#---------------------------------------------
#一年內的潮位  sea_df2

sea_df2 = data.frame(sea_dataframe[,c(1,1,2)])
names(sea_df2)=c('year','month','tide_level')

#要將時間的數據轉為連續的數值
sea_df2[,1] = as.numeric(sea_df2[,1])
sea_df2[,2] = as.numeric(sea_df2[,2])

k=1
for (k in c(1:p) ) {
  sea_df2[k,1] = sea_df2[k,1]%/%100
  k=k+1
}

k=1
for (k in c(1:p) ) {
  sea_df2[k,2] = k%%12
  if(k%%12==0)  sea_df2[k,2] = 12;
  k=k+1
}

#刪除有NULL的資料列
sea_df2 = sea_df2[-del,]
#將潮位資料轉為numeric
sea_df2[,3] = unlist(sea_df2[,3]) 
sea_df2[,3] = as.numeric(sea_df2[,3])

#---------------------------------------
#將sea_df2依照每年分組
year_factor = factor(sea_df2[,1])
levels(year_factor)
sea_df2[,1] = year_factor
#將sea_df2依照每月分組
month_factor = factor(sea_df2[,2])
levels(month_factor)
sea_df2[,2] = month_factor

#!!!!!分組後會影響線性迴歸分析 因為變成非數值

# rm(i)
# rm(j)
# rm(p)
# rm(k)

#------------------------------------------------
#資料視覺化

#折線圖 type="l" - 歷年的海平面
plot(sea_df, type="l", ann = F, xaxt = "n", yaxt = "n")
axis(1,seq(1995,2025,5),las = 1)
axis(2, las = 2)
title(xlab="year",ylab="mean tide level (cm)", 
      main="line chart of mean tide level")

library(ggplot2) 
#折線圖 geom_line - 近十年的海平面週期
#此方法需要將year轉為factor分組 
ggplot(sea_df2[131:249,], aes(x = month, y = tide_level, color=year)) +geom_line()+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = c(seq(-20,50,10)))

#箱線圖 boxplot
boxplot(tide_level~month, data = sea_df2,main="tide level of year")



ggplot(sea_df2, aes(x=month, y=tide_level))+
  geom_boxplot()



#qplot 散佈圖

#==============================================
#常態檢定

qqnorm(sea_df[,2]);qqline(sea_df[,2], col='Red')

shapiro.test(sea_df[,2])   #p-value>0.05 常態分佈

#hist(x=sea_df[,2],main = "Histogram of tide level",probability = TRUE)
#curve(dnorm(x,mean(sea_df[,2],sd(sea_df[,2]))),add = TRUE,col = "Red")

#===============================================
#線性回歸

#訓練模型
seaLM = lm(mean_tide_level~period, data = sea_df)
seaLOESS = loess(mean_tide_level~period, data = sea_df)
ggplot(sea_df, aes(x = period, y = mean_tide_level))+
  geom_point(shape = 10, size = 3)+geom_smooth(method = lm)+
  labs(title = "simple linear regression",x='year',y='mean tide level (cm)')
#想把年平均資料放進圖中


#資料視覺化----------
plot(sea_df)
model.value = fitted(seaLM)

plot(sea_df)
lines(sea_df$mean_tide_level,model.value,col='Green')
#沒東西??


# ggplot(sea_df, aes(x = period, y = mean_tide_level))+
#   geom_point(shape = 10, size = 1)+geom_smooth(method = loess)+
#   labs(title = "simple linear regression")+geom_line()

summary(seaLM)
summary(seaLOESS)


# #訓練模型  - 曲線模型
# seaLOESS2 = loess(tide_level~month, data = sea_df2)
# seaLM2 = lm(tide_level~month, data = sea_df2)
# ggplot(sea_df2, aes(x = month, y = tide_level))+
#   geom_point(shape = 10, size = 3)+geom_smooth(method = loess)+
#   labs(title = "simple linear regression of year")+
#   scale_x_continuous(breaks = c(seq(0, max(sea_df2$month), 1)) )
# 
# summary(seaLOESS2)

#==================================================
#預測
#2050年平均海平面會上升到31.52683cm
new = data.frame(period=2050)
result = predict(seaLM, newdata = new)
result
ggplot(sea_df, aes(x = period, y = mean_tide_level))+
  geom_point(shape = 10, size = 3)+
  geom_smooth(method = lm)+
  scale_x_continuous(breaks = c(seq(1995, new$period+5, 5)) )+
  scale_y_continuous(breaks = c(seq(-20,50,5)))+
  geom_point(x=new$period, y=result, size=5,shape=17,color="red")
  #超過圖表的預測點顯示不出來


#------------------------------------

# new = data.frame(month=11)
# result = predict(seaLOESS2, newdata = new )
# result
# 
# ggplot(sea_df2, aes(x = month, y = tide_level))+
#   geom_point(shape = 10, size = 3)+
#   geom_point(x=new$month, y=result, size=5,shape=17,color="red")+
#   geom_smooth(method = loess)

#==================================================
#複雜線性回歸

seaLM2 = lm(tide_level~month+year, data = sea_df2)
summary(seaLM2)

new = data.frame(year=2030, month=3)
result = predict(seaLM2, newdata = new)
result

# ggplot(sea_df2, aes(x = month, y = tide_level))+
#   geom_point(shape = 10, size = 3)+
#   geom_point(x=new$month, y=result, size=5,shape=17,color="red")+
#   geom_smooth(method = lm)

#--------------------------------------

# seaLOESS2 = loess(tide_level~month+year, data = sea_df2)
# summary(seaLOESS2)
# 
# result2 = predict(seaLOESS2, newdata = new)
# result2
# 
# ggplot(sea_df2, aes(x = month, y = tide_level))+
#   geom_point(shape = 10, size = 3)+
#   geom_point(x=new$month, y=result, size=5,shape=17,color="red")+
#   geom_smooth(method = loess)


#==================================================