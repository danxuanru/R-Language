#期末報告 - 關聯分析

insurance = read.csv("C:/Users/88692/Desktop/課程/R語言/期末報告/insurance.csv")
dataset = as.data.frame(insurance)
#mean(dataset[,7])
#quantile(dataset[,7])

#charges 分成三種狀態 ----------------------
cost = as.numeric(dataset[,7])
is.numeric(cost)
cost.index1 = which(cost<=5000)
cost.index2 = which(cost<=15000 & cost>5000)
cost.index3 = which(cost>15000)

dataset[cost.index1,7] = "good"
dataset[cost.index2,7] = "not bad"
dataset[cost.index3,7] = "too much!!"

#bmi 分成六種狀態 -------------------------
bmi = as.numeric(dataset[,3])
bmi.index1 = which(bmi<18.5)
bmi.index2 = which(bmi>=18.5 & bmi<24)
bmi.index3 = which(bmi>=24 & bmi<27)
bmi.index4 = which(bmi>=27 & bmi<30)
bmi.index5 = which(bmi>=30 & bmi<35)
bmi.index6 = which(bmi>=35)

dataset[bmi.index1,3] = "too thin"
dataset[bmi.index2,3] = "normal"
dataset[bmi.index3,3] = "overweight"
dataset[bmi.index4,3] = "obesity"
dataset[bmi.index5,3] = "moderate obesity"
dataset[bmi.index6,3] = "severe obesity"

#age 分四種狀態 --------------------------
age = as.numeric(dataset[,1])
age.index1 = which(age<20)
age.index2 = which(age>=20 & age<40)
age.index3 = which(age>=40 & age<60)
age.index4 = which(age>=60)  

dataset[age.index1,1] = "under20"
dataset[age.index2,1] = "young adults"
dataset[age.index3,1] = "middle-aged"
dataset[age.index4,1] = "elder"

#將每欄位資料轉為因子------------------------------------
dataset[,"age"] = as.factor(dataset[,"age"])
dataset[,"sex"] = as.factor(dataset[,"sex"])
dataset[,"bmi"] = as.factor(dataset[,"bmi"])
dataset[,"children"] = as.factor(dataset[,"children"])
dataset[,"smoker"] = as.factor(dataset[,"smoker"])
dataset[,"region"] = as.factor(dataset[,"region"])
dataset[,"charges"] = as.factor(dataset[,"charges"])

str(dataset)

#建立關聯規則
library(arules)
rule = apriori(dataset, 
               appearance = list(default='lhs' ,rhs=c("charges=good","charges=not bad","charges=too much!!")))
inspect(rule)

#去除多餘的關聯規則
sort.rule = sort(rule,by="support") #依據support排序
subset = as.matrix(is.subset(x=sort.rule, y=sort.rule))
# 把這個矩陣的下三角去除，只留上三角的資訊
subset[lower.tri(subset,diag = T)] = NA
# 計算每個column中TRUE的個數，若有一個以上的TRUE，代表此column是多餘的
redundant = colSums(subset, na.rm=T) >= 1
sort.rule = sort.rule[!redundant]
inspect(sort.rule)

#規則視覺化
require(arulesViz)
plot(sort.rule)
plot(sort.rule, method="graph")
plot(sort.rule, method="grouped")
