library(ranger)
library(dplyr)
library(rpart)
library(VIM)
library(mice)
train = read.csv2(file.choose(), stringsAsFactors = F)
t=read.csv(file.choose(), stringsAsFactors = T)
test = read.csv(file.choose(), stringsAsFactors = F)


# attach(data)
# missing=data.frame(train[!is.na(Id),])
# missing$
# detach(data)
# missing
data(sleep, package = 'VIM')#利用VIM包自带的数据sleep
aggr(sleep, combined = T)#可视化形式展现sleep数据的缺失值
str(sleep)#查看sleep的列名、数据量、数据类型
data <-
  mice(
    sleep,
    m = 5,
    method = 'pmm',
    maxit = 100,
    seed = 1
  )#对sleep进行多重插补，m=5表示差补次数，产生5个完整的数据集，seed是一个随机数种子，意思是给每次随机生成的数据一个编号，下次再运行这个编号的时候，重新生成的数据和上一次一样，使得随机结果具有可重复性。
summary(data)
data$imp$Dream#插补结果
data_final <-
  complete(data)#这是另外一种缺失值处理方法，对于缺失值较少，complete方法直接保留完整数据，剔除缺失数据
aggr(data$predictorMatrix, combined = T)#可视化展示complete处理后的数据的是否缺失。
model <- with(data, lm(Dream ~ Sleep + Gest))#通过模型来观测新插补的数据列是否有显著的统计学意义
pooled <- pool(model)#将5组插补结果根据以上统计学显著性，合并为一组。
summary(pooled)
library(caret)
data(mdrr)
table(is.na(mdrrDescr))#
head(mdrrDescr)
head(mdrrClass)
down_mdrr <- nearZeroVar(mdrrDescr)#查看方差较小的变量
new1 <- mdrrDescr[, -down_mdrr]#删除上面判断出来的方差较小变量
hight_cor <- findCorrelation(cor(new1), 0.9)#查看强共线性数据
new2 <- new1[, -hight_cor]#删除强共线性变量
info <- findLinearCombos(new2)#查看多重共线性情况
#以下通过模型如随机 森林进行特征选择
subset <-
  c(20, 30, 40, 50, 60, 70, 80)#定义一个子集，每个数值表示变量个数，用于以下rfe模型判断不同变量个数下，模型的准确性。
ctrl <-
  rfeControl(
    functions = rfFuncs,
    method = "cv",
    verbose = F,
    returnResamp = "final"
  )#method表示检验方式，这里是交叉检验，functions表示模型类型，这里使用随机森林。
profile <-
  rfe(new2, mdrrClass, sizes = subset, rfeControl = ctrl)#进行特征选择
plot(profile)#图形的形式查看多个变量的模型效果最好
profile$optVariables
print(profile)#查看本次特征选择保留下来的变量

 