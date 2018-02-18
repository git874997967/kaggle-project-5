library(rpart.plot)
train = read.csv("train.csv", stringsAsFactors = F)
test = read.csv("test.csv", stringsAsFactors = F)
train$label = 'train'
test$label = 'test'
test$SalePrice = 0
str(train)
str(test)
data = bind_rows(train, test)
aggr(data, combined = T)
missValue = function(data) {
  for (i in 1:ncol(data)) {
    if (nrow(dataPrice[is.na(data[, i]), ]) != 0) {
      print(paste(nrow(dataPrice[is.na(data[, i]), ]), colnames(data[i]), sep =
                    " "))
    }
  }
}

colnames(data)
print(paste(nrow(dataPrice[is.na(data[, 2]), ]), colnames(data[, 2])))
print(paste(nrow(dataPrice[is.na(data[, 2]), ]), colnames(data[, 2])))
colnames(data[2])
data1 = data
aggr(data, combined = T)
###fix values
data$WithPool = 1
data$WithPool[data$PoolArea == 0] = 0
table(data$PoolQC)
data$PoolQC[data$PoolArea == 0] = "None"
data$WithFence = 1
data$WithFence[is.na(data$Fence)] = 0
data$Fence[is.na(data$Fence)] = "None"
table(data$Alley)
data$Withmis = 1
data$Withalley[is.na(data$MiscFeature)] = 0
data$MiscFeature[is.na(data$MiscFeature)] = "None"
data$Withalley = 1
data$Withalley[is.na(data$Alley)] = 0
data$Alley[is.na(data$Alley)] = "None"
data$Withfire = 0
data$Withfire[data$Fireplaces != 0] = 1
data$FireplaceQu[data$Withfire == 0] = "None"
data$GarageCars[is.na(data$GarageCars)] = mean(data$GarageCars[-2577])
table(data$GarageCars)
data$Withgargae = 0
table(data$GarageQual)
data$Withgargae[data$GarageCars != 0] = 1
data$GarageArea[data$GarageCars == 0] = 0
data$GarageType[data$GarageCars == 0] = "None"
data$GarageYrBlt[data$GarageCars == 0] = "None"
data$GarageFinish[data$GarageCars == 0] = "None"
data$GarageCond[data$GarageCars == 0] = "None"
data$GarageQual[data$GarageCars == 0] = "None"



table(data$MasVnrType)
MasVnrType.model = rpart(MasVnrType ~ MasVnrArea, data1)
rpart.plot(MasVnrType.model)
data1$MasVnrType[is.na(data1$MasVnrType)] = predict(MasVnrType.model, data1[is.na(data1$MasVnrType), ])
data1$MasVnrType = as.factor(data1$MasVnrType)
table(data$MasVnrType)
data[is.na(data$MasVnrType), c("MasVnrArea", "MasVnrType")]
head(data$MasVnrType, 30)
data$MasVnrType[data$MasVnrArea == 0 & is.na(data$MasVnrType)] = "None"
data[is.na(data$MasVnrType), c(25:27)]
data$MasVnrType[is.na(data$MasVnrArea)] = "None"
data$MasVnrArea[is.na(data$MasVnrArea)] = 0
data$MasVnrType[is.na(data$MasVnrType)] = "BrkFace"
table(data$BsmtFinSF1)
data$BsmtFinSF1[is.na(data$BsmtFinSF1)] = 0
data$BsmtQual[is.na(data$BsmtQual)] = "None"
data$BsmtCond[is.na(data$BsmtCond)] = "None"
data$BsmtExposure[data$BsmtCond == 'None'] = "None"
data$BsmtFinType1[data$BsmtCond == 'None'] = "None"
data$BsmtFinType2[data$BsmtCond == 'None'] = "None"
data$BsmtFinSF2[data$BsmtCond == 'None'] = 0
data$WithBsmt = 1
data$WithBsmt[data$BsmtCond == 'None'] = 0
data[is.na(data$BsmtQual)]
table(data$BsmtQual)
data$BsmtCond = as.factor(data$BsmtCond)
data$BsmtQual = as.factor(data$BsmtQual)
data$BsmtExposure = as.factor(data$BsmtExposure)
data$BsmtFinType1 = as.factor(data$BsmtFinType1)
data$BsmtFinType2 = as.factor(data$BsmtFinType2)
Bsmt.model = rpart(
  BsmtQual ~ BsmtFinType1 + BsmtFinType2 + BsmtExposure + BsmtCond + BsmtFinSF1 +
    BsmtFinSF2,
  data,
  cp = 0.005,
  method = 'class'
)
rpart.plot(Bsmt.model)
data1 = data
data1$BsmtQual = predict(Bsmt.model, data1[is.na(data$BsmtQual)])
data$LotShape = as.factor(data$LotShape)
data$LotConfig = as.factor(data$LotConfig)
data$LotFrontage[data$LotFrontage < 1] = NA
Lot.model = rpart(LotFrontage ~ LotArea + LotShape + LotConfig,
                  data[!is.na(data$LotFrontage), ],
                  method = 'anova',
                  cp = 0.001)
rpart.plot(Lot.model)
Lot.model$cptable

data[is.na(data$TotalBsmtSF), ]
data$LotFrontage[is.na(data$LotFrontage)] = predict(Lot.model, data[is.na(data$LotFrontage), ])
missValue(data)
aggr(data, combined = T)
table(data$TotalBsmtSF)
edit(data[333, ])
data$FullBath[data$BsmtQual == 'None' &
                data$BsmtQual == 'None' & data$BsmtExposure == 'None']

data$FullBath[is.na(data$FullBath)] = 2
data$BsmtFinType2[is.na(data$BsmtFinType2)] = 'Unf'
data[is.na(data$TotalBsmtSF), ]
data$BsmtHalfBath[is.na(data$BsmtHalfBath)] = 0
table(data$Electrical)
data$Utilities[is.na(data$Utilities)] = 'AllPub'
table(data$Utilities[data$Heating == 'GasA'])
data[is.na(data$GarageArea), ]
table(data$Exterior1st[data$RoofMatl == 'Tar&Grv' &
                         data$RoofStyle == 'Flat' &
                         data$MasVnrType == 'None' &
                         data$HouseStyle == '1Story' & data$Neighborhood == 'Edwards'])
data$Exterior1st[is.na(data$Exterior1st)] = 'BrkComm'
data$Exterior2nd[is.na(data$Exterior2nd)] = 'Brk Cmn'
data$GarageCars = as.integer(data$GarageCars)
table(data$GarageCond[data$GarageCars == 1 &
                        data$GarageType == 'Detchd' &
                        data$YearBuilt < 1930 &
                        data$HouseStyle == '2Story' &
                        data$Neighborhood == 'IDOTRR' &
                        data$WithPool == 0 & data$GarageFinish == 'Unf'])
data$GarageYrBlt[is.na(data$GarageYrBlt)] = 1920
data$GarageFinish[is.na(data$GarageFinish)] = 'Unf'
data$GarageQual[is.na(data$GarageQual)] = 'Fa'
data$GarageCond[data$GarageCond == 'Ta'] = 'TA'
data[is.na(data$PoolQC), ]
Pool.model = rpart(
  PoolQC ~ PoolArea + YearBuilt + HouseStyle + OverallQual + OverallCond +
    Fence + BldgType,
  data = data[!is.na(data$PoolQC), ],
  method = 'class',
  cp = 0.0001
)
rpart.plot(Pool.model)
Pool.model$cptable
data$PoolQC[data$PoolArea < 72] = 'None'
table(data$GarageArea[data$GarageCond == 'TA' &
                        data$GarageCars == 1 &
                        data$GarageType == 'Detchd' &
                        data$YearBuilt < 1930 &
                        data$HouseStyle == '2Story' &
                        data$Neighborhood == 'IDOTRR' &
                        data$GarageQual == 'Fa' & data$GarageFinish == 'Unf'])
data$GarageArea[is.na(data$GarageArea)] = 185
data[is.na(data$Functional), ]

Functional.model = rpart(
  Functional ~ HouseStyle + Condition1 + Condition2 + YearBuilt + Heating +
    WithBsmt + WithPool + WithFence + Withalley + Withfire + Withgargae,
  data = data[!is.na(data$Functional), ],
  method = 'class',
  cp = 0.001
)
rpart.plot(Functional.model)
Functional.model$cptable
data$Functional[is.na(data$Functional)] = 'Typ'
data[is.na(data$KitchenQual), ]
table(data$KitchenQual[data$KitchenAbvGr == 1 &
                         data$BldgType == '1Fam' &
                         data$YearBuilt < 1918 & data$LotArea > 10000 & data$LotArea < 15000])
data$KitchenQual[is.na(data$KitchenQual)] = 'TA'
data$SaleType[is.na(data$SaleType)]='WD'
table(data$MSZoning[data$SaleCondition=='Normal'&data$Neighborhood=='Sawyer'])
data[is.na(data$MSZoning),]
MSZoning.model=rpart(MSZoning~Neighborhood+HouseStyle+LotArea+YearBuilt+Foundation+Heating+Electrical+GarageType+WithPool+WithFence+Withmis+Withalley+Withfire+WithBsmt,data=data[!is.na(data$MSZoning),],method='class',cp=0.001)
rpart.plot(MSZoning.model)
MSZoning.model$cptable
data1[is.na(data$MSZoning)]=predict(MSZoning.model,data[is.na(data$MSZoning),])
table(data1$MSZoning)
data$MSZoning[c(1916,2217,2251)]="RM"
data$MSZoning[2905]="RL"
str(data1)
############factorize
dat <- data.frame(var1 = c("a", "b"),
                  var2 = c("hi", "low"),
                  var3 = c(0, 0.1),
                  stringsAsFactors = FALSE
)
character_vars=lapply(data1,class)=='character'
data1[,character_vars]=lapply(data1[,character_vars],as.factor)
 
str(dat)
factorize=function(data){
  character_vars=lapply(data,class)=='character'
  data[,character_vars]=lapply(data[,character_vars],as.factor)
}
 data1=data
 character_vars=lapply(data,class)=='character'
 data[,character_vars]=lapply(data[,character_vars],as.factor)
 rpart(data$LotArea,data)
plot(density(data$YrSold),xlim=c(2000,2015))
barplot(data$YearBuilt,xlim=c(1900,2020))
data$LotSize=''
data[data$LotArea==8450,]
nrow(data[data$LotArea>20000,])
data$LotSize[data$LotArea<=2500]='VerySmall' 
data$LotSize[data$LotArea>2500&data$LotArea<=5500]='Small' 
data$LotSize[data$LotArea>5500&data$LotArea<=10000]='Normal' 
data$LotSize[data$LotArea>10000&data$LotArea<=15000]='Large' 
data$LotSize[data$LotArea>15000&data$LotArea<=20000]='Huge' 
data$LotSize[data$LotArea>=20000]='Jumbo'
str(data)
data$BuildAge=''
 
 
data$BuildAge[data$YearBuilt<=1925]='VeryOld' 
data$BuildAge[data$YearBuilt>1925&data$YearBuilt<=1935]='Old' 
data$BuildAge[data$YearBuilt>1935&data$YearBuilt<=1960]='Mid' 
data$BuildAge[data$YearBuilt>1960&data$YearBuilt<=1985]='Normal' 
data$BuildAge[data$YearBuilt>1985&data$YearBuilt<=2000]='New' 
data$BuildAge[data$YearBuilt>=2000]='VeryNew'
data$LotSize=as.factor(data$LotSize)
data$BuildAge=as.factor(data$BuildAge)
data$GoodSale=''
data$GoodSale[data$YrSold%in%c(2006:2009)]='Hot'
data$GoodSale[data$YrSold==2010]='Normal'
data1$LotSize=data$LotSize
data1$BuildAge=data$BuildAge
data1$GoodSale=data$GoodSale
data1$GoodSale=as.factor(data1$GoodSale)
####build the model
str(train)
train=data1[data1$label=='train',]
test=data1[data1$label=='test',]
SalePrice.model=ranger(SalePrice~.,train,num.trees = 5000,num.threads = 8,mtry=10)
SalePrice.model2=rpart(SalePrice~.,train,method='anova',cp=0.0001)
rpart.plot(SalePrice.model2)
p1=predict(SalePrice.model,test)
test$SalePrice=p1$predictions
 p2=predict(SalePrice.model2,test)
 test$SalePrice2=p2
sub1=data.frame(Id=test$Id,SalePrice=round(test$SalePrice))
 
sub2=data.frame(Id=test$Id,SalePrice=round(test$SalePrice2))
write.csv(sub1,'sub3.csv')
 
plot(density(sub1$SalePrice),
     main="how age infulence with survive",xlab="",ylab="",ylim=c(0,1e-5))
polygon(density(sub2$SalePrice), col=rgb(1,1,0,0.4), border="blue", lwd=2) 
polygon(density(sub1$SalePrice), col=rgb(1,0,1,0.4), border="red", lwd=2) 

