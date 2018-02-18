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
for (i in 1:ncol(data)) {
  if (nrow(dataPrice[is.na(data[, i]),]) != 0) {
    print(paste(nrow(dataPrice[is.na(data[, i]),]), colnames(data[i]), sep =" "))
   }
}
colnames(data)
print(paste(nrow(dataPrice[is.na(data[, 2]),]),colnames(data[,2])))
print(paste(nrow(dataPrice[is.na(data[, 2]),]),colnames(data[,2])))
colnames(data[2])
data1=data
aggr(data,combined=T)
 ###fix values
data$WithPool=1
data$WithPool[data$PoolArea==0]=0
table(data$PoolQC)
data$PoolQC[data$PoolArea==0]="None"
data$WithFence=1
data$WithFence[is.na(data$Fence)]=0
data$Fence[is.na(data$Fence)]="None"
 table(data$Alley)
 data$Withmis=1
 data$Withalley[is.na(data$MiscFeature)]=0
 data$MiscFeature[is.na(data$MiscFeature)]="None"
 data$Withalley=1
 data$Withalley[is.na(data$Alley)]=0
 data$Alley[is.na(data$Alley)]="None"
 data$Withfire=0
 data$Withfire[data$Fireplaces!=0]=1
 data$FireplaceQu[data$Withfire==0]="None"
 data$GarageCars[is.na(data$GarageCars)]=mean(data$GarageCars[-2577])
 table(data$GarageCars)
data$Withgargae=0 
table(data$GarageQual)
data$Withgargae[data$GarageCars!=0]=1
data$GarageArea[data$GarageCars==0]=0
data$GarageType[data$GarageCars==0]="None"
data$GarageYrBlt[data$GarageCars==0]="None"
data$GarageFinish[data$GarageCars==0]="None"
data$GarageCond[data$GarageCars==0]="None"
data$GarageQual[data$GarageCars==0]="None"                                     



table(data$MasVnrType)
MasVnrType.model=rpart(MasVnrType~MasVnrArea,data1)
rpart.plot(MasVnrType.model)
data1$MasVnrType[is.na(data1$MasVnrType)]=predict(MasVnrType.model,data1[is.na(data1$MasVnrType),])
data1$MasVnrType=as.factor(data1$MasVnrType)
table(data$MasVnrType)
data[is.na(data$MasVnrType),c("MasVnrArea","MasVnrType")]
head(data$MasVnrType,30)
data$MasVnrType[data$MasVnrArea==0&is.na(data$MasVnrType)]="None"
data[is.na(data$MasVnrType),c(25:27)]
data$MasVnrType[is.na(data$MasVnrArea)]="None"
data$MasVnrArea[is.na(data$MasVnrArea)]=0
data$MasVnrType[is.na(data$MasVnrType)]="BrkFace"
table(data$BsmtFinSF1)
data$BsmtFinSF1[is.na(data$BsmtFinSF1)]=0
data$BsmtQual[is.na(data$BsmtQual)]="None"
data$BsmtCond[is.na(data$BsmtCond)]="None"
data$BsmtExposure[data$BsmtCond=='None']="None"
data$BsmtFinType1[data$BsmtCond=='None']="None"
data$BsmtFinType2[data$BsmtCond=='None']="None"
data$BsmtFinSF2[data$BsmtCond=='None']=0
data$WithBsmt=1
data$WithBsmt[data$BsmtCond=='None']=0
data[is.na(data$BsmtQual)]
table(data$BsmtQual)
data$BsmtCond=as.factor(data$BsmtCond)
data$BsmtQual=as.factor(data$BsmtQual)
data$BsmtExposure=as.factor(data$BsmtExposure)
data$BsmtFinType1=as.factor(data$BsmtFinType1)
data$BsmtFinType2=as.factor(data$BsmtFinType2)
Bsmt.model=rpart(BsmtQual~BsmtFinType1+BsmtFinType2+BsmtExposure+BsmtCond+BsmtFinSF1+BsmtFinSF2,data,cp=0.005,method='class')
rpart.plot(Bsmt.model)
data1=data
data1$BsmtQual=predict(Bsmt.model,data1[is.na(data$BsmtQual)])
 data$LotShape=as.factor(data$LotShape)
 data$LotConfig=as.factor(data$LotConfig)
Lot.model=rpart(LotFrontage~LotArea+LotShape+LotConfig,data[data$LotFrontage>1,],method='class',cp=0.01)
rpart.plot(Lot.model)
Lot.model$cptable
str(data1)
table(data$LotFrontage)
data$LotFrontage[data$LotFrontage<1]=predict(Lot.model,data[data$LotFrontage<1,])
