library(ranger)
library(VIM)
library(dplyr)
library(rpart)
library(rpart.plot)
library(glmnet)
train = read.csv('train.csv', stringsAsFactors = F)
test = read.csv('test.csv', stringsAsFactors = F)


t = rbind(train, test)
round(mean(t$LotFrontage[!is.na(t$LotFrontage)]))
nrow(house[house$LotFrontage == 69,])
is.na(t$LotFrontage) == T
LotName = t[which(is.na(t$LotFrontage)), 1]

table(house$LotFrontage)


test$SalePrice = 0
house = bind_rows(train, test)
summary(house)
# correct values and NA
house$GarageYrBlt[house$GarageYrBlt == 2207] = 2007
missValue = function(data) {
  for (i in 1:ncol(data)) {
    if (nrow(data[is.na(data[, i]),]) != 0) {
      print(paste(nrow(data[is.na(data[, i]),]), colnames(data[i]), sep =
                    " "))
    }
  }
}

##fix na
house$PoolQC[is.na(house$PoolQC)] = "None"
house[house$PoolQC == 'None', c('PoolQC', 'PoolArea')]
house$MiscFeature[is.na(house$MiscFeature)] = "None"
house$Fence[is.na(house$Fence)] = "None"
##take care of the 2577
house[is.na(house$GarageCars), c(
  'GarageCars',
  'GarageType',
  'GarageYrBlt',
  'GarageFinish',
  'GarageQual',
  'GarageCond'
)]
house$GarageType[is.na(house$GarageType)] = "None"
house$GarageYrBlt[is.na(house$GarageYrBlt)] = "None"
house$GarageFinish[is.na(house$GarageFinish)] = "None"
house$GarageQual[is.na(house$GarageQual)] = "None"
house$GarageCond[is.na(house$GarageCond)] = "None"
house$GarageCars[is.na(house$GarageCars)] = 0
house$GarageArea[is.na(house$GarageArea)] = 0
house[is.na(house$FireplaceQu), c('FireplaceQu', 'Fireplaces')]
house$FireplaceQu[is.na(house$FireplaceQu)] = "None"
house[is.na(house$BsmtFinType1), c(
  'BsmtQual',
  'BsmtCond',
  'BsmtExposure',
  'BsmtFinType1',
  'BsmtFinType2',
  'BsmtFinSF1',
  'BsmtFinType2',
  'BsmtFinSF2'
)]
house$BsmtQual[is.na(house$BsmtQual)] = "None"
house$BsmtCond[is.na(house$BsmtCond)] = "None"
house$BsmtExposure[is.na(house$BsmtExposure)] = "None"
house$BsmtFinType1[is.na(house$BsmtFinType1)] = "None"
house$BsmtFinType2[is.na(house$BsmtFinType2)] = "None"
house$BsmtFinSF1[is.na(house$BsmtFinSF1)] = 0
house$BsmtFinSF2[is.na(house$BsmtFinSF2)] = 0
house[is.na(house$BsmtFullBath), c('BsmtFullBath', 'BsmtHalfBath', 'BsmtUnfSF', 'TotalBsmtSF')]
house$BsmtFullBath[is.na(house$BsmtFullBath)] = 0
house$BsmtHalfBath[is.na(house$BsmtHalfBath)] = 0
house$BsmtUnfSF[is.na(house$BsmtUnfSF)] = 0
house$TotalBsmtSF[is.na(house$TotalBsmtSF)] = 0

house[is.na(house$MasVnrArea), c('MasVnrType', 'MasVnrArea')]
house$MasVnrArea[is.na(house$MasVnrArea)] = 0
house$MasVnrType[is.na(house$MasVnrType)] = 'None'
house$Functional[is.na(house$Functional)] = 'Typ'
house[is.na(house$KitchenQual), c('YearRemodAdd', 'MasVnrArea')]
house$KitchenQual[is.na(house$KitchenQual)] = 'TA'
house[is.na(house$Electrical), c('YearBuilt', 'YearRemodAdd', 'LotArea')]
house$Electrical[is.na(house$Electrical)] = 'SBrkr'
house$Exterior1st[is.na(house$Exterior1st)] = 'Wd Sdng'
house$Exterior2nd[is.na(house$Exterior2nd)] = 'Wd Sdng'
house$Alley[is.na(house$Alley)] = 'None'
house$SaleType[is.na(house$SaleType)] = 'WD'
house$MSZoning[c(1916, 2217, 2251)] = "RM"
house$MSZoning[2905] = "RL"
### take care of this
house$LotFrontage[is.na(house$LotFrontage)] = round(mean(house$LotFrontage[!is.na(house$LotFrontage)]))
###
house[house$MasVnrArea != 0 &
        house$MasVnrType == 'None', 'MasVnrArea']
house$MasVnrType[house$MasVnrArea > 262 &
                   house$MasVnrType == 'None'] = 'BrkFace'
house$MasVnrType[house$MasVnrArea == 1 &
                   house$MasVnrType == 'None'] = 'Stone'
house[is.na(house$Utilities), c('YearBuilt', 'YearRemodAdd', 'LotArea')]
house$Utilities[is.na(house$Utilities)] = 'AllPub'
aggr(house)

house$LotSize = ''
# house[house$LotArea==8450,]
# nrow(house[house$LotArea>20000,])
house$LotSize[house$LotArea <= 2500] = 'VerySmall'
house$LotSize[house$LotArea > 2500 &
                house$LotArea <= 5500] = 'Small'
house$LotSize[house$LotArea > 5500 &
                house$LotArea <= 10000] = 'Normal'
house$LotSize[house$LotArea > 10000 &
                house$LotArea <= 15000] = 'Large'
house$LotSize[house$LotArea > 15000 &
                house$LotArea <= 20000] = 'Huge'
house$LotSize[house$LotArea >= 20000] = 'Jumbo'
#str(house)
#add features
house$BuildAge = ''
house$BuildAge[house$YearBuilt <= 1925] = 'VeryOld'
house$BuildAge[house$YearBuilt > 1925 &
                 house$YearBuilt <= 1935] = 'Old'
house$BuildAge[house$YearBuilt > 1935 &
                 house$YearBuilt <= 1960] = 'Mid'
house$BuildAge[house$YearBuilt > 1960 &
                 house$YearBuilt <= 1985] = 'Normal'
house$BuildAge[house$YearBuilt > 1985 &
                 house$YearBuilt <= 2000] = 'New'
house$BuildAge[house$YearBuilt >= 2000] = 'VeryNew'
table(train$MSSubClass)
table(test$MSSubClass)
plot(YearBuilt ~ GarageYrBlt, house[house$GarageYrBlt != 'None',])
house[house$YearBuilt > house$GarageYrBlt, c('YearBuilt', 'GarageYrBlt', 'GarageType')]
house[house$Condition2 != 'Norm', c('Condition1', 'Condition2')]
boxplot(train$SalePrice ~ train$Condition2)
#
house$GoodPlace = 0
house$GoodPlace[house$Condition2 %in% c('PosA', 'PosN')] = 1
##Good
house$GoodEqu = 0
house$GoodEqu[house$OverallQual %in% c(7:10) &
                house$OverallCond %in% c(7:10)] = 5
house$GoodEqu[house$OverallQual %in% c(7:10) &
                house$OverallCond %in% c(4:6)] = 4
house$GoodEqu[house$OverallQual %in% c(4:6) &
                house$OverallCond %in% c(7:10)] = 4
house$GoodEqu[house$OverallQual %in% c(4:6) &
                house$OverallCond %in% c(4:6)] = 3
house$GoodEqu[house$OverallQual %in% c(4:6) &
                house$OverallCond %in% c(1:3)] = 2
house$GoodEqu[house$OverallQual %in% c(1:3) &
                house$OverallCond %in% c(4:6)] = 2
house$GoodEqu[house$OverallQual %in% c(1:3) &
                house$OverallCond %in% c(1:3)] = 1
### train the model
house1 = house

####refine the LotFountage but the result is not good as the mean solution wired???
house$LotFrontage[LotName] = 0
table(house$LotFrontage)
Lot.model = ranger(
  LotFrontage ~ LotArea + LotShape + LotConfig + Neighborhood + Condition1 +
    Condition2 + GrLivArea + Fence + MiscFeature,
  house[house$LotFrontage != 0,],
  num.trees = 1000
)
LotPred = predict(Lot.model, house[house$LotFrontage == 0,])
house$LotFrontage[house$LotFrontage == 0] = round(LotPred$predictions)
table(house$LotFrontage)
###build model again
character_vars = lapply(house, class) == 'character'
house[, character_vars] = lapply(house[, character_vars], as.factor)
new_train = house[house$SalePrice != 0,]
new_test = house[house$SalePrice == 0,]
new_train = new_train[,-1]
set.seed(123)
pre.model = ranger(
  SalePrice ~ .,
  new_train,
  num.trees = 4000,
  num.threads = 8,
  mtry = 3
)
p2 = predict(pre.model, new_test)
new_test$SalePrice = round(p1$predictions)
sub2 = data.frame(Id = new_test$Id, SalePrice = new_test$SalePrice)
write.csv(sub2, 'fin5.csv')

#### so the best one will take all added features with mean LotFountage
empty = lm(log(SalePrice) ~ 1, data = new_train)
full = lm(log(SalePrice) ~ . , data = new_train)
fm.base = log(SalePrice) ~ log(LotArea) + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + YearBuilt + YearRemodAdd + YrSold + ExterQual

lm.for = step(empty,
              scope = list(lower = empty, upper = full),
              direction = "forward")
summary(lm.for)
lm.fine = predict(lm.for, new_test)
write.csv(data.frame(Id = new_test$Id, SalePrice = exp(lm.fine)), 'refine-lm.csv')

###with Lasso regiression????
# # generate the formula
# LASSO_formula = as.formula(log(SalePrice) ~ .)
# 
# # model.matrix  will transfor category to dummy variables
# x = model.matrix(LASSO_formula, new_train)
# y = log(train$SalePrice)
# x
#execute lasso
set.seed(1234)
#lm.lasso = cv.glmnet(x, y, alpha = 1)

###positive skewness use log transform
lm.lasso=cv.glmnet(
  x = model.matrix(as.formula(log(SalePrice) ~ .), new_train),
  y = log(new_train$SalePrice),
  alpha = 1
)
#new_test[,-1] for no Id influence
new_test$SalePrice = 1
test_x = model.matrix(as.formula(log(SalePrice) ~ .), new_test[, -1])
# predict and save
lm.pred = predict(lm.lasso, newx = test_x, s = "lambda.min")
res = data.frame(Id = test$Id, SalePrice = exp(lm.pred))
write.csv(res, file = "price_lasso.csv", row.names = FALSE)
 
# https://zhuanlan.zhihu.com/p/30415389
#use these
# cols = ['OverallQual','GrLivArea', 'GarageCars','TotalBsmtSF', 'FullBath', 'TotRmsAbvGrd', 'YearBuilt']
# https://cosx.org/2016/10/data-mining-1-lasso/
# lm.lasso=cv.glmnet(x=model.matrix(as.formula(log(SalePrice)~.),new_train),
#                      y=log(new_train$SalePrice),
#                      alpha=1)
# predict(lm.lasso,newx=model.matrix(as.formula(log(SalePrice~.),new_test[,-1])
#                                    ),s='lambda.min')

