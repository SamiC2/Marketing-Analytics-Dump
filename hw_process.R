rm(list=ls())
library(ggplot2)
library(ggthemes)
library(knitr)
library(broom)
library(GA)

set.seed(0)

cData <- read.csv("device_fullData.csv", header=TRUE)
cData$Price<-as.factor(cData$Price)
kable(cData,caption = "Conjoint data for 10 respondents")

xData <- cData[,-10:-1]
Ratings <- cData[,1:10]

Ratings[,1]
attach(cData)

#preferences for respondent 1
fit <- lm(Ratings[,1]~ Price + BrandOS + Storage + ImageQuality)
summary(fit)

#Best: A 12 oz Stamptown Cold brew coffee, priced at $2.99
#Worst: A 4 oz Blue Cup Latte, priced at $4.29

#attribute importance function
get_importance <- function(xData, fit){
  get_range<-function(attr, df, fit){
    coeff <- c(0)
    for(coef in names(fit$coefficients)){
      if(grepl(attr, coef)){coeff <- c(coeff, as.numeric(fit$coefficients[coef]))}
    }
    
    range <- max(coeff) - min(coeff)
    return(range = range)
  }
  
  
  Ranges <- c()
  for(attr in names(xData)){
    range <- get_range(attr = attr,df = cData, fit = fit)
    Ranges <- c(Ranges, range)
  }
  
  imp <- round(100 * Ranges / sum(Ranges),2)
  Res <- data.frame(names(xData), Ranges,  imp)
  names(Res) <- c('Attribute', 'Range', 'Importance')
  return(Res)
}

#for respondent 1
impTable <- get_importance(xData, fit)
kable(impTable, caption = 'Attributes relative importance', align="lcc", digits=2)

#predicting utility for products
newData <- read.csv("device_Products.csv", 
                    header=TRUE)
newData$Price <- as.factor(newData$Price)
kable(newData,
      caption = "Products in the market and potential new products")

newData_resp1 <- newData
newData_resp1$Expected_utility <- predict(fit, newData_resp1)
kable(newData_resp1,caption = "Expected utility for product 1")

#analyze all respondents
I <- ncol(Ratings)

importances <- matrix(0,nrow = ncol(xData),ncol = I+1)
rownames(importances) <- colnames(xData)
colnames(importances) <- c(paste("Respondent",1:I,sep = "_"),"Average")

for(i in 1:I){
  fit <- lm(Ratings[,i]~ Price + BrandOS + Storage + ImageQuality)
  #print(summary(fit_list[[i]]))
  importances[,i] <- get_importance(xData, fit)[,3]
  newData <- cbind(newData,predict(fit, newData))
  colnames(newData)[ncol(newData)] <- paste("Exp_Ut",i,sep = "_")
}

importances[,I+1] <- rowMeans(importances)
#utility for all respondents
kable(newData, 
      caption = "Expected utility for all respondents for all products")

#importance for all 
kable(importances, 
      caption = "Attribute importance for all respondents and average importance")

Exp_ut <- newData[,6:15]

newData$Scenario_1 <- newData$Available + (1:6==4)
newData$Scenario_2 <- newData$Available + (1:6==5)
newData$Scenario_3 <- newData$Available + (1:6==6)

#computing scenarios 
choice_available <- Exp_ut == matrix(apply(Exp_ut[which(newData$Available==1),],2,max),
                                     nrow = 6,ncol = I,byrow = TRUE)
newData$current_mkt_share<-rowSums(choice_available)/I
choice_1 <- Exp_ut == matrix(apply(Exp_ut[which(newData$Scenario_1==1),],2,max),
                             nrow = 6,ncol = I,byrow = TRUE)
newData$sce_1_mkt_share <- rowSums(choice_1)/I
choice_2 <- Exp_ut == matrix(apply(Exp_ut[which(newData$Scenario_2==1),],2,max),
                             nrow = 6,ncol = I,byrow = TRUE)
newData$sce_2_mkt_share <- rowSums(choice_2)/I
choice_3 <- Exp_ut == matrix(apply(Exp_ut[which(newData$Scenario_3==1),],2,max),
                             nrow = 6,ncol = I,byrow = TRUE)
newData$sce_3_mkt_share <- rowSums(choice_3)/I

#show market share
kable(newData[,-6:-15],
      caption = "Market share for current, and all potential scenarios")

# We can see that in the existing market we predict a 50% market share for the Stamptown coffee, 30% market share for the Moonbucks coffee and 20% for the Blue Cup.
# 
# Also if “Coffee and Donuts” would launch:
#   
#   the $1.49 6oz drip coffee, it would get 30% of the market;
# the $2.99 12 oz latte coffee, it would get 40% of the market;
# the $2.99 8oz cold brew coffee, it would get 20% of the market
# So if “Coffee and Donuts” would be interested in maximizing market share they would launch the $2.99 12 oz latte coffee.

#product line optimization
plData <- read.csv("device_ProductLine.csv", header=TRUE)
plData$Price <- as.factor(plData$Price)
margin <- plData$Margin
kable(plData)

#dframe of expected utilities
status_quo <-apply(Exp_ut[which(newData$Available==1),],2,max)
P <- nrow(plData)
utils <- matrix(0, nrow = I, ncol = P+1)
colnames(utils) <- c("status_quo",paste("Product",1:P,sep = "_"))
rownames(utils) <- paste("Respondent",1:I,sep = "_")
utils[,1] <- status_quo

for(i in 1:I){
  fit <- lm(Ratings[,i]~ Price + BrandOS + Storage + ImageQuality)
  #print(summary(fit_list[[i]]))
  utils[i,2:(P+1)] <- predict(fit, plData)
}
kable(utils)

#profit and objective functions
profit<-function(offered, utils, margin){
  
  offered1<-c(1, offered)
  
  uOffered <- t(apply(utils, 1, function(x)(x * offered1)))
  maxUtil <- apply(uOffered, 1, max)
  
  prodHighest<-matrix(0, nrow(utils), ncol(utils)-1)
  
  for(i in 1:nrow(utils)){
    for(j in 2:ncol(utils)){
      if(uOffered[i, j] == maxUtil[i]) {prodHighest[i,j-1]=1;break}
    }
  }
  profitVec<-apply(prodHighest, 1, function(x){x %*% margin})
  sum(profitVec)
}

obj<-function(offered, utils, margin, numProd){
  
  pr<-profit(offered, utils, margin)
  penalty<-10*max(margin)*abs(sum(offered)-numProd)
  
  pr-penalty
}

#optimize with ga, find top 3 products
RNGkind(sample.kind='Rejection')
set.seed(0)
gaOpt<-ga("binary", fitness=obj, utils=utils, margin=margin, numProd=3, nBits=length(margin), maxiter=100)
summary(gaOpt)
sol <- gaOpt@solution
sol
print(paste("Number of solutions:",nrow(sol)))
kable(plData[which(c(sol[1,])==1),])
plot(gaOpt)

#get optimal profits achieved with product line opt
t(apply(utils, 1, function(x)(x * c(1,sol[1,]))))
profit(sol[1,], utils, margin)
