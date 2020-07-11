# Analysis of select Kilts/Dominics Data

library(haven)
library(gamlr)
library(dplyr)
library(reshape2)
library(ggplot2)

setwd("D:/UChicago 2018-19/Spring 2019/SOSC 13300/research")

ctrl <- read_dta("control.dta")
juice <- read_dta("juice2.dta")
cheese <- read_dta("cheese2.dta")
btissue <- read_dta("btissue2.dta")
tooth <- read_dta("tooth2.dta")

ctrl$tier=NA

high=c(2,12,14,32,33,52,53,62,68,71,72,75,93,95,97,100,69,104,106,109,111,123,124,128,130,131)
med=c(4,5,9,28,44,45,47:51,54,56,64,65,74,76,81,84,86,88,89,90:92,98,101,105,107,110,113:119,121,126,129,133,134,136)
low=c(8,18,46,67,73,94,102,103,112,132)
cub=c(21,40,59,70,77,78,80,83,122)

for(i in 1:100){
  if(ctrl$store[i] %in% high){
    ctrl$tier[i]=4
  }else if (ctrl$store[i] %in% med){
    ctrl$tier[i]=3 
  }else if (ctrl$store[i] %in% low){
    ctrl$tier[i]=2 
  }else if (ctrl$store[i] %in% cub){
    ctrl$tier[i]=1
  }else{
    ctrl$tier[i]=NA
  }
}

perish<-rbind(juice,cheese) %>%
  mutate(perishable=1)

nonperish<-rbind(btissue,tooth) %>%
  mutate(perishable=0)

fin1<-merge(x=ctrl,y=perish,by=c("store")) #issue with merge on cheese
fin2<-merge(x=ctrl,y=nonperish,by=c("store"))
fin<-rbind(fin1,fin2)

#fixing pct_diff
data <- fin %>%
  mutate(pct_diff = -1*pct_diff)

data$tier=as.factor(data$tier) 

tier=model.matrix(~.,data=as.data.frame(data[,10]))[,-1]
colnames(tier)=c("low","med","high")

data=cbind(data,tier)

################################
# Heatmap of Pairwise Relations
################################

cormat=cor(cbind(data,tier)[,-c(1,10:13)])
melted_cormat <- melt(cormat, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0, 
                                   size = 10, hjust = 1))+
  coord_fixed()

#######################
#  clustering stores
#######################

df=ctrl[-which(is.na(ctrl$ethnic)),] #removing stores that have no info

# PCA

pc=prcomp(df[,-c(1,10)],scale=T) #remove store num/categoricals
summary(pc)
t(round(pc$rotation[,1:3],2))
cat("PC1 top 3 loadings by magnitude")
round(pc$rotation[,1][order(abs(pc$rotation[,1]),decreasing=T)][1:3],2)
cat("PC2 top 3 loadings by magnitude")
round(pc$rotation[,2][order(abs(pc$rotation[,2]),decreasing=T)][1:3],2)
cat("PC3 top 3 loadings by magnitude")
round(pc$rotation[,3][order(abs(pc$rotation[,3]),decreasing=T)][1:3],2)

par(mfrow=c(2,2))
plot(pc,"PCA on Demographics",type="line")
plot(pc$x[,1],pc$x[,2],xlab="PC1",ylab="PC2",type="n")
text(pc$x[,1],pc$x[,2],
     labels=df$store,cex=0.7,col=df$tier)#,col=color_list)

plot(pc$x[,1],pc$x[,3],xlab="PC1",ylab="PC3",type="n")
text(pc$x[,1],pc$x[,3],
     labels=df$store,cex=0.7,col=df$tier)#,col=color_list)

plot(pc$x[,2],pc$x[,3],xlab="PC2",ylab="PC3",type="n")
text(pc$x[,2],pc$x[,3],
     labels=df$store,cex=0.7,col=df$tier)#,col=color_list)
legend("topright",
       legend=c("high","medium","low","cubfighter"),
       col=c(4,3,2,1),
       pch=19)
par(mfrow = c(1, 1))

################
# causal LASSO
################

#new data
data=data[-which(is.na(data$pct_diff)),]

y=log(data$pct_diff+1)

d=data$perishable #18

dfx=data[,-c(1,10:13,17)] #store1,tier10,upc11,y13,d17,promonum12

x=sparse.model.matrix(~.,data=dfx)[,-1] 

treat <- gamlr(x,d,lambda.min.ratio=1e-4,family="binomial") 
dhat <- predict(treat, x, type="response")

dev0=treat$deviance[1]
dev1=treat$deviance[which.min(AICc(treat))]
R2=1-dev1/dev0
cat("I.S. AICc R2 is",R2)

causal <- gamlr(cbind(d,dhat[,1],x),y,free=2,lmr=1e-4)
coefc=coef(causal)
cat("the coefficient of d in our causal LASSO is",coefc["d",1])
plot(causal,main="Causal LASSO of d=Perishability")

# CROSS VALIDATION
####################

cvlasso=cv.gamlr(cbind(d,x),y,verb=T,nfold=10)
plot(cvlasso)

cv.aicc=coef(cvlasso,select="min") #aicc

sum(cv.aicc!=0) #56
cv.aicc #coefs

coefcv <- data.frame('var' = rownames(cv.aicc), 
                     'Coef' = as.numeric(as.matrix(cv.aicc))) 
int=coefcv$Coef[1]
coefcv$exp=NA

for(i in 2:16){
  coefcv[i,4]=exp(int+coefcv$Coef[i])-exp(int)
  coefcv[i,3]=exp(int+coefcv$Coef[i])/exp(int)
}

cvlasso$seg.min
cvlasso$seg.1se
summary(cvlasso) #oos in seg100

##############
# CART trees
##############

library(randomForest)
library(tree)
library(rpart)
dftr=data
dftr$logy=log(1+dftr$pct_diff)
dftr=dftr[,-c(1,10:13)]

ltrmod=rpart(data$pct_diff~avgcustcount+ethnic+educ+nocar+income+hsizeavg+unemp+retired+storefreq+avgduration+catdiscount+perishable+low+med+high,
             data=dftr) 
plot(ltrmod, compress = TRUE)
text(ltrmod, cex = 0.9, use.n = TRUE, fancy = T, all = TRUE)

plotcp(ltrmod)
printcp(ltrmod)

treepred=predict(ltrmod,dftr[,-16])
plot(data$pct_diff,treepred)

pruned=prune.rpart(ltrmod,cp=0.01)
plot(pruned)
text(pruned)

##########################
# Random Forest Analysis
##########################

lrfmod=randomForest(data$pct_diff~avgcustcount+ethnic+educ+nocar+income+hsizeavg+unemp+retired+storefreq+avgduration+catdiscount+perishable+low+med+high,
                    data=dftr,
                    importance=T)

plot(lrfmod)
varImpPlot(lrfmod,  type=1, pch=21, bg="navy", main='RF variable importance')

predrf1=predict(lrfmod,dftr[-16],type="response")
plot(data$pct_diff,predrf1,xlab="pct_diff",ylab="RF prediction")
abline(col=2,a=0,b=1)

summary(lm(data$pct_diff~predrf1))

lrfmod2=randomForest(logy~avgcustcount+ethnic+educ+nocar+income+hsizeavg+unemp+retired+storefreq+avgduration+catdiscount+perishable+low+med+high,
                     data=dftr,
                     importance=T)
varImpPlot(lrfmod2,  type=1, pch=21, bg="navy", main='RF variable importance')

predrf=predict(lrfmod2,dftr[-16],type="response")
plot(dftr$logy,predrf,xlab="log(pct_diff+1)",ylab="RF prediction")
abline(col=2,a=0,b=1)
abline(lm(dftr$logy ~ predrf),col="blue")
