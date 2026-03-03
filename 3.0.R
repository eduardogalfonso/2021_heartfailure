library(readr)
hf <- read_csv("heart_failure.csv")
View(hf)

library(randomForest)
library(MASS)

dim(hf)
anyNA(hf)
summary(hf)
table(hf$DEATH_EVENT)

par(mar=c(1,1,1,1))
par(mfrow=c(1,7))
boxplot(hf$age, main="Age")
boxplot(hf$creatinine_phosphokinase, main="CPK")
boxplot(hf$ejection_fraction, main="EF")
boxplot(hf$platelets, main="Plat.")
boxplot(hf$serum_creatinine, main="Creat.")
boxplot(hf$serum_sodium, main="Sodium")
boxplot(hf$time, main="Time")

par(mfrow=c(2,3))
hist(hf$age, xlab="Age", include.lowest  = TRUE, labels=FALSE)
hist(hf$creatinine_phosphokinase, xlab="CPK", include.lowest  = TRUE, labels=FALSE)
hist(hf$ejection_fraction, xlab="EF", include.lowest  = TRUE, labels=FALSE)
hist(hf$platelets, xlab="Plat.", include.lowest  = TRUE, labels=FALSE)
hist(hf$serum_creatinine, xlab="Creat.", include.lowest  = TRUE, labels=FALSE)
hist(hf$serum_sodium, xlab="Sodium", include.lowest  = TRUE, labels=FALSE)

hfnodeath <- hf[which(hf$DEATH_EVENT == 0),]
hfdeath <- hf[which(hf$DEATH_EVENT == 1),]

summary(hfnodeath)
summary(hfdeath)

par(mfrow=c(1,2))
boxplot(hfnodeath$time, main="Supervivientes")
boxplot(hfdeath$time, main="Fallecidos")

par(mfrow=c(1,2))
hist(hfnodeath$time, xlab="Supervivientes", include.lowest  = TRUE, labels=FALSE)
hist(hfdeath$time, xlab="Fallecidos", include.lowest  = TRUE, labels=FALSE)

summary(hfnodeath$time)
summary(hfdeath$time)

nrow(hfnodeath)
nrow(hfdeath)

hfnum <- hf[ , c("age", "creatinine_phosphokinase", "ejection_fraction", "platelets", "serum_creatinine", "serum_sodium")]
hfcat <- hf[,c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking")]

plot(hfnum)
cov(hf)
cor(as.matrix(hf))

table(hf$sex)
table(hf$anaemia)
table(hf$diabetes)
table(hf$high_blood_pressure)
table(hf$smoking)

prop.table(table(hf$sex))
prop.table(table(hf$anaemia))
prop.table(table(hf$diabetes))
prop.table(table(hf$high_blood_pressure))
prop.table(table(hf$smoking))

hf2 <- hf[ , -12] #Quitar la variable ???time??? 

mtry<-1
oob_error <- 0

for(mtry in 1:12) 
{
  rf=randomForest(DEATH_EVENT ~ . , data = hf2 ,mtry=mtry,ntree=1000) 
  oob_error[mtry] = rf$mse[1000] 
}
oob_error
par(mfrow=c(1,1))
plot(oob_error, type="b", col="black", lwd=2, xlab="mtry", ylab="Error OOB")


set.seed(154)
forest <-randomForest(DEATH_EVENT~.,ntree=1000,data=hf2, mtry=2, importance=TRUE, sample_replacement=TRUE)
print(forest)
importance(forest)
varImpPlot(forest)

summary(forest$oob.times)

plot(forest, type="l", col="black", main="")

nodes <- treesize(forest)
summary(nodes)
hist(nodes)
boxplot(nodes)

pred <- predict(forest, newdata = EjPred)
print(pred)
