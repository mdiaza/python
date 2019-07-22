########################################
#M.Sc. Richard F. Fernández Vásquez
########################################

########################################
#Modelos Supervisados I
########################################

library(foreign)
telco <- read.csv("telco.csv",header=T,sep = ",")

head(telco)
str(telco)
summary(telco)

attach(telco)

#Análisis Bivariado

par(mfrow=c(2,4))
plot(Portabilidad,Producto,xlab="Portabilidad",ylab="Producto")
plot(Portabilidad,Estado_Civil,xlab="Portabilidad",ylab="Estado_Civil")
plot(Portabilidad,Sexo,xlab="Portabilidad",ylab="Sexo")
plot(Portabilidad,NSE_Cat,xlab="Portabilidad",ylab="NSE")
plot(Portabilidad,Segmento_Cat,xlab="Portabilidad",ylab="Segmento")
plot(Portabilidad,Prom_Flag_Premier_Cat,xlab="Portabilidad",ylab="Flag_Premier")
plot(Portabilidad,Edad_Cat,xlab="Portabilidad",ylab="Edad")
plot(Portabilidad,Plan_Cat,xlab="Portabilidad",ylab="Plan")
par(mfrow=c(1,1))

chisq.test(Portabilidad,Producto)
chisq.test(Portabilidad,Estado_Civil)
chisq.test(Portabilidad,Sexo)
chisq.test(Portabilidad,NSE_Cat)
chisq.test(Portabilidad,Segmento_Cat)
chisq.test(Portabilidad,Prom_Flag_Premier_Cat)
chisq.test(Portabilidad,Edad_Cat)
chisq.test(Portabilidad,Plan_Cat)

prueba_chi1=function(data)
{
  k<-length(data)
  pval=rep(0,k-1)
  for(i in 1:(k-1))
    pval[i] <- chisq.test(data[,k],data[,i])$p.value
  Variables<-names(data[-k])
  p.value<-pval*100
  print(data.frame(Variables,p.value))
}

prueba_chi1(telco[,2:10])

prueba_chi2=function(data)
{
  k<-length(data)
  stat=rep(0,k-1)
    for(i in 1:(k-1))
    stat[i] <- chisq.test(data[,k],data[,i])$statistic
  Variables<-names(data[-k])
  statistic<-stat
  print(data.frame(Variables,statistic))
}

prueba_chi2(telco[,2:10])

#Partición de la muestra
flag_muestra <- sample(2, nrow(telco), replace=TRUE, prob=c(0.70, 0.30))
telco <- cbind(telco,flag_muestra)
head(telco)
str(telco)
summary(telco)
train<-subset(telco[,2:10],flag_muestra==1)
test<-subset(telco[,2:10],flag_muestra==2)
nrow(train)
nrow(test)

#Selección de variables
ggplot(train, aes(x = Portabilidad,fill = Producto)) +
  geom_bar(position = "fill") +
  ylab("Porcentaje Producto") +
  xlab("Portabilidad") +
  ggtitle("Porcentaje Producto por Portabilidad")

library(Boruta)
boruta_output <- Boruta(Portabilidad ~ ., data=na.omit(train), doTrace=2)  # perform Boruta search
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance
boruta_signif

importancia <- data.frame(importancia = apply((data.frame(boruta_output$ImpHistory)),2,mean))
decision <- data.frame(decision = boruta_output$finalDecision)

####################
#REGRESIÓN LOGÍSTICA
####################

modelo1<-glm(Portabilidad ~ Producto + NSE_Cat,data=train,family=binomial(link="logit"))
summary(modelo1)

#Predicción
pred1 <- predict(modelo1,test,type="response")
pred1_Est <- ifelse(pred1>0.5,"Portado","No Portado")
head(data.frame(test,pred1,pred1_Est))

xtab1 <- table(test[, "Portabilidad"],pred1_Est)   # Matriz de Confusión
xtab1

library(caret)
acc_1 <- confusionMatrix(xtab1,positive = "Portado")
acc_1

library("ROCR")
pred_1 <- prediction(pred1,test$Portabilidad)
perf_1 <- performance(pred_1,"tpr","fpr")
auc <- 100*as.numeric(performance(pred_1 ,"auc")@y.values)
auc
gini <- 2*(auc-50)
gini
plot(perf_1,type='o', main = paste('Area Bajo la Curva =',round(auc,2),'%'),col="blue")  
abline(a=0, b= 1,col="red")

#################
#REGRESIÓN PROBIT
#################

modelo2<-glm(Portabilidad ~ Producto + NSE_Cat,data=train,family=binomial(link="probit"))
summary(modelo2)

#Predicción
pred2 <- predict(modelo2,test,type="response")
pred2_Est <- ifelse(pred2>0.5,"Portado","No Portado")
head(data.frame(test,pred2,pred2_Est))

xtab2 <- table(test[, "Portabilidad"],pred2_Est)   # Matriz de Confusión
xtab2

library(caret)
acc_2 <- confusionMatrix(xtab2,positive = "Portado")
acc_2

library("ROCR")
pred_2 <- prediction(pred2,test$Portabilidad)
perf_2 <- performance(pred_2,"tpr","fpr")
auc <- 100*as.numeric(performance(pred_2 ,"auc")@y.values)
auc
gini <- 2*(auc-50)
gini
plot(perf_2,type='o', main = paste('Area Bajo la Curva =',round(auc,2),'%'),col="blue")  
abline(a=0, b= 1,col="red")


####################
#KNN
####################

library(caret)
library(e1071)
x <- data.frame(Portabilidad=train$Portabilidad)
y <- data.frame(Producto = train$Producto,NSE_Cat = train$NSE_Cat)

tuned <- tune.knn(x = x,y = y, 
                  k = 1:20,
                  tunecontrol = tune.control(sampling = "boot"),
                  cross=10)

modelo4 <- knn3(Portabilidad ~ Producto + NSE_Cat, data=train, k=10)

#Predicción
pred4 <- predict(modelo4,test)[,2]
pred4_Est <- predict(modelo4,test,type="class")
head(data.frame(test,pred4,pred4_Est))

xtab4 <- table(test[, "Portabilidad"],pred4_Est)   # Matriz de Confusión
xtab4

library(caret)
acc_4 <- confusionMatrix(xtab4,positive = "Portado")
acc_4

library("ROCR")
pred_4 <- prediction(pred4,test$Portabilidad)
perf_4 <- performance(pred_4,"tpr","fpr")
auc <- 100*as.numeric(performance(pred_4 ,"auc")@y.values)
auc
gini <- 2*(auc-50)
gini
plot(perf_4,type='o', main = paste('Area Bajo la Curva =',round(auc,2),'%'),col="blue")  
abline(a=0, b= 1,col="red")


####################
#SVM
####################

library(e1071)
tuned <- tune.svm(Portabilidad ~ Producto + NSE_Cat, data=train, 
         gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)
tuned

modelo5 <- svm(Portabilidad ~ Producto + NSE_Cat, data=train,
               method="C-classification",
               kernel="radial", 
               gamma = 0.01, cost = 10,
               #cross=5, 
               probability=TRUE)

t(modelo5$coefs)%*% modelo5$SV

#Predicción
pred5 <- attr(predict(modelo5,test,probability = TRUE),"probabilities")[,1]
pred5_Est <- predict(modelo5,test,type="class")
head(data.frame(test,pred5,pred5_Est))

xtab5 <- table(test[, "Portabilidad"],pred5_Est)   # Matriz de Confusión
xtab5

library(caret)
acc_5 <- confusionMatrix(xtab5,positive = "Portado")
acc_5

library("ROCR")
pred_5 <- prediction(pred5,test$Portabilidad)
perf_5 <- performance(pred_5,"tpr","fpr")
auc <- 100*as.numeric(performance(pred_5 ,"auc")@y.values)
auc
gini <- 2*(auc-50)
gini
plot(perf_5,type='o', main = paste('Area Bajo la Curva =',round(auc,2),'%'),col="blue")  
abline(a=0, b= 1,col="red")


####################
#REDES NEURONALES
####################

library(nnet)
tuned <- tune.nnet(Portabilidad ~ Producto + NSE_Cat, data=train, 
                  size = 1:4, decay = 10^(-6:-1))
summary(tuned)
tuned

modelo6 <- nnet(Portabilidad ~ Producto + NSE_Cat, 
                data=train,size=1, decay=0.000001, maxit=500)

#Predicción
pred6 <- as.numeric(predict(modelo6,test))
pred6_Est <- predict(modelo6,test,type="class")
head(data.frame(test,pred6,pred6_Est))

xtab6 <- table(test[, "Portabilidad"],pred6_Est)   # Matriz de Confusión
xtab6

library(caret)
acc_6 <- confusionMatrix(xtab6,positive = "Portado")
acc_6

library("ROCR")
pred_6 <- prediction(pred6,test$Portabilidad)
perf_6 <- performance(pred_6,"tpr","fpr")
auc <- 100*as.numeric(performance(pred_6 ,"auc")@y.values)
auc
gini <- 2*(auc-50)
gini
plot(perf_6,type='o', main = paste('Area Bajo la Curva =',round(auc,2),'%'),col="blue")  
abline(a=0, b= 1,col="red")





