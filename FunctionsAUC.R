doubleAUCfun <- function(xtrain, ytrain,xtest,ytest,s,k) {
  
  yy3<-data.frame(Outcome=ytrain, Predictor=xtrain[,s],Predictor2=xtrain[,k])
  model3<-glm(Outcome~Predictor+Predictor2,data=yy3, family=binomial(link='logit'))
  new3<-data.frame(Predictor=xtest[,s],Predictor2=xtest[,k])
  p3<-predict(model3,new3,type="response")
  new5<-data.frame(Outcome=ytest)
  pr3 <- prediction(p3, new5)
  prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  auc3 <- performance(pr3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  result2<-auc3
}

doubleAUCfunNB <- function(xtrain, ytrain,xtest,ytest,s,k) {
  
  yy3<-data.frame(Outcome=ytrain, Predictor=xtrain[,s],Predictor2=xtrain[,k])
  model4<-naiveBayes(Outcome~Predictor+Predictor2,data=yy3)
  new4<-data.frame(Predictor=xtest[,s],Predictor2=xtest[,k])
  p4<-predict(model4,new4,type="raw")
  #without raw write confusion matrix directly
  prob2<-NULL
  for (i in 1:dim(p4)[1]){
    prob2[i]<-p4[i,2]/p4[i,1]
  }
  new5<-data.frame(Outcome=ytest)
  pr3 <- prediction(prob2, new5)
  prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  auc3 <- performance(pr3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  result2<-auc3
}

doubleAUCfunRFCross <- function(xtrain, ytrain,xtest,ytest,s,k) {
  
  yy3<-data.frame(Outcome=ytrain, Predictor=xtrain[,s],Predictor2=xtrain[,k])
  control <- trainControl(method="repeatedcv", number=10, repeats=3) #Is this the only thing needed for cv
  seed <- 7
  metric <- "Accuracy"
  set.seed(seed)
  #mtry <- 2
  tunegrid <- expand.grid(.mtry=c(1:6))
  set.seed(seed)
  rf_default <- train(Outcome~Predictor+Predictor2,data=yy3, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
  #print(rf_default)
  new4<-data.frame(Predictor=xtest[,s],Predictor2=xtest[,k])
  pred=predict(rf_default, new4,type="prob")
  pred2=predict(rf_default, new4)
  prob2<-NULL
  for (i in 1:dim(pred)[1]){
    prob2[i]<-pred[i,2]/pred[i,1]
  }
  new5<-data.frame(Outcome=ytest)
  pr3 <- prediction(prob2, new5)
  prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  auc3 <- performance(pr3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  result2<-auc3
  
}

doubleAUCfunSVM <- function(xtrain, ytrain,xtest,ytest,s,k) {
  
  yy3<-data.frame(Outcome=ytrain, Predictor=xtrain[,s],Predictor2=xtrain[,k])
  seed <- 7
  set.seed(seed)
  model3<-svm(Outcome~Predictor+Predictor2,data=yy3)
  new3<-data.frame(Predictor=xtest[,s],Predictor2=xtest[,k])
  p3<-predict(model3,new3,decision.values = TRUE)
  p4<-attr(p3,"decision.values")
  new5<-data.frame(Outcome=ytest)
  pr3<- prediction(p4, new5)
  #table(p3, new5)
  #confusionMatrix(p3, new5)
  prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  auc3 <- performance(pr3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  result2<-auc3
}

doubleAUCfunSVMCross <- function(xtrain, ytrain,xtest,ytest,s,k) {
  
  yy3<-data.frame(Outcome=ytrain, Predictor=xtrain[,s],Predictor2=xtrain[,k])
  seed <- 7
  set.seed(seed)
  model4<-svm(Outcome~Predictor+Predictor2,data=yy3,method="C-classification",
              kernel="radial", gamma = 0.01, cost = 100,cross=10, probability=TRUE)
  new4<-data.frame(Predictor=xtest[,s],Predictor2=xtest[,k])
  p4<-predict(model4,new4,decision.values = TRUE)
  p5<-attr(p4,"decision.values")
  new5<-data.frame(Outcome=ytest)
  pr3 <- prediction(p5, new5)
  prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  auc3 <- performance(pr3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  result2<-auc3
}

doublePlusfun <- function(xtrain, ytrain,xtest,ytest,s,k) {
  
  yy3<-data.frame(Outcome=ytrain, Predictor=xtrain[,s],Predictor2=xtrain[,k])
  model3<-glm(Outcome~Predictor+Predictor2,data=yy3, family=binomial(link='logit'))
  new3<-data.frame(Predictor=xtest[,s],Predictor2=xtest[,k])
  p3<-predict(model3,new3,type="response")
  new5<-data.frame(Outcome=ytest)
  pr <- prediction(p3, new5)
  Acc <- performance(pr, measure="acc")
  AccV<-Acc@y.values[[1]][max(which(Acc@x.values[[1]] >= 0.5))]
  Sens <- performance(pr, measure= "sens")
  SensV<-Sens@y.values[[1]][max(which(Sens@x.values[[1]] >= 0.5))]
  Spec <- performance(pr, measure= "spec")
  SpecV<-Spec@y.values[[1]][max(which(Spec@x.values[[1]] >= 0.5))]
  Prec <- performance(pr, measure= "prec")
  PrecV<-Prec@y.values[[1]][max(which(Prec@x.values[[1]] >= 0.5))]
  AllV<-data.frame(Vector=c(AccV, SensV, SpecV, PrecV))
  return(AllV)
}

doubleROCfun <- function(xtrain, ytrain,xtest,ytest,s,k) {
  
  yy3<-data.frame(Outcome=ytrain, Predictor=xtrain[,s],Predictor2=xtrain[,k])
  model3<-glm(Outcome~Predictor+Predictor2,data=yy3, family=binomial(link='logit'))
  new3<-data.frame(Predictor=xtest[,s],Predictor2=xtest[,k])
  p3<-predict(model3,new3,type="response")
  new5<-data.frame(Outcome=ytest)
  pr3 <- prediction(p3, new5)
  prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  result<-prf3
  auc3 <- performance(pr3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  return(result)
  
}

MeansNames <- function(doubleAUCR,trial){
  
  doubleAUCR<-as.data.frame(doubleAUCR)
  doubleAUCR<-mutate(doubleAUCR, Means=rowMeans(doubleAUCR))
  row.names(doubleAUCR)<-trial
  return(doubleAUCR)
}

multipleAUCfun <- function(xtrain,ytrain,xtest,ytest) {
  
  yy3<-data.frame(Outcome=ytrain,xtrain)
  model3<-glm(Outcome~.,data=yy3, family=binomial(link='logit'))
  new3<-data.frame(xtest)
  p3<-predict(model3,new3,type="response")
  new5<-data.frame(Outcome=ytest)
  #pr3 <- prediction(as.numeric(p3), new5)
  pr3 <- prediction(p3, new5)
  prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  auc3 <- performance(pr3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  result2<-auc3
  return(result2)
}

multipleAUCfunNB <- function(xtrain, ytrain,xtest,ytest) {
  
  yy3<-data.frame(Outcome=ytrain,xtrain)
  model4<-naiveBayes(Outcome~.,data=yy3)
  new3<-data.frame(xtest)
  p4<-predict(model4,new3,type="raw")
  #without raw write confusion matrix directly
  prob2<-NULL
  for (i in 1:dim(p4)[1]){
    prob2[i]<-p4[i,2]/p4[i,1]
  }
  new5<-data.frame(Outcome=ytest)
  pr3 <- prediction(prob2, new5)
  prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  auc3 <- performance(pr3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  result2<-auc3
  return(result2)
}

multipleROCfun <- function(xtrain,ytrain,xtest,ytest) {
  
  yy3<-data.frame(Outcome=ytrain,xtrain)
  model3<-glm(Outcome~.,data=yy3, family=binomial(link='logit'))
  new3<-data.frame(xtest)
  p3<-predict(model3,new3,type="response")
  new5<-data.frame(Outcome=ytest)
  pr3 <- prediction(p3, new5)
  prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  auc3 <- performance(pr3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  result2<-auc3
  return(prf3)
  
}

multipleROCfunNB <- function(xtrain, ytrain,xtest,ytest) {
  
  yy3<-data.frame(Outcome=ytrain,xtrain)
  model4<-naiveBayes(Outcome~.,data=yy3)
  new3<-data.frame(xtest)
  p4<-predict(model4,new3,type="raw")
  #without raw write confusion matrix directly
  prob2<-NULL
  for (i in 1:dim(p4)[1]){
    prob2[i]<-p4[i,2]/p4[i,1]
  }
  new5<-data.frame(Outcome=ytest)
  pr3 <- prediction(prob2, new5)
  prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  auc3 <- performance(pr3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  result2<-auc3
  return(prf3)
}


multiplePlusfun <- function(xtrain, ytrain,xtest,ytest) {
  
  yy3<-data.frame(Outcome=ytrain,xtrain)
  model2<-glm(Outcome~.,data=yy3,family=binomial(link='logit'))
  new3<-data.frame(xtest)
  p<-predict(model2,new3,type = "response")
  new4<-data.frame(Outcome=ytest)
  pr <- prediction(p, new4)
  Acc <- performance(pr, measure="acc")
  AccV<-Acc@y.values[[1]][max(which(Acc@x.values[[1]] >= 0.5))]
  Sens <- performance(pr, measure= "sens")
  SensV<-Sens@y.values[[1]][max(which(Sens@x.values[[1]] >= 0.5))]
  Spec <- performance(pr, measure= "spec")
  SpecV<-Spec@y.values[[1]][max(which(Spec@x.values[[1]] >= 0.5))]
  Prec <- performance(pr, measure= "prec")
  PrecV<-Prec@y.values[[1]][max(which(Prec@x.values[[1]] >= 0.5))]
  AllV<-data.frame(Vector=c(AccV, SensV, SpecV, PrecV))
  return(AllV)
  
}

singleAUCfun <- function(xtrain, ytrain,xtest,ytest,s) {
  
  yy2<-data.frame(Outcome=ytrain, Predictor=xtrain[,s])
  model2<-glm(Outcome~Predictor,data=yy2,family=binomial(link='logit'))
  new2<-data.frame(Predictor=xtest[,s])
  p<-predict(model2,new2,type = "response")
  new4<-data.frame(Outcome=ytest)
  pr <- prediction(p, new4)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  return(auc)
}

singlePlusfun <- function(xtrain, ytrain,xtest,ytest,s) {
  
  
  yy2<-data.frame(Outcome=ytrain, Predictor=xtrain[,s])
  model2<-glm(Outcome~Predictor,data=yy2,family=binomial(link='logit'))
  new2<-data.frame(Predictor=xtest[,s])
  p<-predict(model2,new2,type = "response")
  new4<-data.frame(Outcome=ytest)
  pr <- prediction(p, new4)
  Acc <- performance(pr, measure="acc")
  AccV<-Acc@y.values[[1]][max(which(Acc@x.values[[1]] >= 0.5))]
  Sens <- performance(pr, measure= "sens")
  SensV<-Sens@y.values[[1]][max(which(Sens@x.values[[1]] >= 0.5))]
  Spec <- performance(pr, measure= "spec")
  SpecV<-Spec@y.values[[1]][max(which(Spec@x.values[[1]] >= 0.5))]
  Prec <- performance(pr, measure= "prec")
  PrecV<-Prec@y.values[[1]][max(which(Prec@x.values[[1]] >= 0.5))]
  AllV<-data.frame(Vector=c(AccV, SensV, SpecV, PrecV))
  return(AllV)
}

singleROCfun <- function(xtrain, ytrain,xtest,ytest,s) {
  
  yy2<-data.frame(Outcome=ytrain, Predictor=xtrain[,s])
  model2<-glm(Outcome~Predictor,data=yy2,family=binomial(link='logit'))
  new2<-data.frame(Predictor=xtest[,s])
  p<-predict(model2,new2,type = "response")
  new4<-data.frame(Outcome=ytest)
  pr <- prediction(p, new4)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  resultU2<-prf
  tomeany<-as.data.frame(prf@y.values)
  tomeanx<-as.data.frame(prf@x.values)
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  return(resultU2)
  
}



plotAUCSingle <- function(singleAUC, singleAUCR,NumVar,test_name){
  
  AUCT<-as.data.frame(t(singleAUC[1:dim(singleAUC)[2]-1]))
  AUC2T<-as.data.frame(t(singleAUCR[1:dim(singleAUCR)[2]-1]))
  one<-as.data.frame(singleAUC$Means)
  two<-as.data.frame(singleAUCR$Means)
  Mean<-data.frame(one,two)
  Mean<-as.matrix(Mean)
  #
  nm<-names(AUCT)
  for (j in 1:(NumVar-1)){
    Mono<-data.frame(Mono=AUCT[,j],Label=as.factor(c(rep("model",dim(AUCT)[1]))))
    Mono1<-data.frame(Mono=AUC2T[,j],Label=as.factor(c(rep("permuted data",dim(AUC2T)[1]))))
    Mono<-rbind(Mono,Mono1)
    
    pdf(paste0("FigAUC/",test_name,"Time_",j,"_AUCSingle.pdf"),width=8,height=4)
    print(ggdensity(Mono, x = "Mono", fill = "Label", palette = "jco")+geom_vline(xintercept =Mean[j,],linetype = 2,color="black",show.legend = TRUE)+labs(title= paste("Permuted versus real model, density plot of AUC curve of",nm[j]),y=paste("Density",nm[j]),x="AUC values")+
            geom_text(aes(x=as.numeric(Mean[j,1]),y=-0.5,label=signif(Mean[j,1], digits = 2)),size=6.5)+geom_text(aes(x=as.numeric(Mean[j,2]),y=-0.5,label=signif(Mean[j,2], digits = 2)),size=6.5))
    graphics.off()

    
    }
  
  return(Mono)
  
}






plotParamSingle <- function(MatsinglePlus, MatsinglePlusR,NumVar,Data,N,test_name){
  
  #Accuracy
  Mono<-NULL
  MonoR<-NULL
  for (s in 1:(NumVar-1)){
    for (d in 1:4){
      for (j in 1:N){
        Mono1<-data.frame(Mono=MatsinglePlus[s,j][[1]][[d]])
        Mono<-rbind(Mono,Mono1)
        Mono1R<-data.frame(MonoR=MatsinglePlusR[s,j][[1]][[d]])  
        MonoR<-rbind(MonoR,Mono1R)
      }
      MonoLab<-data.frame(Mono=Mono,Label=as.factor(c(rep("model",dim(Mono)[1]))))
      MonoLabR<-data.frame(Mono=MonoR,Label=as.factor(c(rep("permuted data",dim(MonoR)[1]))))
      colnames(MonoLabR)[1]<-c("Mono")
      Means<-data.frame(Mean=mean(MonoLab$Mono),MeanR=mean(MonoLabR$Mono))
      Mono<-rbind(MonoLab,MonoLabR)
      plot3[[d]]<-ggdensity(Mono, x = "Mono", fill = "Label", palette = "jco")+labs(title= paste("Value : ",Data[d], "for", names(xtrain)[s]),x=paste(names(xtrain)[s]))
      #+geom_vline(xintercept=Means[1,],linetype = 2,color="black",show.legend = TRUE)+geom_text(aes(x=as.numeric(Means[1,1]),y=-0.25),label=signif(Means[1,1], digits = 2))+geom_text(aes(x=as.numeric(Means[1,2]),y=-0.25,color="blue"),label=signif(Means[1,2], digits = 2))
      Mono<-NULL
      MonoR<-NULL
      
    }
    pdf(paste0("FigAUC/",test_name,"Time_",s,"_Parameters.pdf"),width=8, height=5)
    print(grid.arrange(plot3[[2]], plot3[[3]],plot3[[4]],plot3[[1]], ncol=2))
    graphics.off()
  }
 
  return(Mono)

}



plotAUCMultiple <- function(multipleAUC, multipleAUCR,val,test_name){
  
  Meanq<-data.frame(multipleAUC$Means,multipleAUCR$Means)
  MA<-data.frame(Mono=t(multipleAUC[1:dim(multipleAUC)[2]-1]))
  MA["Label"]<-as.factor(c(rep("Model",dim(MA)[1])))
  MAR<-data.frame(Mono=t(multipleAUCR[1:dim(multipleAUCR)[2]-1]))
  MAR["Label"]<-as.factor(c(rep("Permuted data",dim(MAR)[1])))
  Mono<-rbind(MA,MAR)
  pp<-ggdensity(Mono, x = "Mono", fill = "Label", palette = "jco")+geom_vline(xintercept=Meanq[1,1],linetype = 2,color="black",show.legend = TRUE)+
    labs(title= paste("AUC Density Plot of GLM Multivariate Model"),y="Density",x="AUC values")+
    geom_text(aes(x=as.numeric(Meanq[1,1]),y=-0.5,label=signif(Meanq[1,1], digits = 2)),size=6.5)+
    geom_text(aes(x=as.numeric(Meanq[1,2]),y=-0.5,label=signif(Meanq[1,2], digits = 2)),size=6.5)
 
  titles <- c("NB","Linear")
  pdf(paste0("FigAUC/",test_name,"Time_",title=titles[val],"_AUCMultiple.pdf"),width=7, height=4)
  plot2 <- print(pp+geom_vline(xintercept=Meanq[1,2],linetype = 2,color="black",show.legend = TRUE))+theme(axis.text=element_text(size=18),axis.title=element_text(size=18),strip.text.x = element_text(size = 18, colour = "black"),
                                                                                                  axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+labs(x="AUC values",y="Density")
  
  graphics.off()
  
  return(Mono)
  }
  

plotAUCMultipleFig <- function(multipleAUC, multipleAUCR,val){
  
  Meanq<-data.frame(multipleAUC$Means,multipleAUCR$Means)
  MA<-data.frame(Mono=t(multipleAUC[1:dim(multipleAUC)[2]-1]))
  #MA["Label"]<-as.factor(c(rep("Model",dim(MA)[1])))
  MAR<-data.frame(Mono=t(multipleAUCR[1:dim(multipleAUCR)[2]-1]))
  # MAR["Label"]<-as.factor(c(rep("Permuted data",dim(MAR)[1])))
  # 
  MA["Label"]<-as.factor(c(rep("Multivariate model",dim(MA)[1])))
  MAR["Label"]<-as.factor(c(rep("NISS alone",dim(MAR)[1])))
  
  Mono<-rbind(MA,MAR)
  pp<-ggdensity(Mono, x = "Mono", fill = "Label", palette = "jco")+geom_vline(xintercept=Meanq[1,1],linetype = 2,color="black",show.legend = TRUE)+
    labs(title= paste("AUC Density Plot of GLM Multivariate Model"),y="Density",x="AUC values")+
    geom_text(aes(x=as.numeric(Meanq[1,1]),y=-0.5,label=signif(Meanq[1,1], digits = 2)),size=6.5)+
    geom_text(aes(x=as.numeric(Meanq[1,2]),y=-0.5,label=signif(Meanq[1,2], digits = 2)),size=6.5)
  
  
  
  titles <- c("NB","Linear")
  
  pdf(paste0("FigAUC/",test_name,"Time_",title=titles[val],"_AUCMultiple.pdf"),width=7, height=4)
  plot2 <- print(pp+geom_vline(xintercept=Meanq[1,2],linetype = 2,color="black",show.legend = TRUE))+theme(axis.text=element_text(size=18),axis.title=element_text(size=18),strip.text.x = element_text(size = 18, colour = "black"),
                                                                                                           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+labs(x="AUC values",y="Density")
  
  graphics.off()
  
  return(plot2)
}



multipleParam <- function(multiplePlus,multiplePlusR,N,test_name){
  


Mono<-NULL
MonoR<-NULL

for (d in 1:4){
  for (j in 1:N){
    Mono1<-data.frame(Mono=multiplePlus[1,j][[1]][[d]])
    Mono<-rbind(Mono,Mono1)
    Mono1R<-data.frame(MonoR=multiplePlusR[1,j][[1]][[d]])  
    MonoR<-rbind(MonoR,Mono1R)
  }
  MonoLab<-data.frame(Mono=Mono,Label=as.factor(c(rep("model",dim(Mono)[1]))))
  MonoLabR<-data.frame(Mono=MonoR,Label=as.factor(c(rep("permuted data",dim(MonoR)[1]))))
  colnames(MonoLabR)[1]<-c("Mono")
  Means<-data.frame(Mean=mean(MonoLab$Mono),MeanR=mean(MonoLabR$Mono))
  Mono<-rbind(MonoLab,MonoLabR)
  plot3[[d]]<-ggdensity(Mono, x = "Mono", fill = "Label", palette = "jco")+labs(title= paste("Value : ",Data[d], "for", names(xtrain)[s]),x=paste(names(xtrain)[s]))
  #+geom_vline(xintercept=Means[1,],linetype = 2,color="black",show.legend = TRUE)+geom_text(aes(x=as.numeric(Means[1,1]),y=-0.25),label=signif(Means[1,1], digits = 2))+geom_text(aes(x=as.numeric(Means[1,2]),y=-0.25,color="blue"),label=signif(Means[1,2], digits = 2))
  Mono<-NULL
  MonoR<-NULL
  
}
pdf(paste0("FigAUC/",test_name,"Time","_ParametersMultiple.pdf"),width=9, height=6)
print(grid.arrange(plot3[[2]], plot3[[3]],plot3[[4]],plot3[[1]], ncol=2))
graphics.off()
return(Mono)
}

multipleAUCfunReg <- function(xtrain,ytrain,xtest,ytest) {
  
  yy3<-data.frame(Outcome=ytrain,xtrain)
  model3<-glm(Outcome~.,data=yy3)
  new3<-data.frame(xtest)
  length(model3$coefficients) > model3$rank #number of cases - 57 
  #has to be true
  p3<-predict(model3,new3)
  new5<-data.frame(Outcome=ytest)
  R2 <-R2(new5,p3)
  #RMSE <- RMSE(new5,p3)
  ADJR2 <- 1 - (1 - R2) * ((length(new5) - 1)/length(new5)-length(model3$coefficients)-1)
  AllV<-data.frame(Vector=c(R2, ADJR2))
  return(R2)
}




singleAUCfunReg <- function(xtrain, ytrain,xtest,ytest,s) {
  
  yy2<-data.frame(Outcome=ytrain, Predictor=xtrain[,s])
  model2<-glm(Outcome~Predictor,data=yy2)
  new2<-data.frame(Predictor=xtest[,s])
  p<-predict(model2,new2)
  new4<-data.frame(Outcome=ytest)
  R2 <-R2(new4,p)
  RMSE <- rmse(as.numeric(unlist(new4)),as.numeric(p))
  ADJR2 <- 1 - (1 - R2) * ((length(new4) - 1)/length(new4)-length(model2$coefficients)-1)
  d<- rmse(new4,p)
  AllV<-data.frame(Vector=c(R2,ADJR2))
  return(R2)
  
}




