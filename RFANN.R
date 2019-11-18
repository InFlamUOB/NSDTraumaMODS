RegMeGeneral <- function(xtrain, ytrain, alpha, nlambda,xtest,ytest){
  
  CVEN=cv.glmnet(xtrain,ytrain,family="binomial",type.measure="class",alpha=alpha, nlambda=nlambda,parallel = TRUE)
  attach(CVEN)
  Lambda.BestEN<-CVEN$lambda.min #can be either minimum or 1 standard deviation
  
  CVFinEN=glmnet(xtrain,ytrain,family="binomial",alpha=alpha,lambda=Lambda.BestEN) 
  CoefEN<-coef(CVFinEN) #Beta coefficients obtained from here
  InterceptEN<-CoefEN@x[1]
  BetasEN<-CVFinEN$beta 
  Betas2EN<-data.frame(Features=BetasEN@Dimnames[[1]][BetasEN@i+1], Coefficients=BetasEN@x) #Beta coefficients names stored here 
  CVPred1EN = predict(CVFinEN, family="binomial", s=Lambda.BestEN, newx = xtest,type="class") #predict in test set to obtain confusion matrix
  ytest2<-as.factor(ytest)
  ResultsEN<-table(CVPred1EN,ytest)
  
  AccuracyEN<-(ResultsEN[1]+ResultsEN[4])/sum(ResultsEN[1:4])
  ErrorEN<-1-AccuracyEN
  
  detach(CVEN)
  return(data.frame(Betas2EN=Betas2EN))}


RegMeFeatures <- function(Betas2EN,BetasFinEN,BNumFinEN){

  LauCoef<-Betas2EN$Coefficients
  LauCoefEN<-data.frame(Coeff=LauCoef,stringsAsFactors=FALSE)
  LauCoef1<-rbind(LauCoef1,LauCoefEN)
  colnames(Betas2EN) <- c("Features","Coefficients")
  BetasTodo<-rbind(BetasTodo,Betas2EN) #store coefficients and store betas
  
  seeEN<-Betas2EN$Features 
  see3EN<-toString(seeEN) 
  
  BetasFinEN[i]<-see3EN #name of features the model used 
  BNumFinEN[i]<-length(seeEN) #number of features the model used 


  return(data.frame(BetasFinEN=BetasFinEN,BNumFinEN=BNumFinEN))
}


RegMeCoefficients <- function(Betas2EN,BetasTodo){
  
  
  colnames(Betas2EN) <- c("Features","Coefficients")
  BetasTodo<-rbind(BetasTodo,Betas2EN) #store coefficients and store betas
  
  return(data.frame(BetasTodo))
}
  
RegMeFrequencies <- function(Betas2EN,see2EN){
  
  colnames(Betas2EN) <- c("Features","Coefficients")
  BetasTodo<-rbind(BetasTodo,Betas2EN) #store coefficients and store betas
  seeEN<-Betas2EN$Features 
  seeEN1<-data.frame(All=seeEN)
  see2EN<-rbind(see2EN,seeEN1)   #all beta names stored
  
  return(see2EN)
}


############




RandomForestMe <- function(Label, train, metric,i,final_col){

metric <- "Accuracy"
rf_default <- train(Label~., data=train, method="rf", metric=metric,importance=TRUE,  allowParallel=TRUE)
importance <- caret::varImp(rf_default, scale=FALSE)

col_index <- varImp(rf_default)$importance %>% 
  mutate(names=row.names(.)) %>%
  select("N","names") 
if (i==1){
  final_col<-col_index
}

final_col <-  inner_join(final_col,col_index,by="names")
return(final_col)
}



###########



ANNMe <- function(Label, train, i, final_col2){

numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
fit2 <- train(Label~., data = train, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)), allowParallel=TRUE)
importance <- caret::varImp(fit2, scale=FALSE)

col_index <- varImp(fit2)$importance %>% 
  mutate(names=row.names(.)) 

colnames(col_index) <- c("N","names")


if (i==1){
  final_col2<-col_index
}


final_col2 <-  inner_join(final_col2,col_index,by="names")

return(final_col2)}




################




processImportance <- function(final_col2){
  
  final_col2<-final_col2[,-c(1)]
  mdata <- melt(final_col2, id=c("names"))
  mdata2 <- data.frame(acast(mdata, variable~names))
  finalRF<-rbind(mdata2,colMeans(mdata2))
  
  finalRF2 <- data.frame(Names=names(finalRF), means=as.numeric(finalRF[dim(finalRF)[1],]))
  Final_Plot_RfA <- finalRF2 %>% arrange(-means)
  

  
return(Final_Plot_RfA)
  }

processRegularization <- function(see2,filter){
  
  m<-count(see2, All)
  m<-m[-1,]
  Final_LASSO<-m[order(-m$n),]
  Final_LASSO1<-filter(Final_LASSO,n>filter)
  Freqs<-Final_LASSO1
  Freqs$All <- factor(Freqs$All, levels = Freqs$All[order(-Freqs$n)]) #plot in a bar graph the frequencies of ocurrance of the betas and order from highest to smalles
  
  return(Freqs)
  }


 