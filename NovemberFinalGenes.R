WD <- paste(path.expand("~"), "/Documents/PhD/R/PipelineAll/", sep="")
setwd(WD)


Clinical <- read.csv("Data/Clinical.csv")
Clinical <- Clinical[-50, ]
NormData <- read.csv("Data/NormData.csv")
NormData2 <- as.data.frame(NormData)

row.names(NormData2) <- NormData2$X
NormData2 <- NormData2[,-1]
NormMat <- t(NormData2)
NormDataFinal <- data.frame(NormMat)

# 
 NormDataFinal["Array"] <- rownames(NormDataFinal)


ClinicalFin <- select(Clinical,Array,Time, MODS)
ClinicalFin <- filter(ClinicalFin, MODS != "")
ClinicalFin$Array<- as.character(ClinicalFin$Array)
NormDataFinal$Array <- as.factor(NormDataFinal$Array)
NormDataFinal$Array<-gsub("X","",NormDataFinal$Array)
Final <- inner_join(ClinicalFin,NormDataFinal)

#filter by times

Final$Time <- as.factor(Final$Time)
# Final1 <- filter(Final, Time == "0h" )
# Final2 <- filter(Final, Time == "24h" )
# Final3 <- filter(Final, Time == "72h" )


s <- Final
s <- s[,-c(1,2)]
names(s)[1]<- c("Label")
s$Label <- factor(s$Label,exclude = "")
levels(s$Label)<- c("N","Y")# has to be N and Y 


#TRAIN
All3 <- s
n<-100 #Number of LASSO and EN loops. Per loop a model with features selected. The 80% most popular features (those that appear in 40/50 or more) will be selected

#N and nn control the number of models created for combinatorial analysis to help in further feature selection - Highest AUC values chosen.
N<-10  
nn<-25

ErrorsFinEN<-vector(mode="double", length=n)
BetasFinEN<-vector(mode="character", length=n)
LambdaFinEN<-vector(mode="double", length=n)
BNumFinEN<-vector(mode="double", length=n)
see2EN<-data.frame(All="All")
LauCoef1<-data.frame(Coeff="See",stringsAsFactors=FALSE)
BetasTodo<-data.frame(Features="Name",Coefficients=1)

ListError<-vector(mode="double", length=n)
BetasFin<-vector(mode="character", length=n)
LambdaFin<-vector(mode="double", length=n)
BNumFin<-vector(mode="double", length=n)
see2<-data.frame(All="All")
LauCoef1L<-data.frame(Coeff="See",stringsAsFactors=FALSE)
BetasTodoL<-data.frame(Features="Name",Coefficients=1)

for (i in 1:n){
  
  
  train_ind2 <- caret::createDataPartition(All3$Label, p = 0.75, list = FALSE, times = 1)
  
  train = All3[train_ind2, ]
  
  #Test set
  test = All3[-train_ind2, ]

  
  xtrain <- train[ , !(names(train) %in% "Label")] %>% data.matrix()
  ytrain = train$Label
  xtest <- test[ , !(names(test) %in% "Label")] %>% data.matrix()
  ytest = test$Label
  
  
  #Choose lambda value that minimize missclassification error. 
  #0.5 as elastic nets, all variables with EN are based on ElasticNets analysis. 100 lambdas sampled with 10 cross validation for each, already internalized in method
  CVEN=cv.glmnet(xtrain,ytrain,family="binomial",type.measure="class",alpha=0.5,nlambda=100)
  
  
 
  
  attach(CVEN)
  Lambda.BestEN<-CVEN$lambda.min #can be either minimum or 1 standard deviation
  print(Lambda.BestEN)
  
  CVFinEN=glmnet(xtrain,ytrain,family="binomial",alpha=0.5,lambda=Lambda.BestEN) 
  CoefEN<-coef(CVFinEN) #Beta coefficients obtained from here
  InterceptEN<-CoefEN@x[1]
  BetasEN<-CVFinEN$beta 
  Betas2EN<-data.frame(Features=BetasEN@Dimnames[[1]][BetasEN@i+1], Coefficients=BetasEN@x) #Beta coefficients names stored here 
  CVPred1EN = predict(CVFinEN, family="binomial", s=Lambda.BestEN, newx = xtest,type="class") #predict in test set to obtain confusion matrix
  
  #Calculate error for categorical values
  ytest2<-as.factor(ytest)
  ResultsEN<-table(CVPred1EN,ytest)
  
  AccuracyEN<-(ResultsEN[1]+ResultsEN[4])/sum(ResultsEN[1:4])
  ErrorEN<-1-AccuracyEN
  
  LauCoef<-Betas2EN$Coefficients
  LauCoefEN<-data.frame(Coeff=LauCoef,stringsAsFactors=FALSE)
  LauCoef1<-rbind(LauCoef1,LauCoefEN)
  BetasTodo<-rbind(BetasTodo,Betas2EN) #store coefficients and store betas
  
  seeEN<-Betas2EN$Features 
  seeEN1<-data.frame(All=seeEN)
  see2EN<-rbind(see2EN,seeEN1)   #all beta names stored
  
  
  mEN<-count(see2EN) #frequency of the betas stored counted
  see3EN<-toString(seeEN) 
  ErrorsFinEN[i]<-ErrorEN #error of the model stored
  BetasFinEN[i]<-see3EN #name of features the model used 
  BNumFinEN[i]<-length(seeEN) #number of features the model used 
  LambdaFinEN[i]<-Lambda.BestEN #lambda chosen for model
  detach(CVEN)
  
  #Change between Lasso and EN, alpha=1 (*)
  CV=cv.glmnet(xtrain,ytrain,family="binomial",type.measure="class",alpha=1,nlambda=100) 
  
  attach(CV)
  
  Lambda.Best<-CV$lambda.min
  CVFin=glmnet(xtrain,ytrain,family="binomial",alpha=1,lambda=Lambda.Best)
  Coef<-coef(CVFin)
  Intercept<-Coef@x[1]
  Betas<-CVFin$beta
  Betas2<-data.frame(Features=Betas@Dimnames[[1]][Betas@i+1], Coefficients=Betas@x)
  CVPred1 = predict(CVFin, family="binomial", s=Lambda.Best, newx = xtest,type="class")
 
  
  
  #Calculate error for categorical values
  ytest2<-as.factor(ytest)
  
  Results<-table(CVPred1,ytest)
  Accuracy<-(Results[1]+Results[4])/sum(Results[1:4])
  Error<-1-Accuracy
  
  #visual display of for
  
  BetasTodoL<-rbind(BetasTodoL,Betas2)
  see<-Betas2$Features
  see1<-data.frame(All=see)
  see2<-rbind(see2,see1)
  m<-count(see2)
  
  see3<-toString(see)
  ListError[i]<-Error
  BetasFin[i]<-see3
  BNumFin[i]<-length(see)
  LambdaFin[i]<-Lambda.Best
  detach(CV)
  
}

All_info<-data.frame(Error=ListError, BetasNames=BetasFin, BetasNum=BNumFin, Lambda=LambdaFin) 
All_infoEN<-data.frame(Error=ErrorsFinEN, BetasNames=BetasFinEN, BetasNum=BNumFinEN, Lambda=LambdaFinEN)


m<-count(see2, All)
mEN<-count(see2EN, All)

m<-m[-1,]
mEN<-mEN[-1,]

Final_LASSO<-m[order(-m$n),] #order highest frequencies above and filter with those that appear more than 80% of times 
Final_LASSO1<-filter(Final_LASSO,n>20) #threshold selected - 80%


Final_EN<-mEN[order(-mEN$n),]
Final_EN1<-filter(Final_EN,n>5)
Final_Plot_Names<-filter(Final_EN,n>45)




#######

### BOX PLOTS ###

# Load libraries
library(data.table)
library(tidyverse)

# Import data
clin <- fread('Clinical.csv')[Group != 'Moderate']
dat <- read.csv('NormData.csv', row.names = 1, check.names = FALSE) 

### Crit vs. Ctrl ###
genes_1 <- c('CRISP3', 'EPHB4', 'FAM131A', 'GFI1', 'GJB6', 'HLX', 'LOC388514', 
             'LOC441582', 'LOC646575', 'LOC728835', 'OLR1', 'PARVG', 'PDXK', 
             'PTGS2', 'PTX3', 'SP100')

# Tidy
mat <- dat[genes_1, match(clin$Array, colnames(dat))]
df <- gather(mat, Sample, Expression) %>%
  mutate(Group = rep(clin$Group, each = nrow(mat)),
         Time = rep(clin$Time, each = nrow(mat)),
         Gene = rep(genes_1, times = ncol(mat)))


# Plot
ggplot(df, aes(Time, Expression, fill = Group)) + 
  geom_boxplot() +
  scale_fill_manual(values = c('#00BA38', '#F8766D')) + 
  labs(title = 'Gene Expression by Group, Time', 
       x = 'Time', 
       y = 'Normalized Expression') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Gene, nrow = 4, ncol = 4)


### Ctrl, MODS, noMODS ###
#genes_2 <- c('AXUD1', 'FRMD4B', 'GJB6', 'JMJD6', 'JOSD1', 'JUNB', 'LOC100127984', 
#             'LOC728877', 'OLR1', 'OSM', 'PTX3', 'RNASE3', 'RPS6KA2', 'SH2D2A', 
#            'SOCS3', 'TFF3')

genes_2 <- c("GIMAP4","GPI","LOC643373","VAT1","FLI1","LOC643373","AMD1","HLA-A29.1","GPR84","MYLIP","C3AR1","MANBA","SRXN1")


genes_2 <- c("SP100","STOM","ST3GAL1","MVD","SLC16A6")

genes_2 <- c("SRXN1","QSOX1","LOC729954","LOC647805","STX4","SLC25A46")

genes_2 <- c("GPI","HS.542713","SLC7A4","A1BG","SH3RF3","KIAA1274")




#genes_2 <- c('AXUD1', 'FRMD4B', 'GJB6', 'JMJD6', 'JOSD1', 'JUNB', 'LOC100127984', 
#             'LOC728877', 'OLR1', 'OSM', 'PTX3', 'RNASE3', 'RPS6KA2', 'SH2D2A', 
#             'SOCS3', 'TFF3')
#genes_2<-as.character(Final_Plot_Names$All)
#genes_2<-as.character(names(Betas_select_thresh_75)[1:5])
#genes_2<-c("VAT1","HLA-A29.1","GPR84","MYLIP","C3AR1","AMD1","FLI1","GIMAP4","GPI","LOC643373")



# Tidy
clin_ctrl <- clin[Group == 'Control']
clin_mods <- clin[Group == 'Critical'
                  ][, MODS := ifelse(MODS == 'Yes', 'MODS', 'NO_MODS')
                    ][, MODS := relevel(as.factor(MODS), ref = 'NO_MODS')]



##really cool code!!!! - relevel as we want. As you have an ifelse, what you are doing
#is saying sunstutute Yes with MODS, the other, with NO_MODS. relevel NoMODS first one. 
#Colon and comma comes beacuse MODS is not an object but a column so asign it in this way?
#  Not changing value but working within??

factor(clin$MODS)
mat_ctrl <- dat[genes_2, match(clin_ctrl$Array, colnames(dat))]
df_ctrl <- gather(mat_ctrl, Sample, Expression) %>%
  mutate(Group = 'Control',
         Time = rep(clin_ctrl$Time, each = nrow(mat_ctrl)),
         Gene = rep(genes_2, times = ncol(mat_ctrl)))
mat_mods <- dat[genes_2, match(clin_mods$Array, colnames(dat))]
df_mods <- gather(mat_mods, Sample, Expression) %>%
  mutate(Group = rep(clin_mods$MODS, each = nrow(mat_mods)),
         Time = rep(clin_mods$Time, each = nrow(mat_mods)),
         Gene = rep(genes_2, times = ncol(mat_mods)))
df <- rbind(df_ctrl, df_mods)

# Plot
ggplot(df, aes(Time, Expression, fill = Group)) + 
  geom_boxplot() +
  scale_fill_manual(values = c('#00BA38', '#619CFF', '#F8766D')) + 
  labs(title = 'Gene Expression by Group, Time', 
       x = 'Time', 
       y = 'Normalized Expression') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Gene, nrow = 3, ncol = 3)


