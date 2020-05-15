install.packages("e1071");
install.packages("rpart");
install.packages("class");

dfFunc <- function(){
  
  file <- dir(path = "DigitosCompleto/", pattern = NULL, all.files = FALSE,
              full.names = FALSE, recursive = FALSE,
              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  nfiles <- length(file)
  number <- vector(,nfiles)
  pixels <- matrix(nrow= 0, ncol=4096)
  
  for (i in 1:nfiles) {
    number[i] <- substring(file[i], 1 , 1)
    
    n0 <- read.csv(file = paste("DigitosCompleto/",file[i], sep="") ,header=FALSE, sep=" ")
    n0 <- n0[-1,]
    n0 <- n0[-1,]
    n0 <- n0[-1,]
    n0 <- n0[,-18]
    
    n0 <- as.vector(t(n0))
    n0 <- n0[-4097]
    n0 <- as.numeric(n0)
    
    pixels <- rbind(pixels,n0)
  }
  
  number <- as.numeric(number)
  
  df <- data.frame(number, pixels)
  
  return(df)
}

#KNN
knnFunc <- function(dadosTreino, dadosTeste, k) {
  library(class)
  
  tiposTreino<-dadosTreino[,1]
  dadosTreino<-dadosTreino[,-1]
  tiposTeste<-dadosTeste[,1]
  dadosTeste<-dadosTeste[,-1]
  
  modelo<-knn(train = dadosTreino, test = dadosTeste, cl = tiposTreino, k = k)
  
  matrizConf<-table(tiposTeste,modelo)
  print(k)
  matrizConf
  sum(diag(matrizConf))/sum(matrizConf)
}

#Arvore de decisão
rpartFunc <- function(dadosTreino,dadosTeste){
  library(rpart)
  
  modelo<-rpart(number~., dadosTreino, method="class")
  
  tiposTeste<-dadosTeste$number
  dadosTeste<-dadosTeste[,-1]
  pred<-predict(modelo, dadosTeste, type="class")
  
  matrizConf<-table(tiposTeste,pred)
  matrizConf
  sum(diag(matrizConf))/sum(matrizConf)
}

#SVM
svmFunc <- function(dadosTreino, dadosTeste){
  library(e1071)
  
  tiposTeste <- dadosTeste[,1]
  dadosTeste <- dadosTeste[, -1]
  
  classifier <- svm( formula = number ~ .,
                     data= dadosTreino,
                     type='C-classification',
                     kernel='linear')
  
  pred = predict(classifier, newdata = dadosTeste)
  
  matrizConf <- table(tiposTeste, pred)
  matrizConf 
  sum(diag(matrizConf))/sum(matrizConf)
}

##Main##
df <- dfFunc()

set.seed(777)
indiceTreino<-sample(1:nrow(df), 0.8*nrow(df))

dadosTreino<-df[indiceTreino,]
dadosTeste<-df[-indiceTreino,]

knnFunc(dadosTreino, dadosTeste, 1)
knnFunc(dadosTreino, dadosTeste, 3)
knnFunc(dadosTreino, dadosTeste, 5)
knnFunc(dadosTreino, dadosTeste, 7)
knnFunc(dadosTreino, dadosTeste, 9)

rpartFunc(dadosTreino, dadosTeste)
svmFunc(dadosTreino, dadosTeste)
