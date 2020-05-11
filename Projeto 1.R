library(gputools)
n0 <- read.csv(file="DigitosCompleto/9_001.BMP.inv.pgm", header=FALSE, sep=" ")

n0 <- n0[-1,]
n0 <- n0[-1,]
n0 <- n0[-1,]

n0 <- n0[,-18]

n0 <- as.vector(t(n0))
n0 <- as.numeric(n0)
n0 <- n0[-4097]
n0 <- matrix(n0, byrow=TRUE, 64, 64)

file <- dir(path = "DigitosCompleto/", pattern = NULL, all.files = FALSE,
            full.names = FALSE, recursive = FALSE,
            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

number <- vector(,2000)
pixels <- matrix(nrow= 0, ncol=4096)

for (i in 1:2000) {
  number[i] <- substring(file[i], 1 , 1)

  n0 <- read.csv(file = paste("DigitosCompleto/",file[i], sep="") ,header=FALSE, sep=" ")
  n0 <- n0[-1,]
  n0 <- n0[-1,]
  n0 <- n0[-1,]
  n0 <- n0[,-18]
  
  n0 <- as.vector(t(n0))
  n0 <- n0[-4097]
  
  pixels <- rbind(pixels,n0)
}

df <- data.frame(number, pixels)

#KNN

set.seed(777)
indiceTreino<-sample(1:nrow(df), 0.8*nrow(df))

dadosTreino<-df[indiceTreino,]
dadosTeste<-df[-indiceTreino,]

tiposTreino<-dadosTreino[,1]
dadosTreino<-dadosTreino[,-1]
tiposTeste<-dadosTeste[,1]
dadosTeste<-dadosTeste[,-1]

library(class)
modelo<-knn(train = dadosTreino, test = dadosTeste, cl = tiposTreino, k = 7)

matrizConf<-table(tiposTeste,modelo)
sum(diag(matrizConf))/sum(matrizConf)

table(tiposTreino)

warnings()

