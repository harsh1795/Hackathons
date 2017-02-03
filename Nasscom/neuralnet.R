data <- read.xlsx("/home/harshit/Desktop/Nasscom/Health_care_Dataset_for_probelm.xlsx.xlsx", sheetIndex = 2, rowIndex = 6:1395, colIndex = 1:62)
View(data)
attach(data)
test <- read.csv("/home/harshit/Desktop/Nasscom/g.xlsx")
View(test)
set.seed(1)
#n<-nrow(TD)
#n1<-as.integer(0.8*n)
#t<-sample(n,n1)
#train_valid<-TD[t,]
#test_data<-TD[-t,]
#n2<-nrow(train_valid)
#n3<-as.integer(0.8*n2)
#p<-sample(n2,n3)
#train_data<-train_valid[p,]
#validation_data<-train_valid[-p,]

library(neuralnet)

#n <- names(train_data)
#n=c(n[1],n[3:62])
#f <- as.formula(paste("Lung_Cancer ~", paste(n, collapse = " + ")))

#nn <- neuralnet(f, train_data, hidden = 4, threshold = 0.01, linear.output = FALSE)
#plot(nn)
#validation_lc <- validation_data$Lung_Cancer
#validation_data$Lung_Cancer <- NULL
#nn.results <- compute(nn, validation_data)
#nn.results$net.result <- round(nn.results$net.result)
#table(validation_lc,nn.results$net.result)
ED = test
TD = data
View(TD)
n <- names(TD)
n=c(n[1],n[3:62])
f <- as.formula(paste("Lung_Cancer ~", paste(n, collapse = " + ")))
nn <- neuralnet(f, TD, hidden = c(6,4), threshold = 0.01, rep = 2,
                algorithm = "rprop+", act.fct = "logistic", linear.output = FALSE)
ED$Lung_Cancer <- NULL
nn.results <- compute(nn, ED)
#nn.results$net.result <- round(nn.results$net.result)
ED$Lung_Cancer <- nn.results$net.result
write.csv(ED, "/home/harshit/Desktop/Nasscom/h.xlsx")

library(verification)
abc <-as.integer(TD$Lung_Cancer)
TD$Lung_Cancer <- NULL
nn.resultstrain <- compute(nn, TD)
veri = verify(abc,
              nn.resultstrain$net.result,
              frcst.type = "prob",
              obs.type = "binary")
reliability.plot(veri)
try changing hidden = c(5,3,2) and so on