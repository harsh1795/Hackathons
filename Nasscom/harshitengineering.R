# Author: Lokrator
#############################
#initial.dir=getwd()
#setwd(dir = "C:/Users/kiran/Desktop/Analytics/Nasscom Health_care_case_study/")
#library(rpart)
#library(randomForest)
## loading datasets
TD <- read.csv("C:/Users/kiran/Desktop/Analytics/Nasscom Health_care_case_study/Training Data.csv")
ED <- read.csv("C:/Users/kiran/Desktop/Analytics/Nasscom Health_care_case_study/Evaluation Data.csv")

## feature engineering
data_copy = TD
test_copy = ED
#data= data_copy
#test=test_copy
# Factor2 is related with Factor4
#data$Patient_ID = NULL
#test$Patient_ID = NULL
#print(unique(TD$Smoke4))
vector = c("DiseaseHis1Times","DiseaseHis2Times","DiseaseHis3Times","DiseaseHis6","DiseaseStage1","DiseaseStage2","LungFunct20","Disease1","Disease2","Disease3","Disease3Times","Disease4","Disease5","Disease6","Ques5")
for (s in vector){
    for(level in unique(TD[,s])){
        TD[paste(s, level, sep = "_")] <- ifelse(TD[,s] == level, 1, 0)
        ED[paste(s, level, sep = "_")] <- ifelse(ED[,s] == level, 1, 0)}
    TD[,s]=NULL
    ED[,s]=NULL
}
null_vector = c("DiseaseHis1","DiseaseHis2","DiseaseHis3")
for(s in null_vector){
    TD[,s]=NULL
    ED[,s]=NULL
}
y=as.factor(TD$Lung_Cancer)
TD$Lung_Cancer=NULL
ED$Lung_Cancer=NULL
n= c(44:147)
nn=c(2,6,8,9,10,29,30,32,33,34,35,n)
TD[,nn] = lapply(TD[,nn],function(x){as.factor(x)})
ED[,nn] = lapply(ED[,nn],function(x){as.factor(x)})

full_data <- rbind(TD,ED)
TD = full_data[1:1389,]
ED = full_data[1390:1985,]
TD$Lung_Cancer = y

#TD = as.matrix(TD)
#ED = as.matrix(ED)

indx <- sapply(TD, is.factor)
TD[indx] <- lapply(TD[indx], function(x) as.numeric(as.character(x)))
indx1 <- sapply(ED, is.factor)
ED[indx1] <- lapply(ED[indx1], function(x) as.numeric(as.character(x)))

#TD <- transform(TD, class=as.numeric(as.character(TD)))
#ED <- transform(ED, class=as.numeric(as.character(ED)))

n <- names(TD)
n=n[1:147]
f <- as.formula(paste("Lung_Cancer ~", paste(n, collapse = " + ")))

nn <- neuralnet(f, TD, hidden = c(5,3), threshold = 0.001, rep = 2,
                algorithm = "rprop+", act.fct = "logistic", linear.output = FALSE)
#ED$Patient_ID <- NULL
#ED$Lung_Cancer <- NULL
#ED$DiseaseHis1 <- NULL
#ED$DiseaseHis2 <- NULL
#ED$DiseaseHis3 <- NULL
nn.results <- compute(nn, ED)
#nn.results$net.result <- round(nn.results$net.result)
ED$Lung_Cancer <- nn.results$net.result
write.csv(ED, "final.csv")

