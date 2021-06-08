library("mlbench")
library(neuralnet)
library(gmodels)
library(caret) 

data(BreastCancer)
summary(BreastCancer)
View(BreastCancer)

Navalues = unique (unlist (lapply (BreastCancer, function (x) which (is.na (x)))))
Navalues
cleanedData <- na.omit(BreastCancer) 

summary(cleanedData)
View(cleanedData)

par(mfrow=c(4, 5))
hist(as.numeric(cleanedData$Cl.thickness))
hist(as.numeric(cleanedData$Cell.size))
hist(as.numeric(cleanedData$Cell.shape))
hist(as.numeric(cleanedData$Marg.adhesion))
hist(as.numeric(cleanedData$Epith.c.size))
hist(as.numeric(cleanedData$Bare.nuclei))
hist(as.numeric(cleanedData$Bl.cromatin))
hist(as.numeric(cleanedData$Normal.nucleoli))
hist(as.numeric(cleanedData$Mitoses))

str(cleanedData)
input<-cleanedData[,2:10]
sapply(input, is.factor)
input <- as.data.frame(lapply(input, function(x) as.numeric(as.character(x))))
str(input)

max_data <- apply(input, 2, max)
min_data <- apply(input, 2, min)
input_scaled <- as.data.frame(scale(input,center = min_data, scale = max_data - min_data))
View(input_scaled)



Cancer<-cleanedData$Class
Cancer<-as.data.frame(Cancer)
Cancer<-with(Cancer, data.frame(model.matrix(~Cancer+0)))
final_data<-as.data.frame(cbind(input_scaled,Cancer))



index = sample(1:nrow(final_data),round(0.70*nrow(final_data)))
train_data <- as.data.frame(final_data[index,])
test_data <- as.data.frame(final_data[-index,])

name = names(final_data[1:9])
formul = as.formula(paste("Cancerbenign + Cancermalignant ~", paste(name, collapse = " + ")))



net = neuralnet(formul,data=train_data,hidden=5,linear.output=FALSE)
plot(net)

predict_net_test <- compute(net,test_data[,1:9])
predict_result<-round(predict_net_test$net.result, digits = 0)
net.prediction = c("benign", "malignant")[apply(predict_result, 1, which.max)]


predict.table = table(cleanedData$Class[-index], net.prediction)
predict.table
CrossTable(x = cleanedData$Class[-index], y = net.prediction,
           prop.chisq=FALSE)




confusionMatrix(predict.table,positive="malignant")
confusionMatrix(predict.table,positive="malignant",mode = "prec_recall")



