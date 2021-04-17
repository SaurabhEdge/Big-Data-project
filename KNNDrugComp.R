#We are going to explore the KNN classification using Caret package
drug <- read.csv("drug_consumption.data")

names(drug)[1]<-"participantID"
names(drug)[2]<-"Age"
names(drug)[3]<-"Gender"
names(drug)[4]<-"Education"
names(drug)[5]<-"Country"
names(drug)[6]<-"Ethnicity"
names(drug)[7]<-"Neuroticism"
names(drug)[8]<-"Escore_ExtraVersion"
names(drug)[9]<-"Oscore_OpennessToExperience"
names(drug)[10]<-"Agreeableness"
names(drug)[11]<-"Conscientiousness"
names(drug)[12]<-"Impulsiveness"
names(drug)[13]<-"SeeingSensation"
names(drug)[14]<-"Alcohol"
names(drug)[15]<-"Amphetamine"
names(drug)[16]<-"Amyl"
names(drug)[17]<-"Bezos"
names(drug)[18]<-"Caff"
names(drug)[19]<-"Cannable"
names(drug)[20]<-"Chocolate"
names(drug)[21]<-"Coke"
names(drug)[22]<-"Crack"
names(drug)[23]<-"Ecstasy"
names(drug)[24]<-"Heroin"
names(drug)[25]<-"Ketamine"
names(drug)[26]<-"Ecstasy"
names(drug)[27]<-"LSD"
names(drug)[28]<-"Meth"
names(drug)[29]<-"Mushrooms"
names(drug)[30]<-"Nicotine"
names(drug)[31]<-"Semer"
names(drug)[32]<-"VSA"

drug$Alcohol <- as.numeric(factor(drug$Alcohol))-1
cols <- c(2:13)
X<-drug[,cols] 
Y<-drug$Alcohol
y=0
#creation of new dataSet considering only the alcohol values
newData <- data.frame(X,Y)
newData <- drug[2:14]
View(newData)
View(drug)

#splitting the data for training and testing
set.seed(400)
intrain<-createDataPartition(y=newData$Alcohol, p=0.7, list = FALSE)
View(intrain)
View(training)
training<-newData[intrain,]
test<-newData[-intrain,]

#checking the dimensions:
dim(training); dim(test)

#checking for any null values :
anyNA(newData)

training[["Alcohol"]]=factor(training[["Alcohol"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(500)

knn_fit<-train(Alcohol ~., data = training, method ="knn",
               trControl=trctrl,
               preProcess =c("center", "scale"),
               tuneLength=10)                             
install.packages("e1071")


#knnfit results
knn_fit

test_pred = predict(knn_fit, newdata = test)
test_pred
