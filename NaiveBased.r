drug <- read.csv("drug_cluster.data")

# This classfies data as per gender whether person have taken Amphet or not.( C0= Drug used, C1=Drug not used)

library("caret")
head(drug_cluster)
names(drug_cluster)
Attribute = drug_cluster[,-6]
class = drug_cluster$Amphet
model = train(Attribute,class,'nb',trControl=trainControl(method='cv',number=10))
model
predict(model$finalModel,Attribute)
table(predict(model$finalModel,Attribute)$class,class)
plot (drug_cluster$Gender, class)


# This classfies data as per Age whether person have taken Amphet or not.( C0= Drug used, C1=Drug not used)

library("caret")
head(drug_cluster)
names(drug_cluster)
Attribute = drug_cluster[,-6]
class = drug_cluster$Amphet
model = train(Attribute,class,'nb',trControl=trainControl(method='cv',number=10))
model
predict(model$finalModel,Attribute)
table(predict(model$finalModel,Attribute)$class,class)
plot (drug_cluster$Age, class)


# This classfies data as per Education whether person have taken Amphet or not.( C0= Drug used, C1=Drug not used)

library("caret")
head(drug_cluster)
names(drug_cluster)
Attribute = drug_cluster[,-6]
class = drug_cluster$Amphet
model = train(Attribute,class,'nb',trControl=trainControl(method='cv',number=10))
model
predict(model$finalModel,Attribute)
table(predict(model$finalModel,Attribute)$class,class)
plot (drug_cluster$Education, class)



# This classfies data as per Country whether person have taken Amphet or not.( C0= Drug used, C1=Drug not used)

library("caret")
head(drug_cluster)
names(drug_cluster)
Attribute = drug_cluster[,-6]
class = drug_cluster$Amphet
model = train(Attribute,class,'nb',trControl=trainControl(method='cv',number=10))
model
predict(model$finalModel,Attribute)
table(predict(model$finalModel,Attribute)$class,class)
plot (drug_cluster$Country, class)



# This classfies data as per Ethnicity whether person have taken Amphet or not.( C0= Drug used, C1=Drug not used)

library("caret")
head(drug_cluster)
names(drug_cluster)
Attribute = drug_cluster[,-6]
class = drug_cluster$Amphet
model = train(Attribute,class,'nb',trControl=trainControl(method='cv',number=10))
model
predict(model$finalModel,Attribute)
table(predict(model$finalModel,Attribute)$class,class)
plot (drug_cluster$Ethnicity, class)





