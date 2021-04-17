
drug <- read.csv("C:/Users/gopsw/OneDrive/Desktop/CSCI620/Mining/Mining/drug_consumption.data",header = TRUE)
colnames(drug) <- c("ID","Age","Gender","Education","Country","Ethnicity","Nscore","Escore","Oscore","Ascore","Cscore","Impulsive","SS","Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Coke","Crack","Ecstacy","Heroin","Ketamine","Legalh","LSD","Meth","Mushrooms","Nicotine","Semer","VSA")

drug.clean <- drug %>%
  as_tibble %>%
  mutate_at(vars (Age:Ethnicity), list(as.factor)) %>%
  mutate (Age = factor (Age, labels = c ("18_24", "25_34", "35_44", "45_54","55_64", "65_"))) %>%
  mutate (Gender = factor (Gender, labels = c ("Male", "Female"))) %>%
  mutate (Education = factor (Education, labels = c ("Under16", "At16", "At17","At18", "SomeCollege","ProfessionalCert", "Bachelors", "Masters","Doctorate"))) %>%
  mutate (Country = factor (Country, labels = c ("USA", "NewZealand", "Other","Australia", "Ireland","Canada","UK"))) %>%
  mutate (Ethnicity = factor (Ethnicity, labels = c ("Black", "Asian", "White","WhiteBlack", "Other", "WhiteAsian", "BlackAsian"))%>%
  select(-ID)

  
  
  

drug.cluster <- drug.clean %>%
  as_tibble %>%
  mutate(Alcohol = factor(Alcohol,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Amphet = factor(Amphet,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Amyl = factor(Amyl,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Benzos = factor(Benzos,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Caff = factor(Caff,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Cannabis = factor(Cannabis,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Choc = factor(Choc,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Coke = factor(Coke,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Crack = factor(Crack,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Ecstacy = factor(Ecstacy,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Heroin = factor(Heroin,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Ketamine = factor(Ketamine,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Legalh = factor(Legalh,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(LSD = factor(LSD,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Meth = factor(Meth,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Mushrooms = factor(Mushrooms,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Nicotine = factor(Nicotine,labels = c("C0","C1","C1","C1","C1","C1","C1")))%>%
  mutate(Semer = factor(Semer,labels = c("C0","C1","C1","C1","C1")))%>%
  mutate(VSA = factor(VSA,labels = c("C0","C1","C1","C1","C1","C1","C1")))






hist(drug.clean$Gender,
     main = "Histogram for the Gender",
     xlab="Gender",
     border="white",
     col=rgb(0.9, 0.8, 0.9, 0.8),
     xlim=c(-0.6, 0.6),
     ylim=c(0, 1000))


gender = c(drug.clean$Gender)

age = c(drug.clean$Age)

education = c(drug.clean$Education)




plot <- cbind(Age = drug.cluster$Age,Education = drug.cluster$Education)
boxplot(plot)

Cafftable = table(drug.cluster$Caff)
pie(Cafftable,main = "Caff")

Cannabis = table(drug.cluster$Cannabis)
pie(Cannabis,main = "Cannabis")

Coke = table(drug.cluster$Coke)
pie(Coke,main = "Coke")

Cafftable = table(drug.cluster$Caff)
pie(Cafftable,main = "Caff")

Cannabis = table(drug.cluster$Cannabis)
pie(Cannabis,main = "Cannabis")

Amyl = table(drug.cluster$Amyl)
pie(Amyl,main = "Amyl")





pairs(drug.cluster[1:5])








