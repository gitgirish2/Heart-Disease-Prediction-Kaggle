library(tidyverse)
library(corrplot)
library(ggplot2)
library(plyr)
library(caTools)
library(caret)

heartdiseases<-read.csv('Dokumen/MachineLearning/heart.csv')
heartdiseases

heartdiseases<-rename(heartdiseases,c("i..age"="age"))
head(heartdiseases)
nrow(heartdiseases)
ncol(heartdiseases)
str(heartdiseases)
summary(heartdiseases)

colSums(is.na(heartdiseases))
Correlation<-cor(heartdiseases)
corrplot(correlation,method = "circle")
heartdiseases<-subset(heartdiseases,select = c(-chol,-fbs,-restecg))
heartdiseases$sex<-as.factor(heartdiseases$sex)
heartdiseases$target<-as.factor(heartdiseases$target)
heartdiseases$cp<-as.factor(heartdiseases$cp)
heartdiseases$ca<-as.factor(heartdiseases$ca)
heartdiseases$exang<-as.factor(heartdiseases$exang)
heartdiseases$slope<-as.factor(heartdiseases$slope)
heartdiseases$thal<-as.factor(heartdiseases$thal)
summary(heartdiseases)
colSums(is.na(heartdiseases))

heartdiseases$target<-as.factor(heartdiseases$target)
ggplot(heartdiseases, aes(x=heartdiseases$target, fill=heartdiseases$target)) + 
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Analysis of Presence and Absence of Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absence", "Presence"))

#menghitung frekuensi dan nilai usia
ageCount<-count(heartdiseases,'age')
ageCount<-subset(ageCount[which(ageCount$freq>10), ])
#merencanakan usia dengan frquency lebih besar dari 10
ggplot(ageCount, aes(x=ageCount$age, y=ageCount$freq))+
  ggtitle("Age Analysis")+
  xlab("Age")+
  ylab("Age Count")+
  geom_bar(stat = "identity")
#Kelompokkan berbagai usia dalam tiga kelompok (muda, menengah, tua)
young<-heartdiseases[which((heartdiseases$age<45)), ]
middle<-heartdiseases[which((heartdiseases$age>=45)&(heartdiseases<55)), ]
elderly<-heartdiseases[which(heartdiseases$age>55),]

groups <- data.frame(age_group = c("young","middle","elderly"), group_count = c(NROW(young$age), NROW(middle$age), NROW(elderly$age)))

#merencanakan berbagai kelompok umur
ggplot(groups, aes(x=groups$age_group, y=groups$group_count, fill=groups$age_group)) + 
  ggtitle("Age Analysis") +
  xlab("Age Group")  +
  ylab("group Count") +
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Age Group", labels = c("Elderly", "Middle", "Young"))

#Menambahkan grup umur ke dataset
heartdiseases <- cbind(heartdiseases, groups = ifelse((heartdiseases$age<45), 0, ifelse((heartdiseases$age>=45)&(heartdiseases$age<55), 1, 2)))
heartdiseases$groups <- as.factor(heartdiseases$groups)
#menghapus umur
heartdiseases=subset(heartdiseases,select = c(-age))

## Discrete vs Discrete vs Variabel diskrit: age_group ~ target ~ sex
ggplot(heartdiseases,aes(x=factor(heartdiseases$groups),y=heartdiseases$sex,colour=target))+
  geom_boxplot(stat="boxplot",
               position="dodge2")+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width =0.2)+
  xlab("Age Groups")+
  ylab("Gender")+
  ggtitle("Analysis of gender with different age group with presence or absense of heart disease")

#sex variable analysis
#plot untuk sex
ggplot(heartdiseases,aes(x=heartdiseases$sex,
                         fill=heartdiseases$target))+
  geom_bar()+
  xlab("Gender")+
  ylab("Gender Count")+
  ggtitle("Analysis of Gender")+
  scale_fill_discrete(name="Heart disease",
                      labels=c("No","Yes"))

heartdiseases=subset(heartdiseases,select=c(-sex))

#Bar plot untuk Nyeri dada yang dialami
ggplot(heartdiseases, aes(x= cp, fill=cp)) + 
  geom_bar() +
  xlab("Chest Pain Type") +
  ylab("Count") +
  ggtitle("Analysis of Chest Pain Experienced") +
  scale_fill_discrete(name = "Chest Pain Type", labels = c("Typical angina pain", "Atypical angina pain", "Non-Anginal pain", "Asymptomatic pain"))

#Bar plot untuk Nyeri dada ~ target
ggplot(heartdiseases, aes(x=cp,fill=target))+
  geom_bar()+
  xlab("Chest Pain Type")+
  ylab("Count")+
  ggtitle("Analysis of Chest Pain Experienced")+
  scale_fill_discrete(name="Heart disease",labels=c("No","Yes"))

#bar for ca
ggplot(heartdiseases, aes(x= ca, fill=ca)) + 
  geom_bar() +
  xlab("number of major vessels") +
  ylab("Count") +
  ggtitle("Analysis of number of major vessels") +
  theme(legend.position="none")

# Bar for ca (number of major vessels (0-3))
ggplot(heartdiseases, aes(x= ca, fill=target)) + 
  geom_bar(position = 'dodge') +
  xlab("number of major vessels") +
  ylab("Count") +
  ggtitle("Analysis of number of major vessels") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# Histogram for trestbps (resting blood pressure)
ggplot(heartdiseases, aes(x=trestbps)) + 
  geom_histogram() +
  xlab("Resting blood pressure") +
  ylab("Count") +
  ggtitle("Analysis of blood pressure")

#menghapus outliers
heartdiseases$trestbps=ifelse(heartdiseases$trestbps>180,NA,heartdiseases$trestbps)
heartdiseases$trestbps=ifelse(is.na(heartdiseases$trestbps),
                              median(heartdiseases$trestbps[which(!is.na(heartdiseases$trestbps))]),heartdiseases$trestbps)

ggplot(heartdiseases, aes(x=trestbps))+
  geom_histogram()+
  xlab("REsting Blood pressure")+
  ylab("Count")+
  ggtitle("Analysis of blood pressure")

#Grafik kepadatan untuk trestbps (tekanan darah istirahat)
ggplot(heartdiseases, aes(x=trestbps,fill=target))+
  geom_density(alpha=0.5)+
  scale_fill_discrete(name="Heart disease", labels=c("No","Yes"))

heartdiseases=subset(heartdiseases,select = c(-trestbps))
heartdiseases

#analisis variabel oldpeak
#Histogram untuk oldpeak (depresi ST yang disebabkan oleh olahraga relatif terhadap istirahat)
ggplot(heartdiseases, aes(x=oldpeak)) + 
  geom_histogram() +
  xlab("ST depression induced by exercise relative to rest") +
  ylab("Count") +
  ggtitle("Analysis of ST depression induced by exercise relative to rest")

#From the above histogram
heartdiseases$oldpeak<-log1p(heartdiseases$oldpeak)

ggplot(heartdiseases, aes(x=oldpeak)) + 
  geom_histogram() +
  xlab("ST depression induced by exercise relative to rest") +
  ylab("Count") +
  ggtitle("Analysis of ST depression induced by exercise relative to rest")

#Plot kepadatan untuk oldpeak ~ target
ggplot(heartdiseases, aes(x = oldpeak, fill = target)) +
  geom_density(alpha=0.5) +
  xlab("ST depression induced") +
  ylab("Count") +
  ggtitle("Analysis of ST depression induced and presence of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

#menganalisa variable Slope
# Plot plot untuk kemiringan (kemiringan segmen ST latihan puncak)
heartdiseases$slope<-ifelse(heartdiseases$slope==0,1,print(heartdiseases$slope))

heartdiseases$slope <- as.factor(heartdiseases$slope)
ggplot(heartdiseases, aes(x=heartdiseases$slope, fill=heartdiseases$slope)) + 
  geom_bar() +
  xlab("Slope of ST segment") +
  ylab("Count") +
  ggtitle("Analysis of slope of the peak exercise ST segment") +
  scale_fill_discrete(name = "Slope of ST segment", labels = c("Upsloping", "Flat", "Downsloping"))

#Plot for slope-target
ggplot(heartdiseases, aes(x= slope, fill=target)) + 
  geom_bar(position = 'dodge') +
  xlab("slope of the peak exercise ST segment") +
  ylab("count") +
  ggtitle("Analysis of slope of the peak exercise ST segment with presence or absense of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))
heartdiseases

#analisa fitur thalach
ggplot(heartdiseases, aes(x=thalach))+
  geom_histogram()+
  xlab("Maximum heart rate achieved")+
  ylab("Count")+
  ggtitle("Analysis of maximum heart rate achieved")

#mengganti outlier
heartdiseases$thalach=ifelse(heartdiseases$thalach<75,NA,heartdiseases$thalach)
heartdiseases$thalach=ifelse(is.na(heartdiseases$thalach),
                             median(heartdiseases$thalach[which(!is.na(heartdiseases$thalach))]),heartdiseases$thalach)

ggplot(heartdiseases, aes(x=thalach))+
  geom_histogram()+
  xlab("Maximum heart rate achieved")+
  ylab("Count")+
  ggtitle("Analysis of maximum heart rate achieved")

#Plot kepadatan untuk thalach ~ target
ggplot(heartdiseases, aes(x=thalach,fill=target))+
  geom_density(alpha=0.5)+
  xlab("Maximum heart rate achieved")+
  ylab("Count")+
  ggtitle("Analysis of relation of heart rate with presence of heart disease")+
  scale_fill_discrete(name="Heart disease",labels=c("No","Yes"))

#Menganalisa fitur kekurangan darah atau thal
ggplot(heartdiseases, aes(x=thal)) + 
  geom_bar() +
  xlab("Blood disorder type") +
  ylab("Count") +
  ggtitle("Analysis of blood disorder (thalassemia)")

#Mengganti nilai yang tidak valid dengan nilai mode thal
heartdiseases$thal=ifelse(heartdiseases$thal==0,2,heartdiseases$thal)
heartdiseases$thal<-as.factor(heartdiseases$thal)
ggplot(heartdiseases, aes(x=thal,fill=thal))+
  geom_bar()+
  xlab("Blood disorder type")+
  ylab("count")+
  ggtitle("Analysis of blood disorder (thalassemia)")+
  scale_fill_discrete(name="Blood disorder",
  labels=c("Normal","Cacat tetap","Cacat yang dapat dibalik"))

ggplot(heartdiseases, aes(x= thal, fill=target)) + 
  geom_bar(position = 'dodge') +
  xlab("blood disorder") +
  ylab("count") +
  ggtitle("Analysis of blood disorder with presence or absense of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

#implementasi model
# Untuk mendapatkan set yang sama setiap kali kita menjalankan kode
set.seed(123)

# Atur ulang kolom untuk menjadikan target sebagai kolom terakhir
heartdiseases<- heartdiseases[, c(1,2,3,4,5,6,7,9,8)]

#Membagi set data dalam set data train dan uji
dataSample<-sample.split(heartdiseases[, ncol(heartdiseases)-1],SplitRatio = 0.80)
trainSet=subset(heartdiseases, dataSample==TRUE)
testSet=subset(heartdiseases,dataSample==FALSE)

#membuat model logistic
logisticmodel<-glm(target~.,data = trainSet,family = "binomial")

#Ringkasan model yang dibuat
summary(logisticmodel)

# Making prediction with the above model
predictdata<-predict(logisticmodel,newdata = testSet[,-9], type = "response")
pred<-ifelse(predictdata>=0.5,1,0)
pred<-as.factor(pred)
observed<-testSet[,9]

#cek akurasi model
confusionMatrix(pred,observed)
