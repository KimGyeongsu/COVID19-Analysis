######################################################################################################################
#COVID19 DEATH RATE_Full R code
######################################################################################################################
rm(list=ls())
###################################################################################
#import data
###################################################################################
setwd("C:/Users/david/Documents/Rdata")
library("readxl")
library("dplyr")
library("agricolae")
###################################################################################
#pre-processing
###################################################################################
######################################################
#1. Create death rate table
# Write the result on death_rate_2020_05_31


temp<-read_excel("owid-covid-data.xlsx")
# death rate(%) 추가, 확진자 수 오름차순 정렬
temp<-temp %>% filter(date=="2020-05-31") %>% mutate(death_rate=total_deaths_per_million/total_cases_per_million*100)%>% arrange(total_cases_per_million)
# 제외시킬 data 수 (확진자 수 하위 30프로)
cnt<-as.integer(nrow(temp)*30/100)
# alphabetical order(by country name), 사망자 수 0,NA인 나라 제외
temp<- temp[(cnt+1):nrow(temp),] %>% arrange(location) %>% filter(!is.na(total_deaths_per_million))%>% filter(total_deaths_per_million!=0) %>% select(iso_code,continent,location,date,death_rate)

library("writexl")
write_xlsx(temp, "death_rate_2020_05_31_.xlsx")

######################################################
#2. Add death table and age data
# Write the result on age_data_level

a<-read_excel("death_rate_2020_05_31.xlsx") %>% select(location, death_rate)
health_dat<-read.csv("country_health_indicators_v3.csv")
old_data <- read_excel("old_age_distribution.xlsx")

#old data 추가 (65세 이상 인구 비율) 
old_data <- old_data %>% select(`Country Name`, '2018')
names(old_data) <-c("Country_Region", "over65")
old_data<-inner_join(old_data,a,by=c("Country_Region"="location")) %>% select(1,3,2)
old_data<-old_data %>% mutate(level=ifelse(death_rate<4, "level1", ifelse(death_rate<8, "level2", "level3"))) %>% select(4,1:3)
old_data<-old_data %>% filter(!is.na(over65))
View(health_dat)


#add median age factor
b <- health_dat %>% select(Country_Region, median.age)
old_data_fin <- inner_join(old_data,b,by=c("Country_Region")) 
old_data_fin[,3:5] <- lapply(old_data_fin[,3:5],round, 2)
old_data_fin <- data.frame(old_data_fin)
View(old_data_fin)
nrow(old_data_fin[old_data_fin$level=="level1",]) #57
nrow(old_data_fin[old_data_fin$level=="level2",]) #25
nrow(old_data_fin[old_data_fin$level=="level3",]) #10

#write age data on excel file
library("writexl")
write_xlsx(old_data_fin, "age_data_level.xlsx")




######################################################
#3. make a dataset0, including level of death rate

#food data : dataset 1 : 21~42 (aquatic, products other)
#hospital : data 1 : 43,44~48
#disease : data 1 : 9~18
#age : datset 2

a<-read_excel("death_rate_2020_05_31.xlsx")
a<-a %>% filter(!is.na(continent))
a <-a %>% mutate(level=ifelse(death_rate<4,"level1",ifelse(death_rate<8,"level2","level3")))
nrow(a[a$level=="level1",]) #67
nrow(a[a$level=="level2",]) #28
nrow(a[a$level=="level3",]) #10

dataset1<-read.csv("country_health_indicators_v3.csv")
dataset2<-read_excel("age_data_level.xlsx")
dataset1$Country_Region <- as.character(dataset1$Country_Region)
b<-inner_join(a,dataset1,by=c("location"="Country_Region")) %>% select(6, 3,5,14:23,26,27,28,30:53)
b<-inner_join(b,dataset2[,c(2,4,5)],by=c("location"="Country_Region"))
a<-na.omit(b)
nrow(a[a$level=="level1",]) #24
nrow(a[a$level=="level2",]) #12
nrow(a[a$level=="level3",]) #5
library("writexl")
write_xlsx(a, "dataset0.xlsx")




##################################################################################################################################
#Box plotting & ANOVA test & post hoc
##################################################################################################################################

ds0 <- read_excel("dataset0.xlsx")
ds0[,3:13] <- lapply(ds0[,3:13],round, 2)
ds0 <- data.frame(ds0)

#########################################################
#1. Disease
#########################################################

y<-ds0
y <- y%>%
  mutate(num_lv=ifelse(level=="level1",1,(ifelse(level=="level2",2,3))))

##############
#Boxplot
##############

par(mfrow = c(1,2))
View(y)
#par(mfrow=c(1,1))
boxplot(y[,4]~y[,1], main = "Death rate vs Cardio", xlab = "Death_Rate", ylab = "Cardiovascular")
boxplot(y[,5]~y[,1], main = "Death rate vs Cancer", xlab = "Death_Rate", ylab = "Cancer")
boxplot(y[,6]~y[,1], main = "Death rate vs Diabetes...", xlab = "Death_Rate", ylab = "Diabetes...")
boxplot(y[,7]~y[,1], main = "Death rate vs Respiratory.dis", xlab = "Death_Rate", ylab = "Respiratory.dis")
boxplot(y[,8]~y[,1], main = "Death rate vs Liver.dis", xlab = "Death_Rate", ylab = "Liver.dis")
boxplot(y[,9]~y[,1], main = "Death rate vs Diarrhea...", xlab = "Death_Rate", ylab = "Diarrhea...")
boxplot(y[,10]~y[,1], main = "Death rate vs Musculoskeletal.dis", xlab = "Death_Rate", ylab = "musculoskeletal.dis")
boxplot(y[,11]~y[,1], main = "Death rate vs HIV.AIDS", xlab = "Death_Rate", ylab = "V.AIDS")
boxplot(y[,12]~y[,1], main = "Death rate vs Malaria...", xlab = "Death_Rate", ylab = "Malaria...")
boxplot(y[,13]~y[,1], main = "Death rate vs Nutritional.def", xlab = "Death_Rate", ylab = "Nutritional.def")

View(y)


##############
#ANOVA & POST_HOC
##############

for(i in 4:13){
  m <- aov(y[,i] ~ level, data=y)
  assign(paste('col',i,'anv',sep='.'),m)
}

#anova
summary(col.5.anv) # Can change i value to see different result
#par(mfrow=c(2,2))
#plot(col.4.anv)

#posthoc
n <- scheffe.test(col.5.anv, "level",console =TRUE) # Can change i value to see different result



#correlation coefficient
#correlation coefficient
cor(y[,5],y$num_lv, method = 'pearson')
cor(y[,15],y$num_lv, method = 'pearson')
cor(y[,16],y$num_lv, method = 'pearson')
cor(y[,32],y$num_lv, method = 'pearson')

cor(y[,5],y$death_rate, method = 'pearson')
cor(y[,15],y$death_rate, method = 'pearson')
cor(y[,16],y$death_rate, method = 'pearson')
cor(y[,32],y$death_rate, method = 'pearson')


#########################################################
#2. Food source
#########################################################

##############
#Boxplotting
##############
y<-ds0
#View(y)
#y <- y%>%
#  mutate(num_lv=ifelse(level=="level1",1,(ifelse(level=="level2",2,3))))
par(mfrow = c(1,3))
#View(y)
#par(mfrow=c(1,1))

colnames(y)
boxplot(y[,14]~y[,1], main = "alcoholic_beverage", xlab = "Death_Rate", ylab = "DALYs") #over65
boxplot(y[,15]~y[,1], main = "animal_fats", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,16]~y[,1], main = "animal_products", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,17]~y[,1], main = "cereals", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,18]~y[,1], main = "eggs", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,19]~y[,1], main = "seafood", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,20]~y[,1], main = "fruits", xlab = "Death_Rate", ylab = "DALYs") #over65
boxplot(y[,21]~y[,1], main = "meat", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,22]~y[,1], main = "milk", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,23]~y[,1], main = "miscellaneous", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,24]~y[,1], main = "offals", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,25]~y[,1], main = "oilcrops", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,26]~y[,1], main = "pulses", xlab = "Death_Rate", ylab = "DALYs") #over65
boxplot(y[,27]~y[,1], main = "spices", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,28]~y[,1], main = "starchy_roots", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,29]~y[,1], main = "stimulants", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,30]~y[,1], main = "sweeteners", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,31]~y[,1], main = "treenuts", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,32]~y[,1], main = "vegetable_oils", xlab = "Death_Rate", ylab = "DALYs") #over65
boxplot(y[,33]~y[,1], main = "vegetables", xlab = "Death_Rate", ylab = "DALYs")
boxplot(y[,34]~y[,1], main = "vegetal_products", xlab = "Death_Rate", ylab = "DALYs")



##############
#ANOVA & POST_HOC
##############
for(i in 14:34){
  m <- aov(y[,i] ~ level, data=y)
  assign(paste('col',i,'anv',sep='.'),m)
}
#anova
summary(col.15.anv)

#par(mfrow=c(2,2))
#plot(col.4.anv)

#posthoc
n <- scheffe.test(col.15.anv, "level",console =TRUE)
View(n)



#########################################################
#3. Health care systems
#########################################################

##############
#Boxplotting
##############
y<-ds0


par(mfrow = c(2,3))

#par(mfrow=c(1,1))
for(i in 35:40){
  boxplot(y[,i]~y[,1], main = i)
}
colnames(y)

##############
#ANOVA & POST_HOC
##############
for(i in 35:40){
  m <- aov(y[,i] ~ level, data=y)
  assign(paste('col',i,'anv',sep='.'),m)
}
#anova
summary(col.40.anv) # can change i value to see other result

#par(mfrow=c(2,2))
#plot(col.4.anv)

#posthoc
n <- scheffe.test(col.35.anv, "level",console =TRUE) # can change i value to see other result
View(n)




#########################################################
#4. Age distribution
#########################################################
x<-ds0
x <- data.frame(x)
x <- x%>%
  mutate(num_lv=ifelse(level=="level1",1,(ifelse(level=="level2",2,3))))
par(mfrow = c(1,2))
View(x)


##############
#Boxplotting
##############
boxplot(x$over65 ~ x[,1], main = "level vs over65", xlab = "Level", ylab = "over65") #over65

boxplot(x$median.age~x[,1], main = "level vs median age", xlab = "Level", ylab = "median_age") #median age



##############
#ANOVA & POST_HOC
##############

#anova
over65_anv <- aov( over65 ~ level, data = x)
summary(over65_anv)
med_anv <- aov( median.age ~ level, data = x)
summary(med_anv)

#post-hoc
over65_phoc <- scheffe.test(over65_anv, "level",console =TRUE, main="level distribution")
med_phoc <- scheffe.test(med_anv, "level",console =TRUE, main="level distribution")

#correlation coefficient
cor(x$median.age, x$death_rate, method = 'pearson')

cor(x$over65, x$death_rate, method = 'pearson')

cor(x$median.age, x$num_lv, method = 'pearson')

cor(x$over65, x$num_lv, method = 'pearson')




#########################################################
#5. Further Analysis for food sources- animal fats & vegetable oils
#########################################################

a<-read.csv("country_health_indicators_v3.csv")
b<-read_excel("age_data_level.xlsx")

View(a)
disease_data<-a %>% select(Country_Region, animal_fats, vegetable_oils, Cancers....)
View(disease_data)

tot.data<-inner_join(disease_data,b,by="Country_Region")
tot.data <- rename(tot.data, Cancer = Cancers....)
tot.data$Cancer<-round(tot.data$Cancer, 2)

View(tot.data)


#####################################
##over65 vs animal_fat
#####################################
#over65 leveling
#over65.data<-tot.data %>% mutate(level=ifelse(over65<5, "level1", ifelse(over65<10, "level2", ifelse(over65<15, "level3","level4"))))
over65.data<-tot.data %>% mutate(level=ifelse(over65<6, "level1", ifelse(over65<12, "level2", ifelse(over65<18, "level3","level4"))))
View(over65.data)
nrow(over65.data[over65.data$level=="level1",]) #33
nrow(over65.data[over65.data$level=="level2",]) #25
nrow(over65.data[over65.data$level=="level3",]) #11
nrow(over65.data[over65.data$level=="level4",]) #23

af.65.data<-over65.data %>% filter(!is.na(animal_fats))
nrow(af.65.data)

par(mfrow=c(1,2))
boxplot(animal_fats ~ level, data = af.65.data, main = "over65 & animal fat", xlab = "over65 level", ylab = "animal fat") #over65

#anova
m <- aov(animal_fats ~ level, data=af.65.data)
summary(m)

#posthoc
n <- scheffe.test(m, "level",console =TRUE)

#####################################
##over65 vs vegetable_oils
#####################################
vo.65.data<-over65.data %>% filter(!is.na(vegetable_oils))
View(vo.65.data)
boxplot(vegetable_oils ~ level, data = vo.65.data, main = "over65 & vegetable oil", xlab = "over65 level", ylab = "animal fat")

#anova
m <- aov(vegetable_oils ~ level, data=vo.65.data)
summary(m)

#posthoc
n <- scheffe.test(m, "level",console =TRUE)



#####################################
##median age vs animal_fats
#####################################

#medage leveling
summary(tot.data$median.age)
#over65.data<-tot.data %>% mutate(level=ifelse(over65<5, "level1", ifelse(over65<10, "level2", ifelse(over65<15, "level3","level4"))))
med.data<-tot.data %>% mutate(level=ifelse(median.age<24, "level1", ifelse(median.age<32, "level2", ifelse(median.age<40, "level3","level4"))))
View(over65.data)
nrow(med.data[med.data$level=="level1",]) #17
nrow(med.data[med.data$level=="level2",]) #27
nrow(med.data[med.data$level=="level3",]) #19
nrow(med.data[med.data$level=="level4",]) #29

af.med.data<-med.data %>% filter(!is.na(animal_fats))
nrow(af.med.data)

par(mfrow=c(1,1))
boxplot(animal_fats ~ level, data = af.med.data, main = "median age & animal fat", xlab = "med.age level", ylab = "animal fat") #over65

#anova
m <- aov(animal_fats ~ level, data=af.med.data)
summary(m)

#posthoc
n <- scheffe.test(m, "level",console =TRUE)

#####################################
##medianage vs vegetable_oils
#####################################
vo.med.data<-med.data %>% filter(!is.na(vegetable_oils))
View(vo.med.data)
boxplot(vegetable_oils ~ level, data = vo.med.data, main = "median age & vegetable oil", xlab = "med.age level", ylab = "vegetable oil")

#anova
m <- aov(vegetable_oils ~ level, data=vo.med.data)
summary(m)

#posthoc
n <- scheffe.test(m, "level",console =TRUE)






#####################################
##cancer vs animal_fats
#####################################

#cancer leveling
summary(tot.data$Cancer)
can.data<-tot.data %>% mutate(level=ifelse(Cancer<6, "level1", ifelse(Cancer<12, "level2", ifelse(Cancer<18, "level3","level4"))))
View(can.data)
nrow(can.data[can.data$level=="level1",]) #25
nrow(can.data[can.data$level=="level2",]) #26
nrow(can.data[can.data$level=="level3",]) #15
nrow(can.data[can.data$level=="level4",]) #16

af.can.data<-can.data %>% filter(!is.na(animal_fats))
nrow(af.can.data)

par(mfrow=c(1,1))
boxplot(animal_fats ~ level, data = af.can.data, main = "cancer & vegetable oil", xlab = "cancer level", ylab = "animal fat") #over65

#anova
m <- aov(animal_fats ~ level, data=af.can.data)
summary(m)

#posthoc
n <- scheffe.test(m, "level",console =TRUE)

#####################################
##cancer vs vegetable_oils
#####################################
vo.can.data<-can.data %>% filter(!is.na(vegetable_oils))
View(vo.can.data)
boxplot(vegetable_oils ~ level, data = vo.can.data, main = "cancer & vegetable oil", xlab = "cancer level", ylab = "vegetable oil")

#anova
m <- aov(vegetable_oils ~ level, data=vo.can.data)
summary(m)

#posthoc
n <- scheffe.test(m, "level",console =TRUE)










###################################################################################
#Prediction model
###################################################################################

library("readxl")
library("dplyr")
install.packages(c("DMwR", "rattle"))

a<-read_excel("death_rate_2020_05_31.xlsx")
a<-a %>% filter(!is.na(continent))
a <-a %>% mutate(level=ifelse(death_rate<4,"level1",ifelse(death_rate<8,"level2","level3")))
nrow(a[a$level=="level1",]) #67
nrow(a[a$level=="level2",]) #28
nrow(a[a$level=="level3",]) #10
#food data : dataset 1 : 21~42 (aquatic, products other)
#hospital : data 1 : 43,44~48
#disease : data 1 : 9~18
#age : datset 2
dataset1<-read.csv("country_health_indicators_v3.csv")
dataset2<-read_excel("age_data_level.xlsx")
dataset1$Country_Region <- as.character(dataset1$Country_Region)
b<-inner_join(a,dataset1,by=c("location"="Country_Region")) %>% select(6, 3,5,14:23,26,27,28,30:53)
b<-inner_join(b,dataset2[,c(2,4,5)],by=c("location"="Country_Region"))
a<-na.omit(b)
nrow(a[a$level=="level1",]) #24
nrow(a[a$level=="level2",]) #12
nrow(a[a$level=="level3",]) #5


################################################
#step 1 - normalization
#quantile normalization - 특정 sample이 same distribution 가진다는 것.
#library(preprocessCore)
#factors<-t(a[,-1:-3])
#factors_m<-data.matrix(factors)
#factors_m<-normalize.quantiles(factors_m)
#rownames(factors_m)<-rownames(factors)

########################
#min-max
factors<-a[,-1:-3]
normalize_min_max <- function(x)
{
  return(round((x- min(x))/(max(x)-min(x)),2))
}
factors <- apply(factors, MARGIN=2, normalize_min_max)
factors<-as.data.frame(factors)
factors$level=as.factor(a$level)
#class-imbalance problem - how to deal with
#Oversampling : SMOTE algorithm 
library(DMwR)
data<-factors %>% mutate(level=ifelse(level=="level1"|level=="level2", "help","level3"))
data$level=as.factor(data$level)
newdata_level3<-SMOTE(level ~.,data, perc.over=500, perc.under=0)
newdata_level3<-na.omit(newdata_level3)[1:24,]
data<-factors %>% mutate(level=ifelse(level=="level1"|level=="level3", "help","level2"))
data$level=as.factor(data$level)
newdata_level2<-SMOTE(level ~.,data, perc.over=100, perc.under=0)
newdata_level1<-factors %>% filter(level=="level1")
oversampled<-rbind(newdata_level1,newdata_level2,newdata_level3)
oversampled <- oversampled %>% select("level",1:39)
write_xlsx(oversampled, "oversampled.xlsx")

########################
#decision tree construction - disease
set.seed(121)
library(caret)
library(rpart)
data<-read_excel("oversampled.xlsx")
data<-data[,c(1,2:11)]
partition <- createDataPartition(data$level, p=0.8, list=FALSE)
train <- data[partition,]
test <- data[-partition,]
#train
fitControl <- trainControl(method = 'cv', number=5) # 5 Fold cross validation
Grid <- expand.grid(cp=seq(0, 0.1, 0.005)) # cp: Complexity parameter: Increases as tree size increases (Split increases)
tree.fit <- train(level ~.,
                  data=train,
                  method="rpart",
                  trControl=fitControl,
                  tuneGrid=Grid)
tree.fit
#optimal cp=0.03
#plotting
par(mfrow=c(1,1))
#accuracy according to complexity parameter
png(file =paste("Optimal Complexity Parameter - decision tree for disease.png"))
plot(tree.fit, main="Optimal Complexity Parameter")
dev.off()
#variance importance plot
par(mfrow=c(1,1))
png(file =paste("Variance importance - decision tree for disease.png"))
plot(varImp(tree.fit), main="Variance Importance") 
dev.off()
#plot of tree
library(rattle)
par(mfrow=c(1,1))
png(file =paste("Raw decision tree for disease.png"))
fancyRpartPlot(tree.fit$finalModel,main="Decision Tree")
dev.off()
#test
tree.pred = predict(tree.fit, newdata = test)
confusionMatrix(tree.pred, factor(test$level))

#bagging
bag.fit <- train(level ~.,
                 data=train,
                 method="treebag",
                 trControl=fitControl,
                 nbagg=100)
bag.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - bagging for disease.png"))
plot(varImp(bag.fit),main="Variance Importance - Bagging")
dev.off()
bag.pred = predict(bag.fit, newdata = test)
confusionMatrix(bag.pred, factor(test$level))
#random forest
library(randomForest)
fitControl <- trainControl(method = 'cv', number=5)
Grid <- expand.grid(.mtry=c(1:8))
rf.fit <- train(level ~.,
                data=data,
                method="rf",
                trControl=fitControl,
                tuneGrid=Grid)
rf.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - RandomForest for disease.png"))
plot(varImp(rf.fit),main="Variance Importance - RandomForest")
dev.off()
rf.pred = predict(rf.fit, newdata = test)
confusionMatrix(rf.pred, factor(test$level))


#decision tree construction - food
data<-read_excel("oversampled.xlsx")
data<-data[,c(1,12:32)]
#data<-data[,c(-16)]
partition <- createDataPartition(data$level, p=0.8, list=FALSE)
train <- data[partition,]
test <- data[-partition,]
#train
fitControl <- trainControl(method = 'cv', number=5) # 5 Fold cross validation
Grid <- expand.grid(cp=seq(0, 0.3, 0.005)) # cp: Complexity parameter: Increases as tree size increases (Split increases)
tree.fit <- train(level ~.,
                  data=train,
                  method="rpart",
                  trControl=fitControl,
                  tuneGrid=Grid)
tree.fit
#optimal cp=0.215
#plotting
par(mfrow=c(1,1))
#accuracy according to complexity parameter
png(file =paste("Optimal Complexity Parameter - decision tree for food.png"))
plot(tree.fit, main="Optimal Complexity Parameter")
dev.off()
#variance importance plot
par(mfrow=c(1,1))
png(file =paste("Variance importance - decision tree for food.png"))
plot(varImp(tree.fit), main="Variance Importance") 
dev.off()
#plot of tree
library(rattle)
par(mfrow=c(1,1))
png(file =paste("Raw decision tree for food.png"))
fancyRpartPlot(tree.fit$finalModel,main="Decision Tree")
dev.off()
#test
tree.pred = predict(tree.fit, newdata = test)
confusionMatrix(tree.pred, factor(test$level))

#bagging
bag.fit <- train(level ~.,
                 data=train,
                 method="treebag",
                 trControl=fitControl,
                 nbagg=100)
bag.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - bagging for food.png"))
plot(varImp(bag.fit),main="Variance Importance - Bagging")
dev.off()
bag.pred = predict(bag.fit, newdata = test)
confusionMatrix(bag.pred, factor(test$level))
#random forest
library(randomForest)
fitControl <- trainControl(method = 'cv', number=5)
Grid <- expand.grid(.mtry=c(1:8))
rf.fit <- train(level ~.,
                data=data,
                method="rf",
                trControl=fitControl,
                tuneGrid=Grid)
rf.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - RandomForest for food.png"))
plot(varImp(rf.fit),main="Variance Importance - RandomForest")
dev.off()
rf.pred = predict(rf.fit, newdata = test)
confusionMatrix(rf.pred, factor(test$level))

#decision tree construction - hospital
data<-read_excel("oversampled.xlsx")
data<-data[,c(1,33:38)]
partition <- createDataPartition(data$level, p=0.8, list=FALSE)
train <- data[partition,]
test <- data[-partition,]
#train
fitControl <- trainControl(method = 'cv', number=5) # 5 Fold cross validation
Grid <- expand.grid(cp=seq(0, 0.3, 0.005)) # cp: Complexity parameter: Increases as tree size increases (Split increases)
tree.fit <- train(level ~.,
                  data=train,
                  method="rpart",
                  trControl=fitControl,
                  tuneGrid=Grid)
tree.fit
#optimal cp=0.215
#plotting
par(mfrow=c(1,1))
#accuracy according to complexity parameter
png(file =paste("Optimal Complexity Parameter - decision tree for hospital.png"))
plot(tree.fit, main="Optimal Complexity Parameter")
dev.off()
#variance importance plot
par(mfrow=c(1,1))
png(file =paste("Variance importance - decision tree for hospital.png"))
plot(varImp(tree.fit), main="Variance Importance") 
dev.off()
#plot of tree
library(rattle)
par(mfrow=c(1,1))
png(file =paste("Raw decision tree for hospital.png"))
fancyRpartPlot(tree.fit$finalModel,main="Decision Tree")
dev.off()
#test
tree.pred = predict(tree.fit, newdata = test)
confusionMatrix(tree.pred, factor(test$level))

#bagging
bag.fit <- train(level ~.,
                 data=train,
                 method="treebag",
                 trControl=fitControl,
                 nbagg=100)
bag.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - bagging for hospital.png"))
plot(varImp(bag.fit),main="Variance Importance - Bagging")
dev.off()
bag.pred = predict(bag.fit, newdata = test)
confusionMatrix(bag.pred, factor(test$level))
#random forest
library(randomForest)
fitControl <- trainControl(method = 'cv', number=5)
Grid <- expand.grid(.mtry=c(1:6))
rf.fit <- train(level ~.,
                data=data,
                method="rf",
                trControl=fitControl,
                tuneGrid=Grid)
rf.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - RandomForest for hospital.png"))
plot(varImp(rf.fit),main="Variance Importance - RandomForest")
dev.off()
rf.pred = predict(rf.fit, newdata = test)
confusionMatrix(rf.pred, factor(test$level))




#decision tree construction - age
data<-read_excel("oversampled.xlsx")
data<-data[,c(1,39:40)]
partition <- createDataPartition(data$level, p=0.8, list=FALSE)
train <- data[partition,]
test <- data[-partition,]
#train
fitControl <- trainControl(method = 'cv', number=5) # 5 Fold cross validation
Grid <- expand.grid(cp=seq(0, 0.3, 0.005)) # cp: Complexity parameter: Increases as tree size increases (Split increases)
tree.fit <- train(level ~.,
                  data=train,
                  method="rpart",
                  trControl=fitControl,
                  tuneGrid=Grid)
tree.fit
#optimal cp=0.215
#plotting
par(mfrow=c(1,1))
#accuracy according to complexity parameter
png(file =paste("Optimal Complexity Parameter - decision tree for age.png"))
plot(tree.fit, main="Optimal Complexity Parameter")
dev.off()
#variance importance plot
par(mfrow=c(1,1))
png(file =paste("Variance importance - decision tree for age.png"))
plot(varImp(tree.fit), main="Variance Importance") 
dev.off()
#plot of tree
library(rattle)
par(mfrow=c(1,1))
png(file =paste("Raw decision tree for age.png"))
fancyRpartPlot(tree.fit$finalModel,main="Decision Tree")
dev.off()
#test
tree.pred = predict(tree.fit, newdata = test)
confusionMatrix(tree.pred, factor(test$level))

#bagging
bag.fit <- train(level ~.,
                 data=train,
                 method="treebag",
                 trControl=fitControl,
                 nbagg=100)
bag.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - bagging for age.png"))
plot(varImp(bag.fit),main="Variance Importance - Bagging")
dev.off()
bag.pred = predict(bag.fit, newdata = test)
confusionMatrix(bag.pred, factor(test$level))
#random forest
library(randomForest)
fitControl <- trainControl(method = 'cv', number=5)
Grid <- expand.grid(.mtry=c(1:2))
rf.fit <- train(level ~.,
                data=data,
                method="rf",
                trControl=fitControl,
                tuneGrid=Grid)
rf.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - RandomForest for age.png"))
plot(varImp(rf.fit),main="Variance Importance - RandomForest")
dev.off()
rf.pred = predict(rf.fit, newdata = test)
confusionMatrix(rf.pred, factor(test$level))



#for all
data<-read_excel("oversampled.xlsx")
data<-data[,c(-26)]
partition <- createDataPartition(data$level, p=0.8, list=FALSE)
train <- data[partition,]
test <- data[-partition,]
#train
fitControl <- trainControl(method = 'cv', number=5) # 5 Fold cross validation
Grid <- expand.grid(cp=seq(0, 0.1, 0.005)) # cp: Complexity parameter: Increases as tree size increases (Split increases)
tree.fit <- train(level ~.,
                  data=train,
                  method="rpart",
                  trControl=fitControl,
                  tuneGrid=Grid)
tree.fit
#optimal cp=0.1
#plotting
par(mfrow=c(1,1))
#accuracy according to complexity parameter
png(file =paste("Optimal Complexity Parameter - decision tree for all.png"))
plot(tree.fit, main="Optimal Complexity Parameter")
dev.off()
#variance importance plot
par(mfrow=c(1,1))
png(file =paste("Variance importance - decision tree for all.png"))
plot(varImp(tree.fit), main="Variance Importance") 
dev.off()
#plot of tree
library(rattle)
par(mfrow=c(1,1))
png(file =paste("Raw decision tree for all.png"))
fancyRpartPlot(tree.fit$finalModel,main="Decision Tree")
dev.off()
#test
tree.pred = predict(tree.fit, newdata = test)
confusionMatrix(tree.pred, factor(test$level))

#bagging
bag.fit <- train(level ~.,
                 data=train,
                 method="treebag",
                 trControl=fitControl,
                 nbagg=100)
bag.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - bagging for all.png"))
plot(varImp(bag.fit),main="Variance Importance - Bagging")
dev.off()
bag.pred = predict(bag.fit, newdata = test)
confusionMatrix(bag.pred, factor(test$level))
#random forest
library(randomForest)
fitControl <- trainControl(method = 'cv', number=5)
Grid <- expand.grid(.mtry=c(1:8))
rf.fit <- train(level ~.,
                data=data,
                method="rf",
                trControl=fitControl,
                tuneGrid=Grid)
rf.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - RandomForest for all.png"))
plot(varImp(rf.fit),main="Variance Importance - RandomForest")
dev.off()
rf.pred = predict(rf.fit, newdata = test)
confusionMatrix(rf.pred, factor(test$level))
##################################################
data<-read_excel("oversampled.xlsx")
data<-data[,c("level","over65","vegetable_oils","Cancers....","median.age","animal_fats")]
partition <- createDataPartition(data$level, p=0.8, list=FALSE)
train <- data[partition,]
test <- data[-partition,]
#train
fitControl <- trainControl(method = 'cv', number=5) # 5 Fold cross validation
Grid <- expand.grid(cp=seq(0, 0.1, 0.005)) # cp: Complexity parameter: Increases as tree size increases (Split increases)
tree.fit <- train(level ~.,
                  data=train,
                  method="rpart",
                  trControl=fitControl,
                  tuneGrid=Grid)
tree.fit
#optimal cp=0.03
#plotting
par(mfrow=c(1,1))
#accuracy according to complexity parameter
png(file =paste("Optimal Complexity Parameter - decision tree ideal.png"))
plot(tree.fit, main="Optimal Complexity Parameter")
dev.off()
#variance importance plot
par(mfrow=c(1,1))
png(file =paste("Variance importance - decision tree ideal.png"))
plot(varImp(tree.fit), main="Variance Importance") 
dev.off()
#plot of tree
library(rattle)
par(mfrow=c(1,1))
png(file =paste("Raw decision tree ideal.png"))
fancyRpartPlot(tree.fit$finalModel,main="Decision Tree")
dev.off()
#test
tree.pred = predict(tree.fit, newdata = test)
confusionMatrix(tree.pred, factor(test$level))

#bagging
bag.fit <- train(level ~.,
                 data=train,
                 method="treebag",
                 trControl=fitControl,
                 nbagg=100)
bag.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - bagging ideal.png"))
plot(varImp(bag.fit),main="Variance Importance - Bagging")
dev.off()
bag.pred = predict(bag.fit, newdata = test)
confusionMatrix(bag.pred, factor(test$level))
#random forest
library(randomForest)
fitControl <- trainControl(method = 'cv', number=5)
Grid <- expand.grid(.mtry=c(1:4))
rf.fit <- train(level ~.,
                data=data,
                method="rf",
                trControl=fitControl,
                tuneGrid=Grid)
rf.fit
par(mfrow=c(1,1))
png(file =paste("Variance importance - RandomForest ideal.png"))
plot(varImp(rf.fit),main="Variance Importance - RandomForest")
dev.off()
rf.pred = predict(rf.fit, newdata = test)
confusionMatrix(rf.pred, factor(test$level))

#########################################################
#
#########################################################
