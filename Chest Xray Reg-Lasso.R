#------------------------------------READING-IMAGE-------------------------------------------
#-------------------------------------NORMAL-TEST--------------------------------------------
setwd("/Volumes/Seagate Backup Plus Drive/chest_xray/test/NORMAL")
library(OpenImageR)
files <- list.files(path="/Volumes/Seagate Backup Plus Drive/chest_xray/test/NORMAL/", 
                    pattern=".jpeg",all.files=T, full.names=F, no.. = T)
nor<-c(1:4096)
for(i in 1:length(files))
{
  a<-readImage(files[i])
  a<-resizeImage(a,64,64)
  a<-as.vector(a)
  a<-t(a)
  if(length(a)==4096)
    nor<-rbind(nor,a)
}
dim(nor)
nor<-nor[-1,]
nor<-data.frame(nor)
write.csv(nor,"/Volumes/Seagate Backup Plus Drive/chest_xray/test/NORMAL/N_Test.csv")


#------------------------------------PNEUMONIA-TEST------------------------------------------
setwd("/Volumes/Seagate Backup Plus Drive/chest_xray/test/PNEUMONIA")
library(OpenImageR)
files <- list.files(path="/Volumes/Seagate Backup Plus Drive/chest_xray/test/PNEUMONIA/", 
                    pattern=".jpeg",all.files=T, full.names=F, no.. = T)
nor<-c(1:4096)
for(i in 1:length(files))
{
  a<-readImage(files[i])
  a<-resizeImage(a,64,64)
  a<-as.vector(a)
  a<-t(a)
  if(length(a)==4096)
    nor<-rbind(nor,a)
}
dim(nor)
nor<-nor[-1,]
nor<-data.frame(nor)
write.csv(nor,"/Volumes/Seagate Backup Plus Drive/chest_xray/test/PNEUMONIA/P_Test.csv")


#-------------------------------------NORMAL-TRAIN-------------------------------------------
setwd("/Volumes/Seagate Backup Plus Drive/chest_xray/train/NORMAL")
library(OpenImageR)
files <- list.files(path="/Volumes/Seagate Backup Plus Drive/chest_xray/train/NORMAL/", 
                    pattern=".jpeg",all.files=T, full.names=F, no.. = T)
nor<-c(1:4096)
for(i in 1:length(files))
{
  a<-readImage(files[i])
  a<-resizeImage(a,64,64)
  a<-as.vector(a)
  a<-t(a)
  if(length(a)==4096)
    nor<-rbind(nor,a)
}
dim(nor)
nor<-nor[-1,]
nor<-data.frame(nor)
write.csv(nor,"/Volumes/Seagate Backup Plus Drive/chest_xray/train/NORMAL/N_Train.csv")


#------------------------------------PNEUMONIA-TRAIN-----------------------------------------
setwd("/Volumes/Seagate Backup Plus Drive/chest_xray/train/PNEUMONIA")
library(OpenImageR)
files <- list.files(path="/Volumes/Seagate Backup Plus Drive/chest_xray/train/PNEUMONIA/", 
                    pattern=".jpeg",all.files=T, full.names=F, no.. = T)

nor<-c(1:4096)
nor<-t(nor)

for(i in 1:length(files))
{
  a<-readImage(files[i])
  j<-dim(a)
  j<-as.vector(j)
  if(length(j)==3)
    a<- rgb_2gray(a)
  a<-resizeImage(a,64,64)
  a<-as.vector(a)
  a<-t(a)
  if(length(a)==4096)
    nor<-rbind(nor,a)
}
dim(a)
class(j)
dim(nor)
View(nor)
nor<-nor[-1,]
nor<-data.frame(nor)
write.csv(nor,"/Volumes/Seagate Backup Plus Drive/chest_xray/train/PNEUMONIA/P_Train.csv")


#-------------------------------------MODEL-BUILDING-----------------------------------------
ntest<-read.csv("/Users/jayprajapati/Downloads/SEM - 4/SM/PROJECT/N_Test.csv")
ntrain<-read.csv("/Users/jayprajapati/Downloads/SEM - 4/SM/PROJECT/N_Train.csv")
ptest<-read.csv("/Users/jayprajapati/Downloads/SEM - 4/SM/PROJECT/P_Test.csv")
ptrain<-read.csv("/Users/jayprajapati/Downloads/SEM - 4/SM/PROJECT/P_Train.csv")

dim(ntest)
dim(ntrain)
dim(ptest)
dim(ptrain)

ptrain$type<-1
ptest$type<-1
ntrain$type<-0
ntest$type<-0

train<-rbind(ptrain,ntrain)
test<-rbind(ptest,ntest)

train<-rbind(ptrain[1:100,],ntrain[1:100,])
test<-rbind(ptest[1:25,],ntest[1:25,])

dim(train)
dim(test)

train<-train[,-1]
test<-test[,-1]

train$type<-as.factor(train$type)
test$type<-as.factor(test$type)

library(glmnet)

X=model.matrix(type~.,train)

Y=train$type

reg.fit<-glmnet(X,Y,alpha = 1,family = "binomial")

plot(reg.fit)

cv.reg.fit=cv.glmnet(X,Y,alpha=1,family="binomial")

k<-cv.reg.fit$lambda.min

out=glmnet(X,Y,alpha=1,lambda = k,family="binomial")

x<-predict(out,type="coefficients",s=k)[1:4098,]

for (i in c(1:4098))
{
  if((x[i]>1 && x[i]<2) || (x[i]<(-1) &&  x[i]>(-2)))
    print(x[i])
}

j<-names(j)

j<-as.vector(j)

lg.fit1<-glm(type~X25+X65+X482+X1236+X1452+X1470+X1747+X1795+X1836+X1887+X1925+X2001+X2126+X2231+X2542+X2671+X2736+X2965+X3385,data=train,family = "binomial")
summary(lg.fit1)

lg.fit1<-glm(type~X25+X65+X482+X1236+X1452+X1747+X1795+X1836+X1925+X2001+X2126+X2231+X2542+X2671+X2736+X2965+X3385+X27+X66+X200+X448+X465+X831+X908+X1102+X1113+X1178+X1253+X1269+X1336+X1369+X1421+X1482+X1551+X1560+X1685+X1886+X1994+X2021+X2123+X2178+X2193+X2450+X2505+X2507+X2546+X2601+X2625+X2781+X3077+X3413+X3437+X3447+X3474+X3964,data=train,family = "binomial")
summary(lg.fit1)


#----------------------------------------PREDICTION------------------------------------------
pred<-predict(lg.fit1,newdata = test, type = "response")
pred1<-pred
pred<-ifelse(pred>0.5,1,0)

table(test$type,pred)

(60+341)/529

a<-charToRaw("a")
class(a)
