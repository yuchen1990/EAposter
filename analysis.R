edata<-read.table('LOC_rawdata.csv',header=TRUE,sep=",")
summary(edata)

library(ggplot2)
library(ggjoy)


edata$Dataset<-factor(edata$Dataset,levels = c("postgres", "platform", "mozilla", "jdt", "columba", "bugzilla"))
ggplot(edata, aes(x=log(LOC+1,2), y=Dataset))+ geom_joy(aes(fill=Dataset))+theme(legend.position="none")



data1<-NULL
c("bugzilla", "columba", "jdt",  "platform", "postgres", "mozilla")


library(fBasics)

data1<-edata[edata$Dataset=="bugzilla",]
hist(data1$LOC,200)
summary(data1)
skewness(data1$LOC)

data1<-edata[edata$Dataset=="columba",]
hist(data1$LOC,200)
summary(data1)
skewness(data1$LOC)

data1<-edata[edata$Dataset=="jdt",]
hist(data1$LOC,200)
summary(data1)
skewness(data1$LOC)

data1<-edata[edata$Dataset=="mozilla",]
hist(data1$LOC,200)
summary(data1)
skewness(data1$LOC)

data1<-edata[edata$Dataset=="platform",]
hist(data1$LOC,200)
summary(data1)
skewness(data1$LOC)

data1<-edata[edata$Dataset=="postgres",]
hist(data1$LOC,200)
summary(data1)
skewness(data1$LOC)

edata<-read.table('10r10f_rawdata.csv',header=TRUE,sep=",")
datasets<-methods<-mtdtypes<-NULL
V_Popt<-V_ACC<-V_AUC<-NULL

for (data in unique(edata$Dataset)){
  #print(data)
  for (mtd in unique(edata$Method)){
    for (type in unique(edata$MethodType)){
      if (length(with(edata,which(Dataset==data & Method==mtd & MethodType==type)))>0){
        data1<-edata[with(edata,which(Dataset==data & Method==mtd & MethodType==type)),]
        sub.Popt<-mean(data1$Popt)
        sub.acc<-mean(data1$ACC)
        sub.auc<-mean(data1$AUC)
        datasets<-c(datasets,data)
        methods<-c(methods,mtd)
        mtdtypes<-c(mtdtypes,type)
        V_Popt<-c(V_Popt,sub.Popt)
        V_ACC<-c(V_ACC,sub.acc)
        V_AUC<-c(V_AUC,sub.auc)
      }
      
    }
  }
}

edata<-data.frame(Dataset=datasets,Method=methods,MethodType=mtdtypes,Popt=V_Popt,ACC=V_ACC,AUC=V_AUC)

edata$MethodType<-factor(edata$MethodType, levels = c("Sup.R-pE","Sup.non-EA","Sup.R-ad","EALR","Sup.R-dd", "Unsup", "Random-pE"))

summary(edata)
library(ggplot2)
edata<-edata[edata$MethodType!="Random-pE",]
ggplot(edata, aes(AUC,Popt))+
  geom_point(aes(color=MethodType,shape=Dataset),size=1.8)+
  stat_smooth(aes(color=MethodType), method="glm", se=F)+
  labs(title = "AUC vs Popt while including LOC's outliers")
ggplot(edata, aes(AUC,ACC))+
  geom_point(aes(color=MethodType,shape=Dataset),size=1.8)+
  stat_smooth(aes(color=MethodType), method="glm", se=F)+
  labs(title = "AUC vs ACC while including LOC's outliers")

edata<-read.table('cutTukeyFence_rawdata.csv',header=TRUE,sep=",")
datasets<-methods<-mtdtypes<-NULL
V_Popt<-V_ACC<-V_AUC<-NULL

for (data in unique(edata$Dataset)){
  #print(data)
  for (mtd in unique(edata$Method)){
    for (type in unique(edata$MethodType)){
      if (length(with(edata,which(Dataset==data & Method==mtd & MethodType==type)))>0){
        data1<-edata[with(edata,which(Dataset==data & Method==mtd & MethodType==type)),]
        sub.Popt<-mean(data1$Popt)
        sub.acc<-mean(data1$ACC)
        sub.auc<-mean(data1$AUC)
        datasets<-c(datasets,data)
        methods<-c(methods,mtd)
        mtdtypes<-c(mtdtypes,type)
        V_Popt<-c(V_Popt,sub.Popt)
        V_ACC<-c(V_ACC,sub.acc)
        V_AUC<-c(V_AUC,sub.auc)
      }
      
    }
  }
}

edata<-data.frame(Dataset=datasets,Method=methods,MethodType=mtdtypes,Popt=V_Popt,ACC=V_ACC,AUC=V_AUC)

edata$MethodType<-factor(edata$MethodType, levels = c("Sup.R-pE","Sup.non-EA","Sup.R-ad","EALR","Sup.R-dd", "Unsup", "Random-pE"))

ggplot(edata, aes(AUC,Popt))+
  geom_point(aes(color=MethodType,shape=Dataset),size=1.8)+
  stat_smooth(aes(color=MethodType), method="glm", se=F)+
  labs(title = "AUC vs Popt while excluding LOC's outliers")
ggplot(edata, aes(AUC,ACC))+
  geom_point(aes(color=MethodType,shape=Dataset),size=1.8)+
  stat_smooth(aes(color=MethodType), method="glm", se=F)+
  labs(title = "AUC vs ACC while excluding LOC's outliers")


