rm(list=ls())

library(rpart)
library(rpart.plot)
library(ggplot2)
library(plot3D)
library(wesanderson)
library(sas7bdat)
library(latex2exp)


a<-read.sas7bdat("C:/Users/cuisa/Desktop/working/Liu/CART/liu5c_test_ml.sas7bdat")


### 25(OH)D >=20 vs 25(OH)D <20 ###

a1= a[,-c(1,47,48,49)]
a1$SEX=as.factor(a1$SEX)
a1$RACEGP=as.factor(a1$RACEGP)
a1$EDU=as.factor(a1$EDU)
a1$poverty=as.factor(a1$poverty)
a1$SMKGP3=as.factor(a1$SMKGP3)
a1$DRINK=as.factor(a1$DRINK)
a1$EXER1=as.factor(a1$EXER1)
a1$DMGP2=as.factor(a1$DMGP2)
a1$region=as.factor(a1$region)
a1$albgp2=as.factor(a1$albgp2)
a1$armgp=as.factor(a1$armgp)
a1$SMIgp2=as.factor(a1$SMIgp2)
a1$Anemia=as.factor(a1$Anemia)

a1$vdpgp2[a1$vdpgp2==0]="25(OH)D >=20"
a1$vdpgp2[a1$vdpgp2==1]="25(OH)D <20"



set.seed(1234)


select<-sample(1:nrow(a1),length(a1$vdpgp2)*0.8)
train=a1[select,]
test=a1[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.005)

dtree<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.01)

dtree_prune<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=1.0)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp2,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance










### true in test ###
test$RACEGP = as.character(test$RACEGP)
test$SEX = as.character(test$SEX)


test$Race[test$RACEGP=="1"]="NHW"
test$Race[test$RACEGP=="2"]="NHB"
test$Race[test$RACEGP=="3"]="Other"


test$Gender[test$SEX=="1"]="Female"
test$Gender[test$SEX=="2"]="Male"

test$Race=as.factor(test$Race)
test$Gender=as.factor(test$Gender)
test$Race=factor(test$Race, levels = c("NHB","NHW","Other"))

test$vdpgp2=as.factor(test$vdpgp2)


VitaminD = test$vdpgp2[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = AGE, y = FOP, colour = Race, shape = VitaminD, size= Gender)) +
  scale_color_brewer(palette = 'Set2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25)) +xlab("Age")+ylab("FOP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=15,y=8.5,label="FOP=8.9",size=3,color="red")










## Female ##

VitaminD = test$vdpgp2[test$Gender=="Female"][1:250]
ggplot()+ geom_point(data=test[test$Gender=="Female",][1:250,],aes(x = AGE, y = FOP, colour = Race, shape = VitaminD),size=3) +
scale_color_brewer(palette = 'Set2') +
scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25)) +xlab("Age")+ylab("FOP")+
theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=15,y=8.5,label="FOP=8.9",size=3,color="red")

 



## Male ##
VitaminD = test$vdpgp2[test$Gender=="Male"][1:250]
ggplot()+ geom_point(data=test[test$Gender=="Male",][1:250,],aes(x = AGE, y = FOP, colour = Race, shape = VitaminD),size=3) +
  scale_color_brewer(palette = 'Set2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25)) +xlab("Age")+ylab("FOP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=15,y=8.5,label="FOP=8.9",size=3,color="red")








### pred in test ###


test = cbind(test,predtree)


test$predtree=as.factor(test$predtree)
VitaminD = test$predtree[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = AGE, y = FOP, colour = Race, shape = VitaminD, size= Gender)) +
  scale_color_brewer(palette = 'Set2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25)) +xlab("Age")+ylab("FOP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=15,y=8.5,label="FOP=8.9",size=3,color="red")



## Female ##

VitaminD = test$predtree[test$Gender=="Female"][1:250]
ggplot()+ geom_point(data=test[test$Gender=="Female",][1:250,],aes(x = AGE, y = FOP, colour = Race, shape = VitaminD),size=3) +
  scale_color_brewer(palette = 'Set2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25)) +xlab("Age")+ylab("FOP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=15,y=8.5,label="FOP=8.9",size=3,color="red")





## Male ##
VitaminD = test$predtree[test$Gender=="Male"][1:250]
ggplot()+ geom_point(data=test[test$Gender=="Male",][1:250,],aes(x = AGE, y = FOP, colour = Race, shape = VitaminD),size=3) +
  scale_color_brewer(palette = 'Set2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25)) +xlab("Age")+ylab("FOP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=16,y=8.5,label="FOP=8.9",size=3,color="red")





### merge in test ###
test$vdpgp2=as.character(test$vdpgp2)
test$predtree = as.character(test$predtree)

test$merge = rep("False Prediction", length(test$predtree))
test$merge[test$vdpgp2=="25(OH)D <20"&test$predtree=="25(OH)D <20"]="True <20"
test$merge[test$vdpgp2=="25(OH)D >=20"&test$predtree=="25(OH)D >=20"]="True >=20"


test$merge=as.factor(test$merge)
VitaminD = test$merge[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = AGE, y = FOP, colour = Race, shape = VitaminD, size= Gender)) +
  scale_color_brewer(palette = 'Set2') +
  scale_shape_manual(values = c(4,2,16),labels = unname(TeX(c("False estimated","True < 20", "$True \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25)) +xlab("Age")+ylab("FOP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=15,y=8.5,label="FOP=8.9",size=3,color="red")+
annotate("text",x=22,y=25,label="Accuracy: 71.1%",size=3,color="Black")+
  annotate("text",x=22,y=24,label="Precision:   68.4%",size=3,color="Black")+
  annotate("text",x=22,y=23,label="Recall:        63.0%",size=3,color="Black")+
  annotate("text",x=22,y=22,label="F1 score:   65.6%",size=3,color="Black")+ guides(shape = guide_legend(reverse=T))




## Female ##
predtree_f<-predict(dtree_prune,newdata=test[test$Gender=="Female",],type="class")  

acctable=table(test$vdpgp2[test$Gender=="Female"],predtree_f,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)





VitaminD = test$merge[test$Gender=="Female"][1:250]
ggplot()+ geom_point(data=test[test$Gender=="Female",][1:250,],aes(x = AGE, y = FOP, colour = Race, shape = VitaminD),size=3) +
  scale_color_brewer(palette = 'Set2') +
  scale_shape_manual(values = c(4,2,16),labels = unname(TeX(c("False estimated","True < 20", "$True \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90))+
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25))+xlab("Age")+ylab("FOP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  annotate("text",x=22,y=25,label="Accuracy: 72.2%",size=3,color="Black")+
  annotate("text",x=22,y=24,label="Precision:   65.8%",size=3,color="Black")+
  annotate("text",x=22,y=23,label="Recall:        46.9%",size=3,color="Black")+
  annotate("text",x=22,y=22,label="F1 score:   54.8%",size=3,color="Black")+ guides(shape = guide_legend(reverse=T))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=16,y=8.5,label="FOP=8.9",size=3,color="red")











## Male ##

predtree_m<-predict(dtree_prune,newdata=test[test$Gender=="Male",],type="class")  

acctable=table(test$vdpgp2[test$Gender=="Male"],predtree_m,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)






VitaminD = test$merge[test$Gender=="Male"][1:250]

ggplot()+ geom_point(data=test[test$Gender=="Male",][1:250,],aes(x = AGE, y = FOP, colour = Race, shape = VitaminD), size = 3) +
  scale_color_brewer(palette = 'Set2') +
  scale_shape_manual(values = c(4,2,16),labels = unname(TeX(c("False estimated","True < 20", "$True \\geq 20$")))     )+
 xlab("Age")+ylab("FOP")+scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25))+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=16,y=8.5,label="FOP=8.9",size=3,color="red")+
  annotate("text",x=22,y=25,label="Accuracy: 70.2%",size=3,color="Black")+
  annotate("text",x=22,y=24,label="Precision:   69.6%",size=3,color="Black")+
  annotate("text",x=22,y=23,label="Recall:        73.2%",size=3,color="Black")+
  annotate("text",x=22,y=22,label="F1 score:   71.3%",size=3,color="Black")+ guides(shape = guide_legend(reverse=T))

















### 25(OH)D >=30 vs 25(OH)D 20-29 vs 25(OH)D <30 ###

a2= a[,-c(1,47,48,50)]
a2$SEX=as.factor(a2$SEX)
a2$RACEGP=as.factor(a2$RACEGP)
a2$EDU=as.factor(a2$EDU)
a2$poverty=as.factor(a2$poverty)
a2$SMKGP3=as.factor(a2$SMKGP3)
a2$DRINK=as.factor(a2$DRINK)
a2$EXER1=as.factor(a2$EXER1)
a2$DMGP2=as.factor(a2$DMGP2)
a2$region=as.factor(a2$region)
a2$albgp2=as.factor(a2$albgp2)
a2$armgp=as.factor(a2$armgp)
a2$SMIgp2=as.factor(a2$SMIgp2)
a2$Anemia=as.factor(a2$Anemia)


a2$vdpgp3[a2$vdpgp3==0]="25(OH)D >=30"
a2$vdpgp3[a2$vdpgp3==1]="25(OH)D 20-29"
a2$vdpgp3[a2$vdpgp3==2]="25(OH)D <20"


set.seed(1234)


select<-sample(1:nrow(a2),length(a2$vdpgp3)*0.8)
train=a2[select,]
test=a2[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.005)

dtree<-rpart(vdpgp3~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.01)

dtree_prune<-rpart(vdpgp3~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=104, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.965)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp3,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2]+acctable[3,3])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]+acctable[3,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]+acctable[1,3]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]+acctable[3,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]+acctable[1,3]))
2*(r*p)/(r+p)


dtree$variable.importance





### MetSyn – yes / no ###

a3= a[,-c(1,48,49,50)]
a3$SEX=as.factor(a3$SEX)
a3$RACEGP=as.factor(a3$RACEGP)
a3$EDU=as.factor(a3$EDU)
a3$poverty=as.factor(a3$poverty)
a3$SMKGP3=as.factor(a3$SMKGP3)
a3$DRINK=as.factor(a3$DRINK)
a3$EXER1=as.factor(a3$EXER1)
a3$DMGP2=as.factor(a3$DMGP2)
a3$region=as.factor(a3$region)
a3$albgp2=as.factor(a3$albgp2)
a3$armgp=as.factor(a3$armgp)
a3$SMIgp2=as.factor(a3$SMIgp2)
a3$Anemia=as.factor(a3$Anemia)



a3$MetSyn[a3$MetSyn==0]="MetSyn = 0"
a3$MetSyn[a3$MetSyn==1]="MetSyn = 1"




set.seed(1234)


select<-sample(1:nrow(a3),length(a3$MetSyn)*0.8)
train=a3[select,]
test=a3[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.001)

dtree<-rpart(MetSyn~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.003)

dtree_prune<-rpart(MetSyn~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.6,varlen=4)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$MetSyn,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance



###  serum 25(OH)D is a continuous variable ###


a4= a[,-c(1,47,49,50)]
a4$SEX=as.factor(a4$SEX)
a4$RACEGP=as.factor(a4$RACEGP)
a4$EDU=as.factor(a4$EDU)
a4$poverty=as.factor(a4$poverty)
a4$SMKGP3=as.factor(a4$SMKGP3)
a4$DRINK=as.factor(a4$DRINK)
a4$EXER1=as.factor(a4$EXER1)
a4$DMGP2=as.factor(a4$DMGP2)
a4$region=as.factor(a4$region)
a4$albgp2=as.factor(a4$albgp2)
a4$armgp=as.factor(a4$armgp)
a4$SMIgp2=as.factor(a4$SMIgp2)
a4$Anemia=as.factor(a4$Anemia)



set.seed(1234)


select<-sample(1:nrow(a4),length(a4$vdp_c)*0.8)
train=a4[select,]
test=a4[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.003)

dtree<-rpart(vdp_c~.,data=train, method="anova", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.0073)

dtree_prune<-rpart(vdp_c~.,data=train, method="anova", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra = 100, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=1)


pred<-predict(dtree_prune,test)
residual=data.frame(x=1:length(pred), residual=test$vdp_c-pred)

ggplot(data = residual, mapping = aes(x = x, y = residual)) + geom_point()+xlab("data points in validation set")+
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed")+scale_y_continuous(breaks=seq(-25,50,5),
                                                                                      limits=c(-25,50)) 


dtree$variable.importance





### 25(OH)D >=30 vs 25(OH)D <30 ###

vdpgp2_2 = rep(0,nrow(a))

a5=cbind(a,vdpgp2_2)

a5$vdpgp2_2[a5$vdpgp3==0]="25(OH)D >=30"
a5$vdpgp2_2[a5$vdpgp3==1]="25(OH)D <30"
a5$vdpgp2_2[a5$vdpgp3==2]="25(OH)D <30"

a5= a5[,-c(1,47,48,49,50)]
a5$SEX=as.factor(a5$SEX)
a5$RACEGP=as.factor(a5$RACEGP)
a5$EDU=as.factor(a5$EDU)
a5$poverty=as.factor(a5$poverty)
a5$SMKGP3=as.factor(a5$SMKGP3)
a5$DRINK=as.factor(a5$DRINK)
a5$EXER1=as.factor(a5$EXER1)
a5$DMGP2=as.factor(a5$DMGP2)
a5$region=as.factor(a5$region)
a5$albgp2=as.factor(a5$albgp2)
a5$armgp=as.factor(a5$armgp)
a5$SMIgp2=as.factor(a5$SMIgp2)
a5$Anemia=as.factor(a5$Anemia)





set.seed(1234)


select<-sample(1:nrow(a5),length(a5$vdpgp2_2)*0.8)
train=a5[select,]
test=a5[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.001)

dtree<-rpart(vdpgp2_2~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.006)

dtree_prune<-rpart(vdpgp2_2~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.7)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp2,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance




### Split race for 25(OH)D >=20 vs 25(OH)D <20 ###

a1_r1 = a1[a1$RACEGP==1,]
a1_r1 = a1_r1[,-29]



set.seed(1234)


select<-sample(1:nrow(a1_r1),length(a1_r1$vdpgp2)*0.8)
train=a1_r1[select,]
test=a1_r1[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.005)

dtree<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)



tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.01)

dtree_prune<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.9)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp2,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance

### true in test ###


test$SEX = as.character(test$SEX)



test$Gender[test$SEX=="1"]="Female"
test$Gender[test$SEX=="2"]="Male"


test$Gender=as.factor(test$Gender)

test$vdpgp2=as.factor(test$vdpgp2)


VitaminD = test$vdpgp2[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = AGE, y = WAIST, colour = Gender, shape = VitaminD), size =3) +
  scale_color_brewer(palette = 'Dark2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(60,140,10),limits=c(60,140))+
  xlab("Age")+ylab("Waist")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =107, colour = "red", lty =2)+annotate("text",x=17,y=105,label="Waist=107",size=3,color="red")+
  geom_vline(xintercept =37, colour = "red", lty =2)+annotate("text",x=42,y=60,label="Age=37",size=3,color="red")



### pred in test ###


test = cbind(test,predtree)


test$predtree=as.factor(test$predtree)
VitaminD = test$predtree[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = AGE, y = WAIST, colour = Gender, shape = VitaminD), size =3) +
  scale_color_brewer(palette = 'Dark2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(60,140,10),limits=c(60,140)) +xlab("Age")+ylab("Waist")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =107, colour = "red", lty =2)+annotate("text",x=17,y=105,label="Waist=107",size=3,color="red")+
  geom_vline(xintercept =37, colour = "red", lty =2)+annotate("text",x=42,y=60,label="Age=37",size=3,color="red")





### merge in test ###

test$predtree = as.character(test$predtree)

test$merge = rep("False Prediction", length(test$predtree))
test$merge[test$vdpgp2=="25(OH)D <20"&test$predtree=="25(OH)D <20"]="True <20"
test$merge[test$vdpgp2=="25(OH)D >=20"&test$predtree=="25(OH)D >=20"]="True >=20"


test$merge=as.factor(test$merge)
VitaminD = test$merge[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = AGE, y = WAIST, colour = Gender, shape = VitaminD), size =3) +
  scale_color_brewer(palette = 'Dark2') +
  scale_shape_manual(values = c(4,2,16),labels = unname(TeX(c("False estimated","True < 20", "$True \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(10,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(60,140,10),limits=c(60,140)) +xlab("Age")+ylab("Waist")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =107, colour = "red", lty =2)+annotate("text",x=17,y=105,label="Waist=107",size=3,color="red")+
  geom_vline(xintercept =37, colour = "red", lty =2)+annotate("text",x=42,y=60,label="Age=37",size=3,color="red")+
  annotate("text",x=22,y=140,label="Accuracy: 72.0%",size=3,color="Black")+
  annotate("text",x=22,y=137,label="Precision:   31.0%",size=3,color="Black")+
  annotate("text",x=21.5,y=134,label="Recall:        6.9%",size=3,color="Black")+
  annotate("text",x=22,y=131,label="F1 score:   11.3%",size=3,color="Black")+ guides(shape = guide_legend(reverse=T))















a1_r2 = a1[a1$RACEGP==2,]
a1_r2 = a1_r2[,-29]



set.seed(1234)


select<-sample(1:nrow(a1_r2),length(a1_r2$vdpgp2)*0.8)
train=a1_r2[select,]
test=a1_r2[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.005)

dtree<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)



tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.01)

dtree_prune<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.75)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp2,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance



### true in test ###


test$SEX = as.character(test$SEX)



test$Gender[test$SEX=="1"]="Female"
test$Gender[test$SEX=="2"]="Male"


test$Gender=as.factor(test$Gender)

test$vdpgp2=as.factor(test$vdpgp2)


VitaminD = test$vdpgp2[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = FOP, y = RBP, colour = Gender, shape = VitaminD), size =3) +
  scale_color_brewer(palette = 'Dark2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(0,20,5),limits=c(0,20)) +
  scale_y_continuous(breaks=seq(40,350,50),limits=c(40,350))+
  xlab("FOP")+ylab("RBP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =230, colour = "red", lty =2)+annotate("text",x=0.5,y=238,label="RBP=230",size=3,color="red")+
  geom_vline(xintercept =4.9, colour = "red", lty =2)+annotate("text",x=3.7,y=40,label="FOP=4.9",size=3,color="red")+
  geom_vline(xintercept =8.2, colour = "red", lty =2)+annotate("text",x=9.5,y=40,label="FOP=8.2",size=3,color="red")


### pred in test ###


test = cbind(test,predtree)


test$predtree=as.factor(test$predtree)
VitaminD = test$predtree[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = FOP, y = RBP, colour = Gender, shape = VitaminD), size =3) +
  scale_color_brewer(palette = 'Dark2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(0,20,5),limits=c(0,20)) +
  scale_y_continuous(breaks=seq(40,350,50),limits=c(40,350)) +xlab("FOP")+ylab("RBP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =230, colour = "red", lty =2)+annotate("text",x=0.5,y=238,label="RBP=230",size=3,color="red")+
  geom_vline(xintercept =4.9, colour = "red", lty =2)+annotate("text",x=3.7,y=40,label="FOP=4.9",size=3,color="red")+
  geom_vline(xintercept =8.2, colour = "red", lty =2)+annotate("text",x=9.5,y=40,label="FOP=8.2",size=3,color="red")




### merge in test ###

test$predtree = as.character(test$predtree)

test$merge = rep("False Prediction", length(test$predtree))
test$merge[test$vdpgp2=="25(OH)D <20"&test$predtree=="25(OH)D <20"]="True <20"
test$merge[test$vdpgp2=="25(OH)D >=20"&test$predtree=="25(OH)D >=20"]="True >=20"


test$merge=as.factor(test$merge)
VitaminD = test$merge[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = FOP, y = RBP, colour = Gender, shape = VitaminD), size =3) +
  scale_color_brewer(palette = 'Dark2') +
  scale_shape_manual(values = c(4,2,16),labels = unname(TeX(c("False estimated","True < 20", "$True \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(0,20,5),limits=c(0,20)) +
  scale_y_continuous(breaks=seq(40,350,50),limits=c(40,350)) +xlab("FOP")+ylab("RBP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =230, colour = "red", lty =2)+annotate("text",x=0.5,y=238,label="RBP=230",size=3,color="red")+
  geom_vline(xintercept =4.9, colour = "red", lty =2)+annotate("text",x=3.7,y=40,label="FOP=4.9",size=3,color="red")+
  geom_vline(xintercept =8.2, colour = "red", lty =2)+annotate("text",x=9.5,y=40,label="FOP=8.2",size=3,color="red")+
  annotate("text",x=2.5,y=340,label="Accuracy: 69.1%",size=3,color="Black")+
  annotate("text",x=2.5,y=330,label="Precision:   72.8%",size=3,color="Black")+
  annotate("text",x=2.5,y=320,label="Recall:       89.8%",size=3,color="Black")+
  annotate("text",x=2.5,y=310,label="F1 score:   80.4%",size=3,color="Black")+ guides(shape = guide_legend(reverse=T))












a1_r3 = a1[a1$RACEGP==3,]
a1_r3 = a1_r3[,-29]



set.seed(1234)


select<-sample(1:nrow(a1_r3),length(a1_r3$vdpgp2)*0.8)
train=a1_r3[select,]
test=a1_r3[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.005)

dtree<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)



tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.025)

dtree_prune<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.9)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp2,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance








### true in test ###


test$SEX = as.character(test$SEX)



test$Gender[test$SEX=="1"]="Female"
test$Gender[test$SEX=="2"]="Male"


test$Gender=as.factor(test$Gender)

test$vdpgp2=as.factor(test$vdpgp2)


VitaminD = test$vdpgp2[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = AGE, y = FOP, colour = Gender, shape = VitaminD), size =3) +
  scale_color_brewer(palette = 'Dark2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(15,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(0,15,5),limits=c(0,15))+
  xlab("Age")+ylab("FOP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=16,y=8.5,label="FOP=8.9",size=3,color="red")




### pred in test ###


test = cbind(test,predtree)


test$predtree=as.factor(test$predtree)
VitaminD = test$predtree[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = AGE, y = FOP, colour = Gender, shape = VitaminD), size =3) +
  scale_color_brewer(palette = 'Dark2') +
  scale_shape_manual(values = c(2,16),labels = unname(TeX(c("25(OH)D < 20", "$25(OH)D \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(15,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(0,15,5),limits=c(0,15)) +xlab("Age")+ylab("FOP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=16,y=8.5,label="FOP=8.9",size=3,color="red")




### merge in test ###

test$predtree = as.character(test$predtree)

test$merge = rep("False Prediction", length(test$predtree))
test$merge[test$vdpgp2=="25(OH)D <20"&test$predtree=="25(OH)D <20"]="True <20"
test$merge[test$vdpgp2=="25(OH)D >=20"&test$predtree=="25(OH)D >=20"]="True >=20"


test$merge=as.factor(test$merge)
VitaminD = test$merge[1:250]

ggplot()+ geom_point(data=test[1:250,],aes(x = AGE, y = FOP, colour = Gender, shape = VitaminD), size =3) +
  scale_color_brewer(palette = 'Dark2') +
  scale_shape_manual(values = c(4,2,16),labels = unname(TeX(c("False estimated","True < 20", "$True \\geq 20$")))     )+
  scale_x_continuous(breaks=seq(15,90,10),limits=c(15,90)) +
  scale_y_continuous(breaks=seq(0,15,5),limits=c(0,15)) +xlab("Age")+ylab("FOP")+
  theme(axis.title.x = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =0))+
  theme(axis.title.y = element_text(size = 13, color = "black", vjust = 0.5, hjust = 0.5, angle =90))+
  geom_hline(yintercept =8.9, colour = "red", lty =2)+annotate("text",x=16,y=8.5,label="FOP=8.9",size=3,color="red")+
  annotate("text",x=22,y=15,label="Accuracy: 63.3%",size=3,color="Black")+
  annotate("text",x=22,y=14.5,label="Precision:   60.2%",size=3,color="Black")+
  annotate("text",x=22,y=14,label="Recall:       60.0%",size=3,color="Black")+
  annotate("text",x=22,y=13.5,label="F1 score:   60.1%",size=3,color="Black")+ guides(shape = guide_legend(reverse=T))
























### Split gender for 25(OH)D >=20 vs 25(OH)D <20 ###


a1_1 = a1[a1$SEX==1,]
a1_1 = a1_1[,-28]




set.seed(1234)


select<-sample(1:nrow(a1_1),length(a1_1$vdpgp2)*0.8)
train=a1_1[select,]
test=a1_1[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.005)

dtree<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.06)

dtree_prune<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=1.0)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp2,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance









a1_2 = a1[a1$SEX==2,]
a1_2 = a1_2[,-28]


set.seed(1234)


select<-sample(1:nrow(a1_2),length(a1_2$vdpgp2)*0.8)
train=a1_2[select,]
test=a1_2[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.003)

dtree<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.01)

dtree_prune<-rpart(vdpgp2~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=1.0)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp2,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance








### Split Age for 25(OH)D >=30 vs 25(OH)D <30 ###




a5_1 = a5[a5$SEX==1,]
a5_1 = a5_1[,-28]



set.seed(1234)


select<-sample(1:nrow(a5_1),length(a5_1$vdpgp2_2)*0.8)
train=a5_1[select,]
test=a5_1[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.001)

dtree<-rpart(vdpgp2_2~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.01)

dtree_prune<-rpart(vdpgp2_2~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.8)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp2,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance







a5_2 = a5[a5$SEX==2,]
a5_2 = a5_2[,-28]


set.seed(1234)


select<-sample(1:nrow(a5_2),length(a5_2$vdpgp2_2)*0.8)
train=a5_2[select,]
test=a5_2[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.001)

dtree<-rpart(vdpgp2_2~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.018)

dtree_prune<-rpart(vdpgp2_2~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.8)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp2,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance




### Split Age for 25(OH)D >=30 vs 25(OH)D 20-29 vs 25(OH)D <30 ###

a2_1 = a2[a2$SEX==1,]
a2_1 = a2_1[,-28]



set.seed(1234)


select<-sample(1:nrow(a2_1),length(a2_1$vdpgp3)*0.8)
train=a2_1[select,]
test=a2_1[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.003)

dtree<-rpart(vdpgp3~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.03)

dtree_prune<-rpart(vdpgp3~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=104, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=1)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp3,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2]+acctable[3,3])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]+acctable[3,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]+acctable[1,3]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]+acctable[3,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]+acctable[1,3]))
2*(r*p)/(r+p)


dtree$variable.importance







a2_2 = a2[a2$SEX==2,]
a2_2 = a2_2[,-28]



set.seed(1234)


select<-sample(1:nrow(a2_2),length(a2_2$vdpgp3)*0.8)
train=a2_2[select,]
test=a2_2[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.003)

dtree<-rpart(vdpgp3~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.01)

dtree_prune<-rpart(vdpgp3~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=104, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.7)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$vdpgp3,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2]+acctable[3,3])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]+acctable[3,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]+acctable[1,3]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]+acctable[3,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]+acctable[1,3]))
2*(r*p)/(r+p)


dtree$variable.importance









### Split Age for MetSyn = 0 Vs MetSyn =1  ###



a3_1 = a3[a3$SEX==1,]
a3_1 = a3_1[,-28]



set.seed(1234)


select<-sample(1:nrow(a3_1),length(a3_1$MetSyn)*0.8)
train=a3_1[select,]
test=a3_1[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.0001)

dtree<-rpart(MetSyn~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.005)

dtree_prune<-rpart(MetSyn~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.7,varlen=4)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$MetSyn,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance








a3_2 = a3[a3$SEX==2,]
a3_2 = a3_2[,-28]



set.seed(1234)


select<-sample(1:nrow(a3_2),length(a3_2$MetSyn)*0.8)
train=a3_2[select,]
test=a3_2[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.00001)

dtree<-rpart(MetSyn~.,data=train, method="class", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.001)

dtree_prune<-rpart(MetSyn~.,data=train, method="class", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra=106, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=0.7,varlen=4)

predtree<-predict(dtree_prune,newdata=test,type="class")  

acctable=table(test$MetSyn,predtree,dnn=c("true","pred")) 
(acctable[1,1]+acctable[2,2])/(sum(acctable))
(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
p=(acctable[1,1]/(acctable[1,1]+acctable[2,1]))
r=(acctable[1,1]/(acctable[1,1]+acctable[1,2]))
2*(r*p)/(r+p)


dtree$variable.importance









### Split age for continuous variable ###



a4_1 = a4[a4$SEX==1,]
a4_1 = a4_1[,-28]




set.seed(1234)


select<-sample(1:nrow(a4_1),length(a4_1$vdp_c)*0.8)
train=a4_1[select,]
test=a4_1[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.003)

dtree<-rpart(vdp_c~.,data=train, method="anova", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.006)

dtree_prune<-rpart(vdp_c~.,data=train, method="anova", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra = 100, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=1)


pred<-predict(dtree_prune,test)
residual=data.frame(x=1:length(pred), residual=test$vdp_c-pred)

ggplot(data = residual, mapping = aes(x = x, y = residual)) + geom_point()+xlab("data points in validation set")+
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed")+scale_y_continuous(breaks=seq(-25,50,5),
                                                                                    limits=c(-25,50)) 


dtree$variable.importance





a4_2 = a4[a4$SEX==2,]
a4_2 = a4_2[,-28]




set.seed(1234)


select<-sample(1:nrow(a4_2),length(a4_2$vdp_c)*0.8)
train=a4_2[select,]
test=a4_2[-select,]


tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.003)

dtree<-rpart(vdp_c~.,data=train, method="anova", parms=list(split="gini"), control=tc) 

printcp(dtree)

plotcp(dtree)




tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=7,cp=0.01)

dtree_prune<-rpart(vdp_c~.,data=train, method="anova", parms=list(split="gini"), control=tc) 
rpart.plot(dtree_prune,type=2,branch=1, extra = 100, under=TRUE, faclen=0,clip.facs=FALSE,
           fallen.leaves=T,cex=1)


pred<-predict(dtree_prune,test)
residual=data.frame(x=1:length(pred), residual=test$vdp_c-pred)

ggplot(data = residual, mapping = aes(x = x, y = residual)) + geom_point()+xlab("data points in validation set")+
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed")+scale_y_continuous(breaks=seq(-25,45,5),
                                                                                    limits=c(-25,45)) 


dtree$variable.importance









citation(package = "rpart")
citation()



