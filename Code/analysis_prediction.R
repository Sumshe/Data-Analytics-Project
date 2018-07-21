setwd("D:/SEM_V/DA/Project")
globalterrorismdb_0617dist <- read.csv("GTD_0617dist/globalterrorismdb_0617dist.csv")
globalterrorismdb_0617dist$gno <- as.numeric(as.factor(globalterrorismdb_0617dist$gname))
library(ggplot2)
library(RColorBrewer)
library(plotly)
packageVersion('plotly')
#Creates nice looking color palettes especially for thematic maps
library(reshape2)
#-----------------------------------------------------------------------------------------------------------------
#month wise attacks
mon_analysis <-function(data){
  mon_count <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("month","count"))
  for(mn in c(1:12)){
    dt <- subset(data,data$imonth==mn)
    mon_count <- rbind(mon_count,c(mn,nrow(dt)))
  }
  colnames(mon_count) <- c("month","count")
  mon_count$mon <- c("Jan","Feb","Mar","April","May","June","July","Aug","Sep","Oct","Novem","Dec")
  p <- ggplot(data=mon_count,aes(x=mon_count$mon, y=mon_count$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Month", y="Number of Attacks") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Month Wise Attacks")
  return(mon_count)
}
#------------------------------------------------------------------------------------------------------------------------------------
#weapon type wise attacks
weap_analysis <- function(data){
  weap_count <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("weap","count"))
  n <- c()
  for(mn in c(1:length(unique(data$weaptype1)))){
    dt <- subset(data,data$weaptype1==mn)
    weap_count <- rbind(weap_count,c(mn,nrow(dt)))
    n <- c(n,as.character(dt[1,"weaptype1_txt"]))
  }
  colnames(weap_count) <- c("weap","count")
  weap_count$name <- n
  weap_count <- weap_count[rev(order(weap_count$count)),]
  weap_count <- head(weap_count)
  ggplot(data=weap_count,aes(x=weap_count$name, y=weap_count$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Weapon", y="Number of Attacks") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=weap_count$count),vjust=-1) +
  ggtitle("Weapon Type")
  return(weap_count)
}
#------------------------------------------------------------------------------------------------------------------------------------
overall <- melt(weap_count, id.var="iyear")
ggplot(overall, aes(x = iyear, y = count, fill = variable)) +
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Stacked_bar_plot_analysis (Weapons Used Year Wise)")+ coord_flip()
#------------------------------------------------------------------------------------------------------------------------------------
#group responsible
group_analysis <- function(data){
  gname_count <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("gname","count"))
  ls <- unique(data$gno)
  n<-c()
  for(mn in c(1:length(ls))){
    dt <- subset(data,data$gno==mn)
    gname_count <- rbind(gname_count,c(mn,nrow(dt)))
    n<-c(n,as.character(dt[1,"gname"]))
  }
  colnames(gname_count) <- c("gno","count")
  gname_count$grp_name <- n
  ggplot(data=gname_count,aes(x=gname_count$grp_name, y=gname_count$count, width=.8)) +
    geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Groups", y="Number of Attacks") +
    theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    ggtitle("Group responsible")
  return(gname_count)
}
#------------------------------------------------------------------------------------------------------------------------------------
#Type of Attack
attack_analysis <-function(data){
  atk_count <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("atk","count"))
  n <- c()
  for(mn in c(1:length(unique(data$attacktype1)))){
    dt <- subset(data,data$attacktype1==mn)
    atk_count <- rbind(atk_count,c(mn,nrow(dt)))
    n <- c(n,as.character(dt[1,"attacktype1_txt"]))
  }
    colnames(atk_count) <- c("atk","count")
    atk_count$atk_name <- n
    atk_count <- atk_count[rev(order(atk_count$count)),]
    ggplot(data=atk_count,aes(x=atk_count$atk_name, y=atk_count$count, width=.8)) +
    geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Type of Attack", y="Number of Attacks") +
    theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_text(aes(label=atk_count$count),vjust=-1) +
    ggtitle("Attack Type")
  
  return(atk_count)
}
#------------------------------------------------------------------------------------------------------------------------------------
#Target of Attack
target_analysis <- function(data){
  tar_count <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("tar","count"))
  n <- c()
  for(mn in c(1:length(unique(data$targtype1)))){
    dt <- subset(data,data$targtype1==mn)
    tar_count <- rbind(tar_count,c(mn,nrow(dt)))
    n <- c(n,as.character(dt[1,"targtype1_txt"]))
  }
  colnames(tar_count) <- c("tar","count")
  tar_count$tar_name <- n
  tar_count <- tar_count[rev(order(tar_count$count)),]
  ggplot(data=tar_count,aes(x=tar_count$tar_name, y=tar_count$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Target", y="Number of Attacks") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=tar_count$count),vjust=-1) +
  ggtitle("Target wise Attacks")
  return(tar_count)
}
#------------------------------------------------------------------------------------------------------------------------------------
# suicide ? (more of a prediction and correlation problem)
suicide <- subset(globalterrorismdb_0617dist,globalterrorismdb_0617dist$suicide==1)
atk_count <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("atk","count"))
n <- c()
for(mn in c(1:9)){
  dt <- subset(suicide,attacktype1==mn)
  atk_count <- rbind(atk_count,c(mn,nrow(dt)))
  n <- c(n,as.character(dt[1,"attacktype1_txt"]))
}
colnames(atk_count) <- c("atk","count")
atk_count$atk_name <- n
atk_count <- atk_count[rev(order(atk_count$count)),]
ggplot(data=atk_count,aes(x=atk_count$atk_name, y=atk_count$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Type of Attack", y="Number of Attacks") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=atk_count$count),vjust=-1) +
  ggtitle("Type of Attack - Suicide occurred")

#------------------------------------------------------------------------------------------------------------------------------------
use <- weap_analysis(dat_1970)
ggplot(data=use,aes(x=use$name, y=use$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Weapon type", y="number of attacks") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=use$count),vjust=-1) +
  ggtitle("weapon analysis in 1970-1980")
use <- weap_analysis(dat_1980)
ggplot(data=use,aes(x=use$name, y=use$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Weapon type", y="number of attacks") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=use$count),vjust=-1) +
  ggtitle("weapon analysis in 1980-1990")
use <- weap_analysis(dat_1990)
ggplot(data=use,aes(x=use$name, y=use$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Weapon type", y="number of attacks") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=use$count),vjust=-1) +
  ggtitle("weapon analysis in 1990-2000")
use <- weap_analysis(dat_2000)
ggplot(data=use,aes(x=use$name, y=use$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Weapon type", y="number of attacks") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=use$count),vjust=-1) +
  ggtitle("weapon analysis in 2000-2010")
use <- weap_analysis(dat_2010)
ggplot(data=use,aes(x=use$name, y=use$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Weapon type", y="number of attacks") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=use$count),vjust=-1) +
  ggtitle("weapon analysis in 2010-2016")
#------------------------------------------------------------------------------------------------------------------------------------
#TOP 20 Bomb Prone Countries
bomb <- subset(globalterrorismdb_0617dist,globalterrorismdb_0617dist$attacktype1==3)
country_count <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("country","count"))
n <- c()
for(mn in c(1:1004)){
  dt <- subset(bomb,country==mn)
  country_count <- rbind(country_count,c(mn,nrow(dt)))
  n <- c(n,as.character(dt[1,"country_txt"]))
}
colnames(country_count) <- c("country","count")
country_count$country_name <- n
country_count <- country_count[rev(order(country_count$count)),]
country_count <- head(country_count,20)
ggplot(data=country_count,aes(x=country_count$country_name, y=country_count$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Country", y="Number of Bomb-Blasts") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=country_count$count),vjust=-1) +
  ggtitle("TOP 20 Bomb-Blast prone countries")
#------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------
# clubbing all the years into groups of 10 (1970-1980 , 1980-1990 , 1990-2000, and so on..)
# stored as dat_<lower_limit>
vv <- c()
dat_1970 <- data.frame(subset(globalterrorismdb_0617dist,globalterrorismdb_0617dist$iyear <=1980 ))

vv <- c(vv,nrow(dat_1970))

dat_1980 <- data.frame(subset(globalterrorismdb_0617dist,globalterrorismdb_0617dist$iyear <=1990))
dat_1980 <- data.frame(subset(dat_1980,dat_1980$iyear >1980))

vv <- c(vv,nrow(dat_1980))

dat_1990 <- data.frame(subset(globalterrorismdb_0617dist,globalterrorismdb_0617dist$iyear <=2000))
dat_1990 <- data.frame(subset(dat_1990,dat_1990$iyear >1990))

vv <- c(vv,nrow(dat_1990))

dat_2000 <- data.frame(subset(globalterrorismdb_0617dist,globalterrorismdb_0617dist$iyear <= 2010))
dat_2000 <- data.frame(subset(dat_2000,dat_2000$iyear >2000))

vv <- c(vv,nrow(dat_2000))

dat_2010 <- data.frame(subset(globalterrorismdb_0617dist,globalterrorismdb_0617dist$iyear <= 2020))
dat_2010 <- data.frame(subset(dat_2010,dat_2010$iyear >2010))

vv <- c(vv,nrow(dat_2010))
#------------------------------------------------------------------------------------------------------------------------------------
# decade wise Number of Attacks
yr<-c("1970-1980","1980-1990","1990-2000","2000-2010","2010-2016")
new <- data.frame(year=yr,count=vv)
ggplot(data=new,aes(x=new$year, y=new$count, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Years", y="Number of Attacks") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=new$count),vjust=-1) +
  ggtitle("Decade Wise Attacks")
#------------------------------------------------------------------------------------------------------------------------------------
# Stacked Bar Plot Analysis - Target - based
# we take the percentages - relative comparison
new_1 <- data.frame(year=yr)
for(i in c(1:22)){
  li <- c()
  li<-c(li,nrow(subset(dat_1970,dat_1970$targtype1==i))/nrow(dat_1970)*100)
  li<-c(li,nrow(subset(dat_1980,dat_1980$targtype1==i))/nrow(dat_1980)*100)
  li<-c(li,nrow(subset(dat_1990,dat_1990$targtype1==i))/nrow(dat_1990)*100)
  li<-c(li,nrow(subset(dat_2000,dat_2000$targtype1==i))/nrow(dat_2000)*100)
  li<-c(li,nrow(subset(dat_2010,dat_2010$targtype1==i))/nrow(dat_2010)*100)
  new_1<-cbind(new_1,li)
}
colnames(new_1) <- c(0:22)
tar_count <- tar_count[order(tar_count$count),]
colnames(new_1) <- c("year",tar_count$tar_name)
new_overall <- melt(new_1,id.var = "year")
ggplot(new_overall, aes(x = year, y = value, fill = variable)) +
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Stacked_bar_plot_analysis (year-target change)")+ coord_flip()
#------------------------------------------------------------------------------------------------------------------------------------
# Stacked Bar Plot Analysis - Weapon of choice - based
new_2 <- data.frame(year=yr)
# these numbers are collected from above plot - (weaptype -> num of attacks)
for(i in c(2,5,6,8,9,13)){
  li <- c()
  li<-c(li,nrow(subset(dat_1970,dat_1970$weaptype1==i))/nrow(dat_1970)*100)
  li<-c(li,nrow(subset(dat_1980,dat_1980$weaptype1==i))/nrow(dat_1980)*100)
  li<-c(li,nrow(subset(dat_1990,dat_1990$weaptype1==i))/nrow(dat_1990)*100)
  li<-c(li,nrow(subset(dat_2000,dat_2000$weaptype1==i))/nrow(dat_2000)*100)
  li<-c(li,nrow(subset(dat_2010,dat_2010$weaptype1==i))/nrow(dat_2010)*100)
  new_2<-cbind(new_2,li)
}
colnames(new_2) <- c(0:6)
weap_count <- weap_count[order(weap_count$count),]
colnames(new_2) <- c("year",weap_count$name)
new2_overall <- melt(new_2,id.var = "year")
ggplot(new2_overall, aes(x = year, y = value, fill = variable)) +
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Stacked_bar_plot_analysis (year-Weapon change)")+ coord_flip()
#------------------------------------------------------------------------------------------------------------------------------------
#Most Destructive Weapon based on(nkill,nwound,property,propextent,propvalue-USD)
w <- subset(globalterrorismdb_0617dist,globalterrorismdb_0617dist$property==1)
w <- subset(w,w$propvalue>=0)
n <- c()
sam <- subset(w,select = c(weaptype1,weaptype1_txt,propvalue))
weap_propvalue <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("weapon","sum of damaged property value"))
for(i in c(1:13)){
  weap <- data.frame(subset(sam,sam$weaptype1==i))
  weap_propvalue <- rbind(weap_propvalue,c(i,sum(weap$propvalue)))
  n <- c(n,as.character(weap[1,"weaptype1_txt"]))
}
colnames(weap_propvalue) <- c("no","sum")
weap_propvalue$weapon <- n
weap_propvalue <- weap_propvalue[rev(order(weap_propvalue$sum)),]
weap_propvalue <- head(weap_propvalue,4)

p <- weap_propvalue %>%
  plot_ly(labels = weap_propvalue$weapon, values = weap_propvalue$sum) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Property Damage",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p
#------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------
grps <- subset(globalterrorismdb_0617dist,select = c(weaptype1,weaptype1_txt) )
grps <- grps[!duplicated(grps), ]
grps <-  grps[order(grps$weaptype1),]
#------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------
# use "data_preprocess.r" before going down :
#------------------------------------------------------------------------------------------------------------------------------------
# PCA
new_frame <- non_char
new_frame$propextent=NULL
prin_comp <- prcomp(new_frame)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
# this graph becomes flat at 60 , so we take 10 components [PC1 - PC10]
final_gtd <- data.frame(prin_comp$x[,1:10])
final_gtd$propextent <- non_char$propextent
#------------------------------------------------------------------------------------------------------------------------------------
# split into test and train :
require(caTools)
set.seed(101)
library(e1071)
sample = sample.split(final_gtd$PC1, SplitRatio = .70)
train = subset(final_gtd, sample == TRUE)
test  = subset(final_gtd, sample == FALSE)
#------------------------------------------------------------------------------------------------------------------------------------
# estimating the propery damage extent : 
# (i)Naive Bayes :
test_nb <- test
library(caret)
library(e1071)
nb_model <- naiveBayes(propextent ~ ., data = train)
test_nb <- cbind(test_nb,nb_cls = predict(nb_model,test,type = "raw"))
test_nb[, "nb"] <- NULL
for(i in c(1:nrow(test_nb))){
  max <- max(c(test_nb[i,12],test_nb[i,13],test_nb[i,14],test_nb[i,15],test_nb[i,16]))
  if(max==test_nb[i,12]){
    test_nb[i,"nb"]=0
  }
  else if(max==test_nb[i,13]){
    test_nb[i,"nb"]=1
  }
  else if(max==test_nb[i,14]){
    test_nb[i,"nb"]=2
  }
  else if(max==test_nb[i,15]){
    test_nb[i,"nb"]=3
  }
  else if(max==test_nb[i,16]){
    test_nb[i,"nb"]=4
  }
}
test_nb$nb_cls.0 <- NULL
test_nb$nb_cls.1 <- NULL
test_nb$nb_cls.2 <- NULL
test_nb$nb_cls.3 <- NULL
test_nb$nb_cls.4 <- NULL
conf_nb <- confusionMatrix(table(test_nb$nb,test_nb$propextent))
cat("The Accuracy of Naive Bayes Classifier is ",conf_nb$overall['Accuracy'],"\n")
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# (ii)kNN :
test_knn <- test
library(class)
test_knn$knn_10 <- knn(train,test_knn,train$propextent,k=7)
conf_knn <- confusionMatrix(table(test_knn$knn_10,test_knn$propextent))
cat("The Accuracy of kNN Classifier is ",conf_knn$overall['Accuracy'],"\n")
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# (iii)SVM :
test_svm <- test
library(e1071)
svm_model <- svm(propextent~.,data=train,kernel="polynomial",type="C-classification")
test_svm$svm <- predict(svm_model,test_svm)
conf_svm <- confusionMatrix(table(test_svm$svm,test_svm$is_patient))
cat("The Accuracy of SVM Classifier is ",conf_svm$overall['Accuracy'],"\n")
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# logistic regression model : Hostage situation
library(ModelGood)
library(lattice)
library(caret)
library(plotrix)
library(ROCR)
library(pROC)
safe_host_release <- subset(non_char,non_char$ishostkid == 1)
for(i in c(1:nrow(safe_host_release))){
  var <- safe_host_release[i,"hostkidoutcome"]
  if(var==1){
    safe_host_release[i,"hostkidoutcome"] <- 0
  }
  else if(var==2){
    safe_host_release[i,"hostkidoutcome"] <- 1
  }
  else if(var==3){
    safe_host_release[i,"hostkidoutcome"] <- 1
  }
  else if(var==4){
    safe_host_release[i,"hostkidoutcome"] <- 0
  }
  else if(var==5){
    safe_host_release[i,"hostkidoutcome"] <- 0
  }
  else if(var==6){
    safe_host_release[i,"hostkidoutcome"] <- 0
  }
  else if(var==7){
    safe_host_release[i,"hostkidoutcome"] <- 0
  }
}
#----------------------------------------------------------------------------------------------------
# PCA part
l <- safe_host_release$hostkidoutcome
safe_host_release$hostkidoutcome = NULL
new_frame <- safe_host_release
new_frame$propextent=NULL
prin_comp <- prcomp(new_frame)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
# this graph becomes flat at 60 , so we take 10 components [PC1 - PC10]
safe_host_release <- data.frame(prin_comp$x[,1:10])
safe_host_release$hostkidoutcome <- l
new <- upSample(safe_host_release,factor(safe_host_release$hostkidoutcome),list=TRUE)
safe_host_release <- new$x
require(caTools)
set.seed(101)
library(e1071)
sample = sample.split(safe_host_release$PC1,SplitRatio = .70)
train_lgm = subset(safe_host_release, sample == TRUE)
test_lgm  = subset(safe_host_release, sample == FALSE)
lg_model <- glm(formula = hostkidoutcome ~ .
                 ,family = binomial("logit"),data=train_lgm)
test_lgm <- cbind(test_lgm,lg_model = round(predict(lg_model,test_lgm,type = "response")))
conf_1 <- confusionMatrix(table(test_lgm$lg_model,test_lgm$hostkidoutcome))
conf_1$overall['Accuracy']
#-------------------------------------------------------------------------------------------------------------------
