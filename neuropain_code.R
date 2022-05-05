library(ggplot2)
library(summarytools)
library(dplyr)
library(tidyverse)
library(Hmisc)

#$\chi$^2^ for chi squared statistic


#data reading
library("readxl")
data=read_excel(path='F:/My Documents/MPH/Biostatistics_projects/Neuropathic_Pain_Data.xlsx', 
                 na=c("-99", "NA"))

#https://en.wikipedia.org/wiki/Data_cleansing

#renaming and formatting
data$from_set<-factor(data$from_set, levels=c(0,1), labels=c("CHOP", "CCHMC"))
#demographic parameters
names(data)[names(data)=='socio02']<-'Age'
names(data)[names(data)=='socio03']<-'Gender'
data$Gender<-as.factor(data$Gender)
data$Gender<-factor(data$Gender, levels=c(1,2), labels=c("Male", "Female"))
label(data$yrs_diagnosed)<-"Time from diagnosis(years)"

#names(data)[names(data)=='socio05']<-'hispanic'
#data$hispanic<-as.factor(data$hispanic)
#names(data)[names(data)=='socio04']<-'Race'
#data$Race<-as.factor(data$Race)
#names(data)[names(data)=='socio07']<-'Education'
#data$Education<-as.factor(data$Education)
#names(data)[names(data)=='SchAtt1']<-'school_attendance'
#data$school_attendance<-as.factor(data$school_attendance)
#names(data)[names(data)=='Demog6']<-'Habitat'
#data$Habitat<-as.factor(data$Habitat)
#names(data)[names(data)=='Demog15']<-'proxy_occupation_status'
#data$proxy_occupation_status<-as.factor(data$proxy_occupation_status)
#names(data)[names(data)=='proxy_socio02']<-'proxy_age'
#names(data)[names(data)=='proxy_socio03']<-'proxy_gender'
#data$proxy_gender<-as.factor(data$proxy_gender)
#data missing completely
#names(data)[names(data)=='proxy_socio05']<-'proxy_race'
#data$proxy_race<-as.factor(data$proxy_race)
#names(data)[names(data)=='proxy_socio06']<-'proxy_relationship'
#data$proxy_relationship<-as.factor(data$proxy_relationship)
#names(data)[names(data)=='proxy_socio07']<-'proxy_education'
#data$proxy_education<-as.factor(data$proxy_education)

#clinical parameters
names(data)[names(data)=='rf1c']<-'Diagnosis'
data$Diagnosis<-as.factor(data$Diagnosis)
data$Diagnosis<-factor(data$Diagnosis, levels=c(1,2), labels=c("Chronic pain condition", "JIA"))

names(data)[names(data)=='rf3c']<-'JIA_type'
#unifying levels 6 and 8 (10 subjects with "undifferentiated" and 2 with "other")
data$JIA_type[data$JIA_type==8]<-6
data$JIA_type<-factor(data$JIA_type, levels=c(1,2,3,4,5,6,7), labels=c("Not applicable", "Systemic", "Psoriatic", "Polyarthiritis", "Oligoarthritis", "Undifferentiated/other", "Enthesis"))
label(data$JIA_type)<-"JIA type"

names(data)[names(data)=='global_peds']<-'Health'
#data$Health<-as.factor(data$Health)
data$Health<-factor(data$Health, levels=c(0,1,2,3,4), labels=c("Excellent", "Very good", "Good", "Fair", "Poor"))
label(data$Health)<-"Global health"

#names(data)[names(data)=='rf2c']<-'ICD9'

#summary(data$ICD9)

#data cleansing: missing values, out-of-range values, nulls, whitespaces, outliers
#incomplete, incorrect, inaccurate or irrelevant,  user entry errors,

#eliminating outliers, looking for out-of-bound values, data consistency
#no age outliers
#dfSummary(data[,3:5], graph.col= FALSE, varnumbers=FALSE, valid.col = FALSE)
#dfSummary(data[,c("hispanic", "Race")], graph.col= FALSE, varnumbers=FALSE, valid.col = FALSE)
#5% hispanic
#summary(data$Race)
#171 missing, 6 out-of-range values. no white participants and 116 blacks-
#this could be due to a data entry error. 
#2.2% latino- does not match the hispanic parameter

#summary(data$yrs_diagnosed)
#hist(data$yrs_diagnosed)
#checking data consistency
#data$Pin[data$yrs_diagnosed>=data$Age]
#15 subjects
#data$Pin[data$yrs_diagnosed>data$Age]
#NA

#summary(data$Diagnosis)
#no SCD, only 2 levels
#summary(data$JIA_type)
#summary(data$Health)

#sample description: age, gender and clinical characteristics for the entire data set
library(table1)
#defining a function to compute p-value
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

table1(~ Gender + Age + yrs_diagnosed +Diagnosis+ JIA_type+ Health
       | from_set, data=data, overall=F, extra.col=list(`P-value`=pvalue))

table1(~ Gender + Age + yrs_diagnosed +Diagnosis+ JIA_type+ Health,
     data=data, overall="Total")

ggplot(data, aes(x=Age))+geom_histogram(binwidth=1)+xlab("Age (years)")+#ylab("number of subjects")+
  ggtitle("Distribution of age")

ggplot(data, aes(x=yrs_diagnosed))+geom_histogram(binwidth=1)+xlab("Time from diagnosis(years)")+#ylab("number of subjects")+
  ggtitle("Distribution of time from diagnosis")

ggplot(data, aes(x=Health))+geom_bar(binwidth=1)+xlab("Pediatric global health")+#ylab("number of subjects")+
  ggtitle("Distribution of global health")

#format ICD9 and rf4c (written description)

#dfSummary(data[,c("Education", "school_attendance", "Habitat", "proxy_occupation_status", "proxy_age")], graph.col= FALSE, varnumbers=FALSE, valid.col = FALSE)

#comorbidities analysis
comorb<-data %>% filter(from_set=="CHOP")#%>% select(6:26)
comorb<-comorb[,c(6:15, 17:26)]
#converting logical columns to numeric
cols <- sapply(comorb, is.logical)
comorb[,cols] <- lapply(comorb[,cols], as.numeric)
comorb[is.na(comorb)] <- 0
#summing comorbidities for each subject
#data$new <- rowSums(data[43:167])
comorb$total<- rowSums(comorb)
t<-table(comorb$total)
knitr::kable(t, col.names=c("Total comorbidities per subject", "Count"), align = "l", 
             caption="Distribution of comorbidites in the CHOP cohort (n=138)" )

#summing cases for each comorbidity
cases<-sort(sapply(comorb, sum))
cases<-data.frame(cases)
knitr::kable(cases, col.names=c("Number of cases"), align = "l", caption="Prevalence of comorbidites in the CHOP cohort (n=138)" )


#creating variables:
#Widespread Pain

#choosing the relevant columns
#back <- data[ , grep("Back", colnames(data))]
#front <- data[ , grep("Front", colnames(data))]
#wdf<-cbind(back, front)

#widespeard pain
data<-data%>% mutate_at(c(201:261), ~replace(., is.na(.), 0))
data$wspain<-rowSums(data[201:261])
#summary(data$wspain)
#hist(data$wspain)
#df<-data.frame(val = unclass(summary(data$wspain)))%>%round(digits=2)
#knitr::kable(df, col.names=c("value"), align = "l", 
#             caption="Widespread pain descriptive statistics" )

#pain behavior
data<-data%>% mutate_at(c(72:93, 131:156), ~replace(., is.na(.), 0))
data$pain_behavior<-rowSums(data[c(72:93, 131:156)])/48
#summary(data$pain_behavior)
#hist(data$pain_behavior)

#pain catastrophizing
data<-data%>% mutate_at(c(157:169), ~replace(., is.na(.), 0))
data$pcs<-rowSums(data[c(157:169)])/13
#summary(data$pcs)
#hist(data$pcs)

#pain intensity1 (range 0 to 10)
data<-data%>% mutate_at(c(68:71), ~replace(., is.na(.), 0))
data$pain_intensity1<-rowSums(data[c(68:71)])/4
#summary(data$pain_intensity1)
#hist(data$pain_intensity1)

#pain intensity2 (range 0 to 4)
data<-data%>% mutate_at(c(170:172), ~replace(., is.na(.), 0))
data$pain_intensity2<-rowSums(data[c(64, 170:172)])/4
#summary(data$pain_intensity2)
#hist(data$pain_intensity2)

#ggplot(data, aes(x=pain_intensity2))+geom_histogram(binwidth=1)#+xlab("Time from diagnosis(years)")+#ylab("number of subjects")+
  #ggtitle("Distribution of time from diagnosis")


#pain quality1 (range 0 to 4)
data<-data%>% mutate_at(c(94:126, 128:130), ~replace(., is.na(.), 0))
data$pain_quality1<-rowSums(data[c(94:126, 128:130)])/36
#summary(data$pain_quality1)
#hist(data$pain_quality1)

#pain quality2 (range 0/1)
data<-data%>% mutate_at(c(173:194), ~replace(., is.na(.), 0))
data$pain_quality2<-rowSums(data[c(127,173:194)])/23
#summary(data$pain_quality2)
#hist(data$pain_quality2)

names(data)[names(data)=='peds_depression_8a_raw']<-'depression'
names(data)[names(data)=='peds_fatigue_10a_raw']<-'fatigue'
names(data)[names(data)=='peds_pain_interference_8a_raw']<-'interference'
data$front_l<-log10(data$Frontpeds)
data$back_l<-log10(data$Backpeds)
label(data$depression)<-'Depression'
label(data$fatigue)<-'Fatigue'
label(data$pain_behavior)<-'Pain behavior'
label(data$pain_intensity1)<-'Pain intensity (0-10)'
label(data$interference)<-'Interference'
label(data$pcs)<-'Pain Catastrophization Scale'
label(data$wspain)<-'Widespread pain'

#-----------------------------1111111111111111111111111

#scatter plots and correlations for continuous variables
library(psych)
pairs.panels(data[,c(3, 5,262,263,264,274:278, 281,282)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             stars=TRUE)
#r>0.7: 
#fatigue- interference, pain beahvior, pcs, depression
#interference-pcs, pain intensity1+2
#pain bahavior-pain intensity

#exploring correlations between ordinal variable health and all possible predictors
#cont- violin plots/box plots 
#binary- chisq
chisq.test(data$Health, data$Gender)
#non significant, small sample of males

chisq.test(data$Health, data$Diagnosis)
#significant

#Age
library(ggpubr)
ggboxplot(subset(data, !is.na(data$Health)), x = "Health", y = "Age",
          color = "Health", add = "jitter", shape = "Health")
#poor health is related to higher ages

#years diagnosed
ggboxplot(subset(data, !is.na(data$Health)), x = "Health", y = "yrs_diagnosed",
          color = "Health", add = "jitter", shape = "Health")
#counter intuitive. 
#same avg time from diagnosis for all health categories, except for excellent- 
#higher avg
#poor health is related to either long time or short time from diagnosis




#ggviolin(subset(data, !is.na(data$Health)), x = "Health", y = "yrs_diagnosed", fill = "Health",
#         add = "boxplot", add.params = list(fill = "white"))

ggboxplot(subset(data, !is.na(data$Health)), x = "Health", y = "depression",
          color = "Health", add = "jitter", shape = "Health")
#direct correlation, long right tails in each health category


ggboxplot(subset(data, !is.na(data$Health)), x = "Health", y = "fatigue",
          color = "Health", add = "jitter", shape = "Health")
#stronger correlation

ggboxplot(subset(data, !is.na(data$Health)), x = "Health", y = "interference",
          color = "Health", add = "jitter", shape = "Health")
#correlated
ggboxplot(subset(data, !is.na(data$Health)), x = "Health", y = "wspain",
          color = "Health", add = "jitter", shape = "Health")
#weak correlation

ggboxplot(subset(data, !is.na(data$Health)), x = "Health", y = "pain_behavior",
          color = "Health", add = "jitter", shape = "Health")
#direct proportional linear correlation

ggboxplot(subset(data, !is.na(data$Health)), x = "Health", y = "pcs",
          color = "Health", add = "jitter", shape = "Health")
#same

ggboxplot(subset(data, !is.na(data$Health)), x = "Health", y = "pain_intensity1",
          color = "Health", add = "jitter", shape = "Health")
#same

ggboxplot(subset(data, !is.na(data$Health)), x = "Health", y = "pain_intensity2",
          color = "Health", add = "jitter", shape = "Health")
#same

###!!!pain behavior, fatigue, depression, pain intensity1, age,interference 
#checking collinearity
library(psych)
pairs.panels(data[,c("Age", "depression", "fatigue", "pain_behavior", "pain_intensity1", "Diagnosis")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             stars=TRUE)
#depression+pain behavior, fatigue+pain intensity

#ordinal logistic regression
require(MASS)
require(Hmisc)
library(performance) 
library(interactions)

#depression and behavior
#m1 <- polr(Health ~ depression+pain_behavior+Age+pain_quality1  , data, Hess=TRUE)
#summary(m1)
#residual deviance: 816.8
#AIC=831

m1 <- polr(Health ~ depression+ fatigue+pain_behavior+ Age+interference+pain_intensity1, data, Hess=TRUE)
summary(m1)
#AIC=817

#this models output does not have p value. the intercepts of levels (cutpoints) are 
#evenly distanced, and the proportional odds assumption is met.
#extracting the coefficients from the summary
(ctable <- coef(summary(m1)))
#manual calculation of the p-value, 2-sided
p <- round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2, digits = 3)
model_performance(m1)
library(brant)
#Brant test checks if there is a proportional transfer between levels,
#in an ordinal outcome variable in a logistic regression model (=ordered logit regression)
#Brant test: H0- proportionality exists
brant(m1)
vif(m1)

library(caret)
set.seed(3453)
training.samples <- data$Health %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

#model<-polr(Health ~ depression+ pain_behavior+ Age  , data=train.data, Hess=TRUE)
model<-polr(Health ~ depression+ fatigue+pain_behavior+ Age+interference+pain_intensity1,
            data=train.data, Hess=TRUE)
test.data$probablilities <- predict(model,newdata=test.data, type='probs', allow.new.levels = TRUE)
test.data$results<-colnames(test.data$probablilities)[max.col(test.data$probablilities)]
#checking accuracy
mean(test.data$Health == test.data$results)

library(sure)
autoplot.polr(model, what = "qq")
autoplot.polr(model, what = "fitted")
p2qq<-autoplot.polr(model, what = "qq")+ggtitle("Model 2 Q-Q plot")
autoplot.polr(model, what = "fitted")


#fatigue and pain intensity
m2 <- polr(Health ~ fatigue + pain_intensity1+ Age  , data, Hess=TRUE)
summary(m2)
model_performance(m2)
brant(m2)
#residual deviance: 798.5
#AIC=812.5
library(car)
car::vif(m2)

#this models output does not have p value. the intercepts of levels (cutpoints) are 
#evenly distanced, and the proportional odds assumption is met.
#extracting the coefficients from the summary
(ctable <- coef(summary(m2)))
#manual calculation of the p-value, 2-sided
p <- round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2, digits = 3)
(ctable <- cbind(ctable, "p value" = p))
#calculating confidence intervals
(ci <- confint(m))
#calculating odds ratio
exp(coef(m))
#combining odds ratio and CI for the odds ratio
exp(cbind(OR = coef(m), ci))


#testing model's accuracy with a test and training set
library(caret)
set.seed(322)
training.samples <- data$Health %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

model<-polr(Health ~ fatigue + pain_intensity1+ Age  , data=train.data, Hess=TRUE)
test.data$probablilities <- predict(model,newdata=test.data, type='probs', allow.new.levels = TRUE)
test.data$results<-colnames(test.data$probablilities)[max.col(test.data$probablilities)]
#checking accuracy
mean(test.data$Health == test.data$results)
#32-44% accurate


library(caret)
#model 1
acc<-c()
for (i in 1:1000){
  set.seed(22+i*3)
  training.samples <- data$Health %>% createDataPartition(p = 0.8, list = FALSE)
  train.data  <- data[training.samples, ]
  test.data <- data[-training.samples, ]
  model<-polr(Health ~ depression+ fatigue+pain_behavior+ Age+interference+pain_intensity1,
              data=train.data, Hess=TRUE)
  test.data$probablilities <- predict(model,newdata=test.data, type='probs', allow.new.levels = TRUE)
  test.data$results<-colnames(test.data$probablilities)[max.col(test.data$probablilities)]
  #checking accuracy
  acc[i]=mean(test.data$Health == test.data$results)
}
#computing confidence interval for the model's accuracy
error <- qt(0.975,df=length(acc)-1)*sd(acc)/sqrt(length(acc))
lcl<-mean(acc)-error
ucl<-mean(acc)+error
df<-data.frame(val = unclass(summary(acc)))%>%round(digits=2)
knitr::kable(df, col.names=c("value"), align = "l", caption="Model 1 accuracy statistics" )

#confusion matrix for model predictions
set.seed(232)
training.samples <- data$Health %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
model<-polr(Health ~ depression+ fatigue+pain_behavior+ Age+interference+pain_intensity1,
            data=train.data, Hess=TRUE)
test.data$probablilities <- predict(model,newdata=test.data, type='probs', allow.new.levels = TRUE)
test.data$results<-colnames(test.data$probablilities)[max.col(test.data$probablilities)]
confusionMatrix(as.factor(test.data$results), test.data$Health)

#model 2
acc<-c()
for (i in 1:1000){
  set.seed(24+i*3)
  training.samples <- data$Health %>% createDataPartition(p = 0.8, list = FALSE)
  train.data  <- data[training.samples, ]
  test.data <- data[-training.samples, ]
  model<-polr(Health ~ fatigue + pain_intensity1+ Age  , data=train.data, Hess=TRUE)
  test.data$probablilities <- predict(model,newdata=test.data, type='probs', allow.new.levels = TRUE)
  test.data$results<-colnames(test.data$probablilities)[max.col(test.data$probablilities)]
  #checking accuracy
  acc[i]=mean(test.data$Health == test.data$results)
}
#summary(acc)
error <- qt(0.975,df=length(acc)-1)*sd(acc)/sqrt(length(acc))
lcl<-mean(acc)-error
ucl<-mean(acc)+error
df<-data.frame(val = unclass(summary(acc)))%>%round(digits=2)
knitr::kable(df, col.names=c("value"), align = "l", caption="Model 2 accuracy statistics" )

m<-c("Model 1","Model 2" )
min<-c("0.220", "0.203")
avg<-c("0.386","0.383")
max<-c("0.542", "0.559")
lci<-c("0.379", "0.380")
uci<-c("0.386", "0.387")
df<-cbind(m,min, avg, max, lci, uci)
knitr::kable(df, col.names=c("", "Min.", "Average", "Max.", "2.5% CI", "97.5% CI"), 
             align = "l", caption="Prediction Models' accuracy statistics" )



###1. using sure package to perform diagnostics for model
###2. converting health into a numeric variable, and creating a linear regression 
#prediction model

###3. using LDA linear discriminant analysis, for linear relationships only
#https://www.r-bloggers.com/2021/05/linear-discriminant-analysis-in-r/
#http://www.sthda.com/english/articles/36-classification-methods-essentials/143-evaluation-of-classification-model-accuracy-essentials/
library(MASS)
set.seed(322)
training.samples <- data$Health %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

model<-lda(Health ~ fatigue + pain_intensity1+ Age  , data=train.data, Hess=TRUE)
model<-lda(Health ~ fatigue , data=train.data, Hess=TRUE)
model<-lda(Health ~ depression+ fatigue+pain_behavior+ Age+interference+pain_intensity1+ 
             pcs+wspain, data=train.data, Hess=TRUE)

# Make predictions on the test data
predictions <- model %>% predict(test.data)
# Model accuracy
confusionMatrix(predictions$class, test.data$Health)



ggplot(data, aes(x=pain_intensity1, y=wspain, color=Health))+geom_smooth(method = 'loess',span =2, na.rm=TRUE)
#the better the health, the weaker the correlation between pain intensity and wspain

ggplot(data, aes(x=pain_intensity1, y=fatigue, color=Health))+geom_smooth(method = 'loess',span =2, na.rm=TRUE)

ggplot(data, aes(x=depression, y=pain_behavior, color=Health))+geom_smooth(method = 'loess',span =2, na.rm=TRUE)

ggplot(data, aes(x=fatigue, y=depression, color=Health))+geom_smooth(method = 'loess',span =2, na.rm=TRUE)


#---------------------222222222222------------------------
table1(~ Gender + Age + yrs_diagnosed +depression+ fatigue+
         pain_behavior+ pain_intensity1+ interference+ pcs+wspain
       | Diagnosis, data=data, overall=F, extra.col=list(`P-value`=pvalue))

library(ggpubr)
table(data$Diagnosis, data$Gender)
chisq.test(data$Diagnosis, data$Gender)
#significant, females are nore likely to have chronic pain
t.test(data$Age[data$Diagnosis=="Chronic pain condition"], data$Age[data$Diagnosis=="JIA"])
ggboxplot(data, x = "Diagnosis", y = "Age",
          color = "Diagnosis", add = "jitter", shape = "Diagnosis")
#significant, JIA patients are younger on average

t.test(data$yrs_diagnosed[data$Diagnosis=="Chronic pain condition"], data$yrs_diagnosed[data$Diagnosis=="JIA"])
#very significant (means 1.7, 4.8), longer for JIA
ggboxplot(data, x = "Diagnosis", y = "yrs_diagnosed",
          color = "Diagnosis", add = "jitter", shape = "Diagnosis")

t.test(data$depression[data$Diagnosis=="Chronic pain condition"], data$depression[data$Diagnosis=="JIA"])
ggboxplot(data, x = "Diagnosis", y = "depression",
          color = "Diagnosis", add = "jitter", shape = "Diagnosis")
#very significant, much higher for chronic pain

t.test(data$fatigue[data$Diagnosis=="Chronic pain condition"], data$fatigue[data$Diagnosis=="JIA"])
ggboxplot(data, x = "Diagnosis", y = "fatigue",
          color = "Diagnosis", add = "jitter", shape = "Diagnosis")
#significant, higher for chronic pain

t.test(data$interference[data$Diagnosis=="Chronic pain condition"], data$interference[data$Diagnosis=="JIA"])
ggboxplot(data, x = "Diagnosis", y = "interference",
          color = "Diagnosis", add = "jitter", shape = "Diagnosis")
#very significant, higher in chronic pain

t.test(data$wspain[data$Diagnosis=="Chronic pain condition"], data$wspain[data$Diagnosis=="JIA"])
ggboxplot(data, x = "Diagnosis", y = "wspain",
          color = "Diagnosis", add = "jitter", shape = "Diagnosis")
#very significant, higher in chronic pain

t.test(data$pain_behavior[data$Diagnosis=="Chronic pain condition"], data$pain_behavior[data$Diagnosis=="JIA"])
ggboxplot(data, x = "Diagnosis", y = "pain_behavior",
          color = "Diagnosis", add = "jitter", shape = "Diagnosis")
#significant, higher in chronic pain

t.test(data$pcs[data$Diagnosis=="Chronic pain condition"], data$pcs[data$Diagnosis=="JIA"])
ggboxplot(data, x = "Diagnosis", y = "pcs",
          color = "Diagnosis", add = "jitter", shape = "Diagnosis")
#significant, average higher in chronic pain

t.test(data$pain_intensity1[data$Diagnosis=="Chronic pain condition"], data$pain_intensity1[data$Diagnosis=="JIA"])
ggboxplot(data, x = "Diagnosis", y = "pain_intensity1",
          color = "Diagnosis", add = "jitter", shape = "Diagnosis")
#significant, higher in chronic pain

table(data$Diagnosis, data$Health)
chisq.test(data$Diagnosis, data$Health)
#significant

#statistically significant predictors of diagnosis: age, gender, depression, fatigue, 
#interference, widespread pain, pain behavior, pain intensity and pcs.

#checking collinearity
library(psych)
pairs.panels(data[,c("Age", "depression", "fatigue", "wspain", 
                     "pain_intensity1", "pain_quality1")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             stars=TRUE)

#model<-glm(Diagnosis~Age+Gender+depression+fatigue+wspain+pain_intensity1, data, family=binomial)
#summary(model)

model<-glm(Diagnosis~Age+Gender+depression+wspain, data, family=binomial)
summary(model)
#model_performance()

library(car)
car::vif(model)

library(pROC)
library(caret)
set.seed(143)
training.samples <- data$Diagnosis %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
data$fitted.results <- predict(model,newdata=data, type='response', allow.new.levels = TRUE)
#calculating and plotting ROC and AUC
roc_obj <- roc(data$Diagnosis, data$fitted.results)
plot(roc_obj, print.thres = "best",print.auc=T)
coords(roc_obj, "best", "threshold")
#spe:0.755, sen: 0.844, AUC=0.858
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.483, 1, 0)
test<-as.numeric(test.data$Diagnosis)-1
mean(predicted.classes == test)
#accuracy: 86.9%

#model2

model<-glm(Diagnosis~Age+Gender+depression+wspain+yrs_diagnosed, data, family=binomial)
summary(model)

#model_performance()

library(car)
car::vif(model)

library(pROC)
library(caret)
set.seed(12)
training.samples <- data$Diagnosis %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
data$fitted.results <- predict(model,newdata=data, type='response', allow.new.levels = TRUE)
#calculating and plotting ROC and AUC
roc_obj <- roc(data$Diagnosis, data$fitted.results)
plot(roc_obj, print.thres = "best",print.auc=T)
coords(roc_obj, "best", "threshold")
#spe:0.836, sen: 0.856, AUC=0.928
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.433, 1, 0)
test<-as.numeric(test.data$Diagnosis)-1
mean(predicted.classes == test)
#accuracy: 85.2%

#model 3
model<-glm(Diagnosis~Age+Gender+depression+wspain+yrs_diagnosed+fatigue+pain_intensity1, data, family=binomial)
summary(model)

#model_performance()

library(car)
car::vif(model)

library(pROC)
library(caret)
set.seed(12)
training.samples <- data$Diagnosis %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
data$fitted.results <- predict(model,newdata=data, type='response', allow.new.levels = TRUE)
#calculating and plotting ROC and AUC
roc_obj <- roc(data$Diagnosis, data$fitted.results)
plot(roc_obj, print.thres = "best",print.auc=T)
coords(roc_obj, "best", "threshold")
#spe:0.877, sen: 0.812, AUC=0.93
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.552, 1, 0)
test<-as.numeric(test.data$Diagnosis)-1
mean(predicted.classes == test)
#accuracy: 86.9%

#model 4
model<-glm(Diagnosis~Age+Gender+yrs_diagnosed+depression+wspain+fatigue+pain_intensity1+pain_behavior, data, family=binomial)
summary(model)

model<-glm(Diagnosis~Age+Gender*depression+wspain+yrs_diagnosed+fatigue+pain_intensity1+pain_behavior, data, family=binomial)
summary(model)

library(car)
car::vif(model)

library(pROC)
library(caret)
set.seed(12)
training.samples <- data$Diagnosis %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
data$fitted.results <- predict(model,newdata=data, type='response', allow.new.levels = TRUE)
#calculating and plotting ROC and AUC
roc_obj <- roc(data$Diagnosis, data$fitted.results)
plot(roc_obj, print.thres = "best",print.auc=T)
coords(roc_obj, "best", "threshold")
#spe:0.863, sen: 0.85, AUC=0.935
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.494, 1, 0)
test<-as.numeric(test.data$Diagnosis)-1
mean(predicted.classes == test)
#accuracy: 88.5% 


#model 5
model<-glm(Diagnosis~Age+Gender*depression+wspain+pain_behavior+fatigue+pain_intensity1*yrs_diagnosed, data, family=binomial)
summary(model)
#AIC=214.66

set.seed(12)
training.samples <- data$Diagnosis %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
data$fitted.results <- predict(model,newdata=data, type='response', allow.new.levels = TRUE)
#calculating and plotting ROC and AUC
roc_obj <- roc(data$Diagnosis, data$fitted.results)
plot(roc_obj, print.thres = "best",print.auc=T)
coords(roc_obj, "best", "threshold")
#spe:0.884, sen: 0.856, AUC=0.939
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.491, 1, 0)
test<-as.numeric(test.data$Diagnosis)-1
mean(predicted.classes == test)
#accuracy: 90.2% best so far
library(MLmetrics)
F1_Score(test, predicted.classes, positive = NULL)

ggboxplot(data, x = "Gender", y = "depression",
          color = "Gender", add = "jitter", shape = "Gender")+ facet_grid(.~Diagnosis)

ggplot(data, aes(x=pain_intensity1, y=yrs_diagnosed, color=Diagnosis))+geom_smooth(method = 'loess',span =2, na.rm=TRUE)

ggplot(data, aes(x=pain_intensity1, y=, color=Diagnosis))+geom_smooth(method = 'loess',span =2, na.rm=TRUE)

ggplot(data, aes(x=wspain, y=pain_intensity1))+geom_point()+facet_grid(.~Diagnosis)#+xlab("Centered time (months) by group")

#model 6
model<-glm(Diagnosis~Age+Gender+depression+wspain+pain_behavior+fatigue+pain_intensity1+yrs_diagnosed, data, family=binomial)
summary(model)
#AIC=221.13

model<-glm(Diagnosis~Age+Gender*depression+wspain*pain_intensity1*yrs_diagnosed+pain_behavior+fatigue, data, family=binomial)
summary(model)
#216.65

ggplot(data, aes(x=pain_intensity1, y=wspain, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)
#pcs, depression, fatigue, interference- same trend, higher for chronic pain
#CI overlap
set.seed(123)
training.samples <- data$Diagnosis %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
data$fitted.results <- predict(model,newdata=data, type='response', allow.new.levels = TRUE)
#calculating and plotting ROC and AUC
roc_obj <- roc(data$Diagnosis, data$fitted.results)
plot(roc_obj, print.thres = "best",print.auc=T)
coords(roc_obj, "best", "threshold")
#spe:0.918, sen: 0.838, AUC=0.942
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.527, 1, 0)
test<-as.numeric(test.data$Diagnosis)-1
mean(predicted.classes == test)
#Accuracy= 86.9%

#--------------------3333333333333333-----------
#pain widespreadness (sum of the body locations), depression, fatigue, pain intensity, 
#interference, pain behavior and catastrophizing potentially moderated by diagnosis (rf1c).

#correlation, no moderation
model<-lm(wspain~depression, data=data)
d<-summary(model)
model<-lm(wspain~fatigue, data=data)
f<-summary(model)
model<-lm(wspain~pain_intensity1, data=data)
i1<-summary(model)
model<-lm(wspain~pain_behavior, data=data)
b<-summary(model)
model<-lm(wspain~interference, data=data)
i2<-summary(model)
model<-lm(wspain~pcs, data=data)
p<-summary(model)


d<-round(d$coefficients[2,], digits=2)
f<-round(f$coefficients[2,], digits=2)
i1<-round(i1$coefficients[2,], digits=2)
b<-round(b$coefficients[2,], digits=2)
i2<-round(i2$coefficients[2,], digits=2)
p<-round(p$coefficients[2,], digits=2)

regtab<-cbind(d,f,i1,b,i2,p)
knitr::kable(regtab, caption="Summary of correlation testing models", 
             col.names = c("Depression", "Fatigue", "Pain intensity", "Pain behavior", "Interference", "PCS"))

library(ggpubr)
ggdensity(data, x = "wspain",
          add = "mean", rug = TRUE,
          color = "Diagnosis", fill = "Diagnosis",
          palette = c("#00AFBB", "#E7B800"))+
  xlab("Widespread pain")+
  ggtitle("Distribution of widespread pain by diagnosis")

#data$wspain[data$wspain==0]<-0.00001
data$wspainl<-sqrt(data$wspain)

ggdensity(data, x = "wspainl",
          add = "mean", rug = TRUE,
          color = "Diagnosis", fill = "Diagnosis",
          palette = c("#00AFBB", "#E7B800"))+
  xlab("Widespread pain")+
  ggtitle("Distribution of widespread pain (log10) by diagnosis")

library(ggpubr)
ggboxplot(data, x = "Diagnosis", y = "wspain",
          color = "Diagnosis", add = "jitter", shape = "Diagnosis")


depr<-ggplot(data, aes(x=depression, y=wspain, color=Diagnosis))+geom_point()+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+ #facet_grid(.~Gender)
  ylab("Widespread pain")+
  xlab("Depression")

fati<-ggplot(data, aes(x=fatigue, y=wspain, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+#facet_grid(.~Gender)
  xlab("fatigue")+
  ylab("Widespread pain")

intens<-ggplot(data, aes(y=wspain, x=pain_intensity1,  color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+
  ylab("Widespread pain")+
  xlab("Pain intensity (0-10)")


beha<-ggplot(data, aes(y=wspain, x=pain_behavior, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+
  ylab("Widespread pain")+
  xlab("Pain behavior")

intf<-ggplot(data, aes(y=wspain, x=interference,  color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+
  ylab("Widespread pain")+
  xlab("Interference")

pcs<-ggplot(data, aes(y=wspain, x=pcs, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+
  ylab("Widespread pain")+
  xlab("Pain catastrophizing scale")
#similar
library(gridExtra)
grid.arrange(depr, fati, ncol=2)
grid.arrange(intens, beha, ncol=2)
grid.arrange(intf, pcs, ncol=2)


ggplot(data, aes(x=depression, y=wspain, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+ #facet_grid(.~Gender)
  ylab("Widespread pain")+
  xlab("Depression")
###-------------------wspain moderation-----no log----
jdata<-data%>% filter(data$Diagnosis=="JIA")
cdata<-data%>% filter(data$Diagnosis=="Chronic pain condition")
#depression
ggplot(data, aes(x=depression, y=wspain, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+ #facet_grid(.~Gender)
  ylab("Widespread pain")+
  xlab("Depression")

model<-lm(wspain~depression*Diagnosis, data=data)
summary(model)

#subgroup analysis
model<-lm(wspain~I(depression^2), data=jdata)
summary(model)
model<-lm(wspain~sqrt(depression), data=jdata)
summary(model)

model<-lm(wspain~depression, data=cdata)
summary(model)

#significant
library(interactions)

interact_plot(model, pred=fatigue, modx=Diagnosis, interval = TRUE, 
              x.label ="Widespread pain", 
              y.label = "Depression",
              main.title ="Widespread pain vs. depression by diagnosis", 
              legend.main = "Diagnosis",
              data=data)
ss<-sim_slopes(model, pred=fatigue, modx=Diagnosis)
as_huxtable(ss)
#slope of chronic pain is significant

##fatigue
ggplot(data, aes(x=fatigue, y=wspain, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+ #facet_grid(.~Gender)
  ylab("Widespread pain")+
  xlab("Fatigue")

model<-lm(wspain~fatigue*Diagnosis, data=data)
summary(model)
#non significant
model<-lm(wspain~Diagnosis*sqrt(pain_behavior), data=data)
summary(model)

model<-lm(wspain~Diagnosis*pcs, data=data)
summary(model)

model<-lm(wspain~sqrt(pcs), data=cdata)
summary(model)
model<-lm(wspain~pcs, data=cdata)
summary(model)

model<-lm(wspain~pain_behavior, data=jdata)
summary(model)

model<-lm(wspain~pcs, data=jdata)
summary(model)

##pain intensity
ggplot(data, aes(x=pain_intensity1, y=wspain, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+ #facet_grid(.~Gender)
  ylab("Widespread pain")+
  xlab("Pain intensity")

model<-lm(wspain~pain_intensity1*Diagnosis, data=data)
summary(model)
#significant
interact_plot(model, pred=pain_intensity1, modx=Diagnosis, interval = TRUE, 
              x.label ="Widespread pain", 
              y.label = "Pain intensity",
              main.title ="Widespread pain vs. pain intensity by diagnosis", 
              legend.main = "Diagnosis",
              data=data)
ss<-sim_slopes(model, pred=pain_intensity1, modx=Diagnosis)
as_huxtable(ss)
#both slopes are significant

#pain behavior
ggplot(data, aes(x=pain_behavior, y=wspain, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+ #facet_grid(.~Gender)
  ylab("Widespread pain")+
  xlab("Pain behavior")

model<-lm(wspain~pain_behavior*Diagnosis, data=data)
summary(model)
model<-lm(wspain~sqrt(pain_behavior)*Diagnosis, data=data)
summary(model)
#non significant

#interference
ggplot(data, aes(x=interference, y=wspain, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+ #facet_grid(.~Gender)
  ylab("Widespread pain")+
  xlab("Interference")

model<-lm(wspain~interference*Diagnosis, data=data)
#summary(model)
#significant
interact_plot(model, pred=interference, modx=Diagnosis, interval = TRUE, 
              x.label ="Widespread pain", 
              y.label = "interference",
              main.title ="Widespread pain vs. interference by diagnosis", 
              legend.main = "Diagnosis",
              data=data)
ss<-sim_slopes(model, pred=interference, modx=Diagnosis)
as_huxtable(ss)
#both slopes significant

#PCS
ggplot(data, aes(x=pcs, y=wspain, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+ #facet_grid(.~Gender)
  ylab("Widespread pain")+
  xlab("PCS")

model<-lm(wspain~pcs*Diagnosis, data=data)
summary(model)
#significant
interact_plot(model, pred=pcs, modx=Diagnosis, interval = TRUE, 
              x.label ="Widespread pain", 
              y.label = "PCS",
              main.title ="Widespread pain vs. PCS by diagnosis", 
              legend.main = "Diagnosis",
              data=data)
ss<-sim_slopes(model, pred=pcs, modx=Diagnosis)
as_huxtable(ss)
#single slope significant




##depression
model<-lm(wspainl~depression, data=data)
summary(model)
#R2=0.19, AIC=2496, significant
model<-lm(wspainl~Diagnosis*sqrt(depression), data=data)
summary(model)
#R2=0.29, AIC=2452
AIC(model)
model<-lm(wspainl~Diagnosis+depression, data=data)
summary(model)


##fatigue
data$fatigues<-sqrt(data$fatigue)

model<-lm(wspainl~fatigues, data=data)
summary(model)
AIC(model)
#interaction not significant
model<-lm(wspainl~Diagnosis*fatigues, data=data)
summary(model)

model<-lm(wspainl~fatigue, data=data)
summary(model)
#significant correlation, estimate=0.5788

#pain intensity
model<-lm(wspainl~sqrt(pain_intensity1), data=data)
summary(model)

model<-lm(wspainl~pain_intensity1*Diagnosis, data=data)
summary(model)

model<-lm(wspainl~fatigue, data=data)
summary(model)



library(interactions)
library(huxtable)
interact_plot(model, pred=fatigue, modx=Diagnosis, interval = TRUE, 
              x.label ="Widespread pain", 
              y.label = "Fatigue",
              main.title ="Widespread pain vs. fatigue by diagnosis", 
              legend.main = "Diagnosis",
              data=data)
ss<-sim_slopes(model, pred=fatigue, modx=Diagnosis)
as_huxtable(ss)
#the slope of chronic pain is statistically significant, but JIA is not
#might be due to small amount of observations with high widespread pain in the JIA group



#subgroup analysis
jdf<-data%>% filter(data$Diagnosis=="JIA")
cdf<-data%>% filter(data$Diagnosis=="Chronic pain condition")


model<-lm(wspain~depression , data=jdf)
summary(model)
AIC(model)
#1069, non significant

model<-lm(wspain~I(depression^2), data=jdf)
summary(model)
AIC(model)
#1072, non significant


ggplot(data, aes(x=pain_intensity1, y=wspain, color=Diagnosis))+geom_smooth(method = 'loess',span =2, na.rm=TRUE)
#chronic pain patients have an upward trend- pain intensity increase as swpain increases
#strong correaltion beween wspain and pain intensity.
#JIA patients have an almost horizontal trend

#piecewise model to describe the association between wspain and depression/..
#wspain median=Q2=6, Q3=18 max=61 the median or mean (13) cound be the cutpoints
a<-ggplot(data, aes(x=Age))+geom_histogram(binwidth=1)+xlab("Age (years)")+
  ggtitle("Distribution of age")

hist(data$pain_quality2)
m<-lm(wspain~pain_quality1, data)
summary(m)
ggplot(data, aes(x=pain_quality1, y=wspain, color=Diagnosis))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)

set.seed(322)
training.samples <- data$Health %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

#model<-lda(Health ~ fatigue + pain_intensity1+ Age  , data=train.data, Hess=TRUE)
#model<-lda(Health ~ fatigue , data=train.data, Hess=TRUE)
model<-lda(Health ~ depression+ fatigue+pain_behavior+ Age+interference+pain_intensity1+ 
             pcs+wspain, data=train.data, Hess=TRUE)

# Make predictions on the test data
predictions <- model %>% predict(test.data)
# Model accuracy
conf<-confusionMatrix(predictions$class, test.data$Health)
t<-conf$table
knitr::kable(t, caption="LDA model: prediction vs. reference")