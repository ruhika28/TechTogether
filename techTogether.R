#install.packages("party")<- so that you  can use the ctree fucntion which is a classification alorithm it helps classify
#the average interest rate  we make use of ctree in order to do a conditional classifcation
# to predict what the interest rates should be on an average on the basis of cosigners payback history,  
#install.packages("ggplot2")
#incase using ggplot2 forplotting data
install.packages("partykit")
library(party)#invoke library party
library(ggplot2)#invoke ggplot2
#reading the  file 

df<-read.csv(choose.files())
#checing number of obervations
str(df)
#checking for null values 
null_values_if_any<-df[complete.cases(df),]
#This recheck the number of  rows after elimination of null values
str(null_values_if_any)
  
#not a good fit as the root mean square  value is very high we ignore this linear model 
linear_depedency<-lm(df$interest_rate~df$Married+df$Dependents+df$Education+df$Self_Employed+df$Self_Employed+df$ApplicantIncome+df$CoapplicantIncome)
summary(linear_depedency)

#better fit as root mean value is high and op value is low 
linear_depedency1<-lm(df$interest_rate~df$ApplicantIncome+df$CoapplicantIncome+df$credit_score_cosigner+df$LoanAmount+df$Recent.payment.history)
summary(linear_depedency1)

  
#So we  understand that these factors are important in order to depict
 #interest rate which helps us predict the lender that must approached
 
#all the importatnt factors are taken as an input with a unique identifier to distinguish each observation
impfactordataset<-df[,c(4,5,6,7,8,9,10,12,14,13,15,1)]
impfactordataset <- df[complete.cases(impfactordataset),]

#displaying the column names 
colnames(impfactordataset)

#standardizing the values  based on interest rate and companies 
#wll be using this to predict similary  compaines offerening  loans  based  on average interest rates
means_impfactor <- tapply(impfactordataset$interest_rate,impfactordataset$company,mean)


distances2 <- dist(means_impfactor,method="euclidean")



cluster<-hclust(distances2)
#this plots a decision tree and is a clustering alogorithm
plot(cluster)
# from this we understand that solely based on interest rates Credible and Lending offer competitive rates 
#prosper and Sofi also demonstrate competitive rates 



#based on credit score of the cosigner  and their recent payment history  we determine the average interest
#rate that they can be charged 

imp2<-impfactordataset[,c("Loan_ID","Credit_History","CoapplicantIncome","interest_rate","company","Education", "credit_score_cosigner", "Recent.payment.history")]

classification<-ctree(interest_rate~credit_score_cosigner+Recent.payment.history,data=impfactordataset)
plot(classification)

# it is observed that applicants not having cosigners with a good payment history  and credit scores  up having meadian  interests rates 
#fixed at 7.7 whereas applicants having cosigners having a good payment history end up with an median 
#interest rate of 4.65


classification2<-ctree(company~interest_rate+credit_score_cosigner+Recent.payment.history,data=impfactordataset)
plot(classification2)
#shows us that of most loan request were for sofi company 

data()



