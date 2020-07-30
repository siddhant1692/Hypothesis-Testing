#Hypothesis Assignment
#1
#A F&B manager wants to determine whether there is any significant difference in the diameter of the
#cutlet b/w two units. A randomly selected sample of cutlets was collcted from both units and measured.
#analyze the data and draw inferences at 5% significance level. please state the assumption and tests that
#you are carried out to check validity of assumption"""

#research question:- is there any significance difference in diameter of cutlets of two units?
library(stats)
cutlets<-read.csv(file.choose())
attach(cutlets)

#checking for normality of data by shapiro.test
#Ho:data is not normal
#Ha:data is  normal

shapiro.test(Unit.A)

#since p-value is 0.32>0.05 indicates that unit.A is normaly distributed

shapiro.test(Unit.B)
#since p-value is 0.5525>0.05indicates that unit.b is normaly distributed

#to apply which type of two sample test we require, we have to check varibles have equal varriances or not

#H0:the varince are not equal 
#Ha:variance are  equal

var.test(Unit.A,Unit.B)

#since p-value is 0.7>0.05 so we can say that their varience are equal and we can apply 2-sample test for
#equal varience

#H0:diameter(unit.a cutlet)!= diameter(unit.b cutlet)
#Ha:diameter(unit.a cutlet)=diameter(unit.b cutlet)

t.test(Unit.A,Unit.B,alternative = 'two.sided',conf.level = 0.95)
#since p-value 0.47>0.05 so we can conclude that diameter of cutlets of both units are same
-------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------
#2
#A hospital wants to determine whether there is any difference in the average Turn Around Time (TAT) of reports of the laboratories on their preferred list. They collected a random sample and recorded TAT for reports of 4 laboratories. TAT is defined as sample collected to report dispatch.

#Analyze the data and determine whether there is any difference in average TAT among the different laboratories at 5% significance level.

library(stats)
lab<-read.csv(file.choose())
attach(lab)

shapiro.test(Laboratory.1)

#since p-value is 0.55>0.05 indicates that Laboratory1 is normaly distributed

shapiro.test(Laboratory.2)
#since p-value is 0.86>0.05indicates that Laboratory2 is normaly distributed

shapiro.test(Laboratory.3)
#since p-value is 0.42>0.05 indicates that Laboratory3 is normaly distributed
shapiro.test(Laboratory.4)#since p-value is 0.66>0.05 indicates that Laboratory4 is normaly distributed
#to apply which type of two sample test we require, we have to check varibles have equal varriances or not

#H0:the varince are not equal 
#Ha:variance are  equal


t.test(Laboratory.1,Laboratory.2,Laboratory.3,Laboratory.4,alternative = 'two.sided',mu=0,paired = TRUE, var.equal = FALSE,conf.level = 0.95)
#since p-value 0.77>0.05 
#Since p-value is more than 0.05 we do not reject null hypothesis


#another method

#View(lab)
#Stacked_Data <- stack(lab)
#View(Stacked_Data)
#attach(Stacked_Data)

#shapiro.test(lab$`Laboratory.1`)
#shapiro.test(lab$`Laboratory.2`)
#shapiro.test(lab$`Laboratory.3`)
#shapiro.test(lab$`Laboratory.4`)
#summary(lab)
# Data is normally distributed
#library(car)
#leveneTest(values~ ind, data = Stacked_Data)
#Anova_results <- aov(values~ind,data = Stacked_Data)
#summary(Anova_results)
# p-value = 0.104 > 0.05 accept null hypothesis 
# All Proportions all equal
-------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
#3
#Sales of products in four different regions is tabulated for males and females. Find if male-female buyer rations are similar across regions.
#chai squre
library(readxl)
buyer_Ratio<-read.csv("C:\\Users\\Nik\\Downloads\\BuyerRatio.csv") # Bahaman.xlsx
attach(buyer_Ratio)
View(buyer_Ratio)
t1<-buyer_Ratio$East 
t2 <- buyer_Ratio$North
t3 <- buyer_Ratio$South
t4 <- buyer_Ratio$West
t = data.frame(t1,t2,t3,t4)
View(t)

chisq.test(t)
#output
#data:  t
#X-squared = 1.5959, df = 3, p-value = 0.6603
..................................................................................................
#method2
#stack_info<-stack(buyer_Ratio)
#Veiw(stack_info)

#View(table(stack_info))
#it will convert stacked data into table formate on which we can apply chi square test

#chisq.test(table(stack_info))
#if i apply above code it will change the count value which may give slight change  in p-value=0.2931 
#with error massage"Warning message:In chisq.test(table(stack_info)) :Chi-squared approximation may be
#incorrect"
.............................................................................................
#method3

#library(readxl)
#buyer_Ratio<-read.csv("C:\\Users\\Nik\\Downloads\\BuyerRatio.csv") # Bahaman.xlsx
#Stacked_Data <- stack(buyer_Ratio)
#View(Stacked_Data)
#attach(Stacked_Data)
#View(buyer_Ratio)
#attach((buyer_Ratio))
#table(Stacked_Data$East, Stacked_Data$North, Stacked_Data$Observed.Values, Stacked_Data$South, Stacked_Data$West)
#t1<- table(East)
#t2 <- table(North)
#t3 <- table(South)
#t4 <- table(West)

#?chisq
#chisq.test(table(Stacked_Data$Males, Stacked_Data$Females))

--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------

#4
#TeleCall uses 4 centers around the globe to process customer order forms. They audit a certain %  of the customer order forms. Any error in order form renders it defective and has to be reworked before processing.  The manager wants to check whether the defective %  varies by centre. Please analyze the data at 5% significance level and help the manager draw appropriate inferences
#Minitab File: CustomerOrderForm.mtw
#Fantaloons Sales managers commented that % of males versus females walking in to the store differ based on day of the week. Analyze the data and determine whether there is evidence at 5 % significance level to support this hypothesis.


library(readxl)
faltoon<-read.csv("C:\\Users\\Nik\\Downloads\\Faltoons.csv") #Faltoons.xlsx
View(faltoon)
attach(faltoon)
table(Weekdays, Weekend)

t2 <- prop.table(table(Weekdays))
t1 <- table(Weekend)
?chisq
chisq.test(table(Country,Defective))
# p-value = 0.6315 > 0.05  => Accept null hypothesis
# => All countries have equal proportions 

#data:  table(Country, Defective)
#X-squared = 1.7244, df = 3, p-value = 0.6315