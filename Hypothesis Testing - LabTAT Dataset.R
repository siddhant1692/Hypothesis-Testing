
LabTAT <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Hypothesis Testing\\LabTAT.csv')

a <- LabTAT$Laboratory.1
b <- LabTAT$Laboratory.2
c <- LabTAT$Laboratory.3
d <- LabTAT$Laboratory.4

boxplot(a)
boxplot(b)
boxplot(c)
boxplot(d)
hist(a)
hist(b)
hist(c)
hist(d)

shapiro.test(a)
shapiro.test(b)
shapiro.test(c)
shapiro.test(d)

t.test(a,b,c,d,alternative = 'two.sided',mu=0,paired = TRUE, var.equal = FALSE,conf.level = 0.95)

