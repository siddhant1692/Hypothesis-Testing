
Cutlets <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Hypothesis Testing\\Cutlets.csv')

x <- Cutlets$Unit.A
y <- Cutlets$Unit.B

boxplot(x)
boxplot(y)
hist(x)
hist(y)

shapiro.test(x)
shapiro.test(y)
var.test(x,y)

t.test(x,y, alternative="two.sided",conf.level = 0.95)
