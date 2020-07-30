
faltoons <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Hypothesis Testing\\Faltoons.csv')

View(faltoons)

table(Weekdays)
table(Weekend)

a <- prop.table(table(Weekdays))
b <- prop.table(table(Weekend))

t <- data.frame(a,b)
t1 <- t[-c(1),-c(1,3,5,7)]

chisq.test(t1)

t.test(t1)


