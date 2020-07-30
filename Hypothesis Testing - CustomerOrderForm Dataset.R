
cof <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Hypothesis Testing\\Costomer+OrderForm.csv')

table(Phillippines)
table(Indonesia)
table(Malta)
table(India)

a <- prop.table(table(Phillippines))
b <- prop.table(table(Indonesia))
c <- prop.table(table(Malta))
d <- prop.table(table(India))

t <- data.frame(a,b,c,d)
t1 <- t[-c(2),-c(1,3,5,7)]

chisq.test(t1)
