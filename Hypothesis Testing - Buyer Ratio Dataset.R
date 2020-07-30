
BuyerRatio <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Hypothesis Testing\\BuyerRatio.csv')

attach(BuyerRatio)
View(BuyerRatio)
t1 <- BuyerRatio$East 
t2 <- BuyerRatio$North
t3 <- BuyerRatio$South
t4 <- BuyerRatio$West
t = data.frame(t1,t2,t3,t4)
View(t)

chisq.test(t)
