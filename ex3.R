music <- read.csv("music-data.csv")
summary(music)

music$sex <- as.factor(music$sex)
music$subscribeToMusic <- as.factor(music$subscribeToMusic)
music$Segment <- as.factor(music$Segment)
layout(1) 
plot(music$Segment,
     main = "Respondent Categories",
     xlab = "Categories",
     ylab = "numbers")

barplot(table(music$Segment),
     main = "Respondent Categories",
     xlab = "Categories",
     ylab = "Frequency Count")

boxplot(music$age,
        main = "Age distribution",
        ylab = "ages")

hist(music$householdIncome,
        breaks = 8,
     main = "household income of respondents",
     xlab = "household income",
     ylab = "numbers")

plot(density(music$householdIncome),
     main = "density of household income of respondents",
     xlab = "household income")


# Q2
store <- read.csv("store-data.csv")

par(mfrow = c(1,2)) 

hist(store$p1sales, breaks = 10,
     ylim = c(0, 800),
     main = "quantity sold of product 1",
     xlab = "quantity")

hist(store$p2sales, breaks = 10,
     ylim = c(0, 800),
     main = "quantity sold of product 2",
     xlab = "quantity")
par(mfrow = c(1,1))


boxplot(store$p1price, store$p2price,
        main = "price change of products",
        ylab = "price",
        names = c("product 1", "product 2")
        )
plot(store$p1sales ~ store$p2price,
     main = "p1sales with p2price",
     xlab = "p2price",
     ylab = "p1sales")
lm1 <-lm(store$p1sales ~ store$p2price, data = store)
abline(coef(lm1), lty = 2)

pairs(~ p1sales + p2sales + p1price + p2price, data = store,
      main = "Scatterplot Matrix for the Sales & Prices of the Two Products",
      labels = c("Sales\n Product 1", "Sales\n Product 2", "Price\n Product 1", "Price\n Product 2"))
cor(store$p1sales, store$p2sales)
# the relationship between the two prices is difficult to assess visually due to the limited number of values
cor(store$p1price, store$p2price)
# it seems the two prices are very weak and (slightly) positively linearly associated
# the correlation between each product's price and sales appears negative (in line with market theory)
cor(store$p1sales, store$p1price)
cor(store$p2sales, store$p2price)
# but the correlation between each product's sales and the other product's price is positive
cor(store$p1sales, store$p2price)
cor(store$p2sales, store$p1price)
# as suggested in the Exercise 2, the two products seem to be substitutive goods rather than complements

par(mfrow = c(2,2)) 
boxplot(store$p1sales ~ store$p1prom,
        main = "p1sales and p1prom",
        xlab = "p1prom",
        ylab = "p1sales")
boxplot(store$p2sales ~ store$p2prom,
        main = "p2sales and p2prom",
        xlab = "p2prom",
        ylab = "p2sales")
boxplot(store$p1price ~ store$p1prom,
        main = "p1price and p1prom",
        xlab = "p1prom",
        ylab = "p1price")
boxplot(store$p2price ~ store$p2prom,
        main = "p2price and p2prom",
        xlab = "p2prom",
        ylab = "p2price")
# B.2
par(mfrow = c(1,1)) 
plot(music$subscribeToMusic ~ music$Segment, 
     main = "music subscription and respondents’ segment",
     xlab = "segment",
     ylab = "music subscription")

lm2 <-lm(music$subscribeToMusic ~ music$Segment, data = store)
abline(coef(lm2), lty = 2)

# C.a

mycol <- c("black", "bisque3")
head(mycol[as.factor(music$subscribeToMusic)])
plot(householdIncome ~ age, data = music,
     main = "Scatterplot: Respondents' Household Income vs Age",
     xlab = "Age",
     ylab = "Income",
     cex = 1.0, 
     pch = 16,
     col = mycol)
legend(x = "bottomright", pch = 16, col = mycol, legend = c("No subscribe", "subscribe"))
abline(h = c(50000, 75000), lty = 2, col = "darkred")

lm3 <-lm(householdIncome ~ age, data = music)
abline(coef(lm3), lty = 1, col = "red")




