setwd("D:/Desktop/HUST/2023.1/Phân tích số liệu")
food <- read.csv("food-texture.csv",row.names = "X")
str(food)
food <- scale(food)

food_fa <- factanal(food, factors = 2)

food_fa$uniquenesses 
h <- apply(food_fa$loadings, 1, function(row) sum(row^2))
#1 - h

Psi <- diag(food_fa$uniquenesses)
S <- food_fa$correlation
Sigma <- food_fa$loadings %*% t(food_fa$loadings) + Psi

print(Psi) 
print(S)
print(Sigma)

round(S - Sigma, 6)

food_fa_none <- factanal(food, factors = 2, rotation = "none")
food_fa_varimax <- factanal(food, factors = 2, rotation = "varimax")
food_fa_promax <- factanal(food, factors = 2, rotation = "promax")

no_rotation <- data.frame(factor1 = food_fa_none$loadings[, 1],
                          factor2 = food_fa_none$loadings[, 2])

varimax_rotation <- data.frame(factor1 = food_fa_varimax$loadings[, 1],
                               factor2 = food_fa_varimax$loadings[, 2])

promax_rotation <- data.frame(factor1 = food_fa_promax$loadings[, 1],
                              factor2 = food_fa_promax$loadings[, 2])
print(no_rotation)
print(varimax_rotation)
print(promax_rotation)

par(mfrow = c(1, 3))
plot(food_fa_none$loadings[, 1],
     food_fa_none$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     main = "No rotation")
abline(h = 0, v = 0)

text(food_fa_none$loadings[, 1] - 0.08,
     food_fa_none$loadings[, 2] + 0.08,
     colnames(food),
     col = "blue")
abline(h = 0, v = 0)

plot(food_fa_varimax$loadings[, 1],
     food_fa_varimax$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     main = "Varimax rotation")

text(food_fa_varimax$loadings[, 1] - 0.08,
     food_fa_varimax$loadings[, 2] + 0.08,
     colnames(food),
     col = "blue")
abline(h = 0, v = 0)

plot(food_fa_promax$loadings[, 1],
     food_fa_promax$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     main = "Promax rotation")
abline(h = 0, v = 0)

text(food_fa_promax$loadings[, 1] - 0.08,
     food_fa_promax$loadings[, 2] + 0.08,
     colnames(food),
     col = "blue")
abline(h = 0, v = 0)


