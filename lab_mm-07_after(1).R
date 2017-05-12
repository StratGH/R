# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# Математическое моделирование: Практика 5
#  Кросс-валидация и бутстреп

library('MASS')              
library('splines')           # сплайны
library('gam')               # обобщённые аддитивные модели
library('akima')             # график двумерной плоскости
#library('boot')

?Boston

# Локальная регрессия ----------------------------------------------------------

agelims <- range(Boston$age)

# значения age, для которых делаем прогноз (от min до max с шагом 1)
age.grid <- seq(from = agelims[1], to = agelims[2])


# график 
plot(Boston$age, Boston$nox, xlim = agelims, cex = 0.5, col = 'darkgrey')
title('Локальная регрессия')

# подгоняем модель c окном 0.2
fit1 <- loess(nox ~ age, span = 0.2, data = Boston)
# подгоняем модель c окном 0.3
fit2 <- loess(nox ~ age, span = 0.3, data = Boston)
# подгоняем модель c окном 0.4
fit3 <- loess(nox ~ age, span = 0.4, data = Boston)
# подгоняем модель c окном 0.5
fit4 <- loess(nox ~ age, span = 0.5, data = Boston)
# подгоняем модель c окном 0.6
fit5 <- loess(nox ~ age, span = 0.6, data = Boston)
# подгоняем модель c окном 0.7
fit6 <- loess(nox ~ age, span = 0.7, data = Boston)
# подгоняем модель c окном 0.8
fit7 <- loess(nox ~ age, span = 0.8, data = Boston)


# рисум модели
lines(age.grid, predict(fit1, data.frame(age = age.grid)),
      col = 'red', lwd = 2)
lines(age.grid, predict(fit4, data.frame(age = age.grid)),
      col = 'green', lwd = 2)
lines(age.grid, predict(fit7, data.frame(age = age.grid)),
      col = 'blue', lwd = 2)
legend('topleft', 
       c('s = 0.2', 's = 0.5', 's = 0.8'),
       col = c('red','green', 'blue'), lty = 1, lwd = 2, cex = 0.8)

#anova(fit, fit2,fit3,fit4,fit5, fit6, fit7)

my.seed <- 1
# общее число наблюдений
n <- nrow(Boston)

# доля обучающей выборки
train.percent <- 0.5

# выбрать наблюдения в обучающую выборку


MSE <- rep(0, 7)
names(MSE) <- (2:8)/10

for (i in 2:8){
  fit <- loess(nox ~ age, span = (i/10), data = Boston)
  for (j in 1:5){
    my.seed <- j
    set.seed(my.seed)
    inTrain <- sample(n, n * train.percent)
    MSE[i-1] <- MSE[i-1]+mean((Boston$nox[-inTrain] - predict(fit,
                                                  Boston[-inTrain, ]))^2)
  }
  MSE[i-1] <- MSE[i-1]/5
}




#cv.glm(bos, fit1,
#       K = 5)$delta[1]
#cv.err.k.fold <- rep(0, 7)
#names(cv.err.k.fold) <- (2:8)/10
#bos <- Boston[, c('nox', 'age')]
#
#for (i in 2:8){
#  fit <- loess(nox ~ age, span = (i/10), data = Boston)
#  cv.err.k.fold[i-1] <- cv.glm(bos, fit,
#                             K = 10)$delta[1]
#}
# результат
#cv.err.k.fold



