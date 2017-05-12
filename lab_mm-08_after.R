
# Математическое моделирование: Практика 8
#  Деревья решений

library('tree')
library('ISLR')
library('MASS')
library('randomForest')
library('gbm')



# Бэггинг ----------------------------------------------

# бэггинг -- частный случай случайного леса с m = p, поэтому и то, и другое
#  можно построить функцией randomForest

# бэггинг: =====================================================================
au <- Auto[,-9]

my.seed <- 1
n <- nrow(au)
train.percent <- 0.5
set.seed(my.seed)
inTrain <- sample(n, n * train.percent)
# обучающая выборка -- 50%
ncol(au)
#  берём все 7 предикторов на каждом шаге 
set.seed(1)
bag.au <- randomForest(mpg ~ ., data = au, subset = inTrain, 
                           mtry = 7, importance = TRUE)
bag.au
?randomForest
# прогноз
yhat.bag <-  predict(bag.au, newdata = au[-inTrain, ])

# график "прогноз -- реализация"
au.test <- au[-inTrain, "mpg"]

plot(yhat.bag, au.test)
abline(0, 1)   # линия идеального прогноза
mean((yhat.bag - au.test)^2)  # MSE на тестовой (тыс.долл.)

# можно изменить число деревьев с помощью аргумента ntree
set.seed(1)
bag.au <- randomForest(mpg ~ ., data = au, subset = inTrain,
                          mtry = 7, ntree = 500)
yhat.bag <- predict(bag.au, newdata = au[-inTrain, ])
mean((yhat.bag - au.test)^2)

importance(bag.au)
