# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# Математическое моделирование: Практика 1

# Точность модели, непрерывный Y -----------------------------------------------

# Пример из лекции =============================================================

#  Генерируем данные ###########################################################

# ядро
my.seed <- 1486372882

# наблюдений всего
n.all <- 60
# доля обучающей выборки
train.percent <- 0.85
# стандартное отклонение случайного шума
res.sd <- 1
# границы изменения X
x.min <- 5
x.max <- 105

# фактические значения X
set.seed(my.seed)
x <- runif(x.min, x.max, n = n.all)
x[1:5]

# случайный шум
set.seed(my.seed)
res <- rnorm(mean = 0, sd = res.sd, n = n.all)

# номера наблюдений тестовой выборки
set.seed(my.seed)
inTrain <- sample(seq_along(x), size = train.percent*n.all)
inTrain[1:5]

# истинная функция взаимосвязи\
#y.func <- function(x){4-2e-02*x+5.5e-03*x^2-4.9e-05*x^3}
y.func <- function(x){8 + 3.5*sin((x - 75) / 9)}

# фактические значения y
y <- y.func(x) + res
y[1:5]

# для графика истинной взаимосвязи
x.line <- seq(x.min, x.max, length = 200)
y.line <- y.func(x.line)

# Создаём векторы с данными для построения графиков ############################

# наблюдения на обучающей  выборке
x.train <- x[inTrain]
y.train <- y[inTrain]

# наблюдения на тестовой выборке
x.test <- x[-inTrain]
y.test <- y[-inTrain]

#  График 1: Исходные данные на график #########################################

# убираем широкие поля графика
par(mar = c(4, 4, 1, 1))

# наименьшие / наибольшие значения по осям
x.lim <- c(x.min, x.max)
y.lim <- c(min(y), max(y))

# фактические значения на обучающей выборке
plot(x.train, y.train, col = grey(0.2),
     pch = 21, bg = grey(0.2),
     xlab = 'X', ylab = 'Y', 
     xlim = x.lim, ylim = y.lim,
     cex = 1.2, cex.lab = 1.2, cex.axis = 1.2)
# фактические на тестовой выборке
points(x.test, y.test, col = 'red', bg = 'red', pch = 21)
# истинная взаимосвязь
lines(x.line, y.line, lwd = 2, lty = 2)
# легенда
legend('bottomright', legend = c('обучение', 'тест', 'f(X)'),
       pch = c(16, 16, NA), col = c(grey(0.2), 'red',
                                    'black'),
       lty = c(0, 0, 2), lwd = c(0, 0, 2),
       cex = 1.2)

#  Строим модель ###############################################################

# сплайн с df = 6
mod <- smooth.spline(x = x.train, y = y.train,
                     df = 6)
# модельные значения
#  на обучающей выборке
y.model.train <- predict(mod, 
                         data.frame(x = x.train))$y[, 1]
y.model.train[1:5]
# на тестовой выборке
y.model.test <- predict(mod, 
                         data.frame(x = x.test))$y[, 1]
y.model.test[1:5]

# оценка точности
MSE <- c(sum((y.train - y.model.train)^2)/length(y.train),
         sum((y.test - y.model.test)^2)/length(y.test))
MSE


#  Цикл по степеням свободы ####################################################

# макс. степень свободы
max.df <- 40

# таблица для записи ошибок
tbl <- data.frame(df = 2:40,
                  MSE.train = rep(0, max.df - 1),
                  MSE.test = rep(0, max.df - 1))
head(tbl)

for(i in 2:max.df){
  # модель
  mod <- smooth.spline(x = x.train, y = y.train,
                       df = i)
  # модельные значения
  y.model.train <- predict(mod, 
                           data.frame(x = x.train))$y[, 1]
  y.model.test <- predict(mod, 
                          data.frame(x = x.test))$y[, 1]
  # расчёт ошибки
  MSE <- c(sum((y.train - y.model.train)^2)/length(y.train),
           sum((y.test - y.model.test)^2)/length(y.test))
  # запись в таблицу
  tbl[tbl$df == i, c(2, 3)] <- MSE
}

head(tbl)
# до этого места разабрали на паре

#  График 2: Зависимость MSE от гибкости модели ################################

plot(x = tbl$df, y = tbl$MSE.test, type = 'l', col = 'red', lwd = 2,
     xlab = 'Степени свободы сплайна', ylab = 'MSE',
     ylim = c(min(tbl$MSE.train, tbl$MSE.test), 
              max(tbl$MSE.train, tbl$MSE.test)),
     cex = 1.2, cex.lab = 1.5, cex.axis = 1.5)
lines(x = tbl$df, y = tbl$MSE.train, col = grey(0.3), lwd = 2)
# неустранимая ошибка
abline(h = res.sd, lty = 2, col = grey(0.4), lwd = 2)

# степени свободы у наименьшей ошибки на тестовой выборке
tbl[tbl$MSE.test == min(tbl$MSE.test), 'df']
# подробное сообщение в консоль
print(paste0('Наименьшая MSE на тестовой выборке достигается при df = ', 
             tbl[tbl$MSE.test == min(tbl$MSE.test), 'df'], '.'))

# вертикальная прямая для найденного значения df
abline(v = tbl[tbl$MSE.test == min(tbl$MSE.test), 'df'], lty = 2, lwd = 2)
# точка минимальной MSE на тестовой выборке
points(x = tbl[tbl$MSE.test == min(tbl$MSE.test), 'df'],
       y = tbl[tbl$MSE.test == min(tbl$MSE.test), 'MSE.test'], 
       pch = 15, col = 'blue', cex = 1.5)

#  График 3: Модель с наименьшей MSE на тестовой выборке #######################
# то же, что график 2, но для df, соответствующего наименьшей MSE на тестовой выборке

mod.MSE.test <- smooth.spline(x = x.train, y = y.train, df = 4)

# для гладких графиков модели
x.model.plot <- seq(x.min, x.max, length = 250)
y.model.plot <- predict(mod.MSE.test, data.frame(x = x.model.plot))$y[, 1]

# убираем широкие поля рисунка
par(mar = c(4, 4, 1, 1))

# наименьшие/наибольшие значения по осям
x.lim <- c(x.min, x.max)
y.lim <- c(min(y), max(y))

# наблюдения с шумом (обучающая выборка)
plot(x.train, y.train, col = grey(0.2), bg = grey(0.2), pch = 21,
     xlab = 'X', ylab = 'Y', 
     xlim = x.lim, ylim = y.lim, 
     cex = 1.2, cex.lab = 1.5, cex.axis = 1.5)

# наблюдения тестовой выборки
points(x.test, y.test, col = 'red', bg = 'red', pch = 21)

# истинная функция
lines(x.line, y.line, lwd = 2, lty = 2)

# модель
lines(x.model.plot, y.model.plot, lwd = 2, col = 'blue')

# легенда

legend('bottomright', legend = c('обучение', 'тест', 'f(X)', 'модель'),
       pch = c(16, 16, NA, NA), 
       col = c(grey(0.2), 'red', 'black', 'blue'),  
       lty = c(0, 0, 2, 1), lwd = c(1, 1, 2, 2), cex = 1.2)


# Сравнение с результатами, полученными для первоначальных параметров:

# MSE на обучающей выборке была 0.8707958 для сплайна с df = 6
# MSE на тестовой выборке была 1.7136031 для сплайна с df = 6
# Наименьшая MSE на тестовой выборке достигалось при df = 12, она была 1.684893

# MSE на обучающей выборке стала 1.274271 для сплайна с df = 6
# MSE на тестовой выборке стала 1.187493 для сплайна с df = 6
# И наименьшая MSE на тестовой выборке достигается при df = 6

# Усиление нелинейности (увеличение df сплайна) приводит к тому, MSE на обучающей выборке будет уменьшаться,
# а MSE на тестовой выборке, скорее всего, будет быстро уменьшаться до определённого значения df, а затем будет расти.

