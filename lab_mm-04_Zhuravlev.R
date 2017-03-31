# Математическое моделирование: Практика 4

library('GGally')
library('MASS')
library('mlbench')

my.seed <- 12345
train.percent <- 0.85

# Данные BreastCancer ---------------------------------------------------------------
data(BreastCancer)

#сохранение без NA:
bc <- BreastCancer[!is.na(BreastCancer$Bare.nuclei),-1]
for (i in 1:9){bc[, i] <- as.numeric(bc[, i])}

# графики разброса
ggpairs(bc[,c(1:3,10)])
ggpairs(bc[,c(4:7,10)])
ggpairs(bc[,c(8:10)])

# обучающая выборка
set.seed(my.seed)
inTrain <- sample(seq_along(bc$Class),
                  nrow(bc) * train.percent)
df <- bc[inTrain,]
df.nt <- bc[-inTrain,]
head(df)

# фактические значения на обучающей выборке
Факт <- df$Class

# Логистическая регрессия ======================================================
model.logit <- glm(Class ~ ., data = df,
                   family = 'binomial')
#?glm
summary(model.logit)

# обучающая выборка:
# прогноз: вероятности принадлежности классу malignant
p.logit <- predict(model.logit, df, 
                   type = 'response')
p.logit
Прогноз <- factor(ifelse(p.logit > 0.5,
                         1, 2),
                  levels = c(1, 2),
                  labels = c('malignant', 'benign'))
Прогноз
head(cbind(Факт, p.logit, Прогноз))
# матрица неточностей
table(Факт, Прогноз)


# LDA ==========================================================================
model.lda <- lda(Class ~ ., data = df)
model.lda

# обучающая выборка:
# прогноз: вероятности принадлежности классу 'malignant'
p.lda <- predict(model.lda, df, type = 'response')
head(p.lda$posterior)
Прогноз <- factor(ifelse(p.lda$posterior[, 'malignant'] > 0.5,
                         1, 2),
                  levels = c(1, 2),
                  labels = c('malignant', 'benign'))

Прогноз
# матрица неточностей
Факт <- df$Class
table(Факт, Прогноз)


# ROC-кривые на обучающей выборке: ======================================================================================================================

# ROC-кривая для LDA ===========================================================

# считаем 1-SPC и TPR для всех вариантов границы отсечения
x <- NULL    # для (1 - SPC)
y <- NULL    # для TPR

# заготовка под матрицу неточностей
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2))
rownames(tbl) <- c('fact.malignant', 'fact.benign')
colnames(tbl) <- c('predict.malignant', 'predict.benign')
tbl

# цикл по вероятностям отсечения
for (p in seq(0, 1, length = 500)){
  # прогноз
  Прогноз <- factor(ifelse(p.lda$posterior[, 'malignant'] > p,
                           1, 2),
                    levels = c(1, 2),
                    labels = c('malignant', 'benign'))
  
  # фрейм со сравнением факта и прогноза
  df.compare <- data.frame(Факт = Факт, 
                               Прогноз = Прогноз)
  
  # заполняем матрицу неточностей
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'malignant' & 
                                 df.compare$Прогноз == 'malignant', ]) 
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'benign' & 
                                 df.compare$Прогноз == 'benign', ])
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'malignant' & 
                                 df.compare$Прогноз == 'benign', ])
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'benign' & 
                                 df.compare$Прогноз == 'malignant', ])
  
  # считаем характеристики
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1])
  y <- c(y, TPR)
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2])
  x <- c(x, 1 - SPC)
}

# строим ROC-кривую
par(mar = c(5, 5, 1, 1))
# кривая
plot(x, y, 
     type = 'l', col = 'blue', lwd = 3,
     xlab = '(1 - SPC)', ylab = 'TPR', 
     xlim = c(0, 1), ylim = c(0, 1))
# прямая случайного классификатора
abline(a = 0, b = 1, lty = 3, lwd = 2)


# ROC-кривая для логистической регрессии ===========================================================

# считаем 1-SPC и TPR для всех вариантов границы отсечения
x2 <- NULL    # для (1 - SPC)
y2 <- NULL    # для TPR

# заготовка под матрицу неточностей
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2))
rownames(tbl) <- c('fact.malignant', 'fact.benign')
colnames(tbl) <- c('predict.malignant', 'predict.benign')
tbl

# цикл по вероятностям отсечения
for (p in seq(0, 1, length = 500)){
  # прогноз
  Прогноз <- factor(ifelse(p.logit > p,
                           1, 2),
                    levels = c(1, 2),
                    labels = c('malignant', 'benign'))
  
  # фрейм со сравнением факта и прогноза
  df.compare <- data.frame(Факт = Факт, 
                               Прогноз = Прогноз)
  
  # заполняем матрицу неточностей
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'malignant' & 
                                 df.compare$Прогноз == 'malignant', ]) 
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'benign' & 
                                 df.compare$Прогноз == 'benign', ])
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'malignant' & 
                                 df.compare$Прогноз == 'benign', ])
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'benign' & 
                                 df.compare$Прогноз == 'malignant', ])
  
  # считаем характеристики
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1])
  y2 <- c(y2, TPR)
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2])
  x2 <- c(x2, 1 - SPC)
}

# кривые
lines(x2,y2,col="red",lwd = 2)

legend('bottomright', lty = c(1, 1), 
       col = c('blue', 'red'), 
       legend = c('LDA', 'Логистическая регрессия'), 
       lwd = rep(2, 2))

# ROC-кривые на тестовой выборке: ======================================================================================================================

Факт <- df.nt$Class
# Логистическая регрессия
# тестовая выборка:
# прогноз: вероятности принадлежности классу malignant
p.logit.nt <- predict(model.logit, df.nt, 
                      type = 'response')
p.logit.nt
Прогноз <- factor(ifelse(p.logit.nt > 0.5,
                         1, 2),
                  levels = c(1, 2),
                  labels = c('malignant', 'benign'))
Прогноз
head(cbind(Факт, p.logit.nt, Прогноз))
# матрица неточностей

table(Факт, Прогноз)


# LDA 
# тестовая выборка:
# прогноз: вероятности принадлежности классу 'malignant'
p.lda.nt <- predict(model.lda, df.nt, type = 'response')
head(p.lda.nt$posterior)
Прогноз <- factor(ifelse(p.lda.nt$posterior[, 'malignant'] > 0.5,
                         1, 2),
                  levels = c(1, 2),
                  labels = c('malignant', 'benign'))

Прогноз
# матрица неточностей
table(Факт, Прогноз)


# ROC-кривая для LDA ===========================================================

# считаем 1-SPC и TPR для всех вариантов границы отсечения
x <- NULL    # для (1 - SPC)
y <- NULL    # для TPR

# заготовка под матрицу неточностей
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2))
rownames(tbl) <- c('fact.malignant', 'fact.benign')
colnames(tbl) <- c('predict.malignant', 'predict.benign')
tbl

# цикл по вероятностям отсечения
for (p in seq(0, 1, length = 500)){
  # прогноз
  Прогноз <- factor(ifelse(p.lda.nt$posterior[, 'malignant'] > p,
                           1, 2),
                    levels = c(1, 2),
                    labels = c('malignant', 'benign'))
  
  # фрейм со сравнением факта и прогноза
  df.compare <- data.frame(Факт = Факт, 
                               Прогноз = Прогноз)
  
  # заполняем матрицу неточностей
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'malignant' & 
                                 df.compare$Прогноз == 'malignant', ]) 
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'benign' & 
                                 df.compare$Прогноз == 'benign', ])
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'malignant' & 
                                 df.compare$Прогноз == 'benign', ])
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'benign' & 
                                 df.compare$Прогноз == 'malignant', ])
  
  # считаем характеристики
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1])
  y <- c(y, TPR)
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2])
  x <- c(x, 1 - SPC)
}

# строим ROC-кривую
par(mar = c(5, 5, 1, 1))
# кривая
plot(x, y, 
     type = 'l', col = 'blue', lwd = 3,
     xlab = '(1 - SPC)', ylab = 'TPR', 
     xlim = c(0, 1), ylim = c(0, 1))
# прямая случайного классификатора
abline(a = 0, b = 1, lty = 3, lwd = 2)


# ROC-кривая для логистической регрессии ===========================================================

# считаем 1-SPC и TPR для всех вариантов границы отсечения
x2 <- NULL    # для (1 - SPC)
y2 <- NULL    # для TPR

# заготовка под матрицу неточностей
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2))
rownames(tbl) <- c('fact.malignant', 'fact.benign')
colnames(tbl) <- c('predict.malignant', 'predict.benign')
tbl

# цикл по вероятностям отсечения
for (p in seq(0, 1, length = 500)){
  # прогноз
  Прогноз <- factor(ifelse(p.logit.nt > p,
                           1, 2),
                    levels = c(1, 2),
                    labels = c('malignant', 'benign'))
  
  # фрейм со сравнением факта и прогноза
  df.compare <- data.frame(Факт = Факт, 
                               Прогноз = Прогноз)
  
  # заполняем матрицу неточностей
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'malignant' & 
                                 df.compare$Прогноз == 'malignant', ]) 
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'benign' & 
                                 df.compare$Прогноз == 'benign', ])
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'malignant' & 
                                 df.compare$Прогноз == 'benign', ])
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'benign' & 
                                 df.compare$Прогноз == 'malignant', ])
  
  # считаем характеристики
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1])
  y2 <- c(y2, TPR)
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2])
  x2 <- c(x2, 1 - SPC)
}

# кривые
lines(x2,y2,col="red",lwd = 2)

legend('bottomright', lty = c(1, 1), 
       col = c('blue', 'red'), 
       legend = c('LDA', 'Логистическая регрессия'), 
       lwd = rep(2, 2))

# Вывод ===============================================================

# Модель LDA лучше, тк прощадь под ROC кривой (AUC) на тестовой выборке у неё больше, чем у модели логистической регрессии.
