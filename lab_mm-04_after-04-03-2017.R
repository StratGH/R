# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# Математическое моделирование: Практика 4

library('GGally')
library('MASS')
library('mlbench')

my.seed <- 12345
train.percent <- 0.85

# Данные Default ---------------------------------------------------------------
?BreastCancer
data(BreastCancer)
head(BreastCancer)
str(BreastCancer)

# графики разброса
ggpairs(BreastCancer[,-1])
bc <- BreastCancer[-is.na(BreastCancer$Bare.nuclei),-1]
ggpairs(bc[,c(1:3,10)])
ggpairs(bc[,c(4:7,10)])
ggpairs(bc[,c(8:10)])
?ggpairs

#bc[,1:9] <- as.character(bc[,1:9])

# обучающая выборка
set.seed(my.seed)
inTrain <- sample(seq_along(bc$Class),
                  nrow(bc) * train.percent)
df <- bc[inTrain,]
head(df)

# фактические значения на обучающей выборке
Факт <- df$Class

# Логистическая регрессия ======================================================
model.logit <- glm(Class ~ ., data = df,
                   family = 'binomial')
?glm
summary(model.logit)
# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.logit <- predict(model.logit, df, 
                   type = 'response')
p.logit
Прогноз <- factor(ifelse(p.logit > 0.5,
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))
Прогноз
head(cbind(Факт, p.logit, Прогноз))

# матрица неточностей
table(Факт, Прогноз)

# LDA ==========================================================================
model.lda <- lda(default ~ ., data = df)
model.lda

# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.lda <- predict(model.lda, df, type = 'response')
head(p.lda$posterior)
Прогноз <- factor(ifelse(p.lda$posterior[, 'Yes'] > 0.5,
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))
Прогноз
# матрица неточностей
table(Факт, Прогноз)

# QDA ==========================================================================
model.qda <- qda(default ~ ., data = df)
model.qda

# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.qda <- predict(model.qda, df, type = 'response')
Прогноз <- factor(ifelse(p.qda$posterior[, 'Yes'] > 0.5,
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))

# матрица неточностей
table(Факт, Прогноз)

# ROC-кривая для LDA ===========================================================

# считаем 1-SPC и TPR для всех вариантов границы отсечения
x <- NULL    # для (1 - SPC)
y <- NULL    # для TPR

# заготовка под матрицу неточностей
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2))
rownames(tbl) <- c('fact.No', 'fact.Yes')
colnames(tbl) <- c('predict.No', 'predict.Yes')
tbl

# цикл по вероятностям отсечения
for (p in seq(0, 1, length = 500)){
    # прогноз
    Прогноз <- factor(ifelse(p.lda$posterior[, 'Yes'] > p,
                             2, 1),
                      levels = c(1, 2),
                      labels = c('No', 'Yes'))
    
    # фрейм со сравнением факта и прогноза
    df.compare <- data.frame(Факт = Факт, 
                                 Прогноз = Прогноз)
    
    # заполняем матрицу неточностей
    tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'No' & 
                                   df.compare$Прогноз == 'No', ]) 
    tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'Yes' & 
                                   df.compare$Прогноз == 'Yes', ])
    tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'No' & 
                                   df.compare$Прогноз == 'Yes', ])
    tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'Yes' & 
                                   df.compare$Прогноз == 'No', ])
    
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
