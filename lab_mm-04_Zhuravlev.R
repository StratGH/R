# Математическое моделирование: Практика 4

library('GGally')
library('MASS')
library('mlbench')

my.seed <- 12345
train.percent <- 0.85

# Данные BreastCancer ---------------------------------------------------------------
?BreastCancer
data(BreastCancer)
head(BreastCancer)
str(BreastCancer)

# графики разброса
ggpairs(BreastCancer[,-1])

#сохранение без NA:
bc <- BreastCancer[-is.na(BreastCancer$Bare.nuclei),-1]


#ggpairs(bc[,c(1:3,10)])
#ggpairs(bc[,c(4:7,10)])
#ggpairs(bc[,c(8:10)])
#?ggpairs

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
#?glm
summary(model.logit)
