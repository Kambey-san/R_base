# Урок 1. Начало работы в R/ ДЗ

getwd()

x <- 10 # Создаем переменную X
x

x.1 <- 17
x.1

# удаление переменной x.1
rm(x.1)
x.1    #Error: object 'x.1' not found

# Основные математические функции «+» , «-», «:», « * », степень «^»,
# квадратный корень sqrt(), log2(), factorial(), exp()
sqrt(25)
log2(256)
log10(1000)
exp(1)
log(exp(1))
factorial(5)

help(factorial)


# Множества и действия с ними
a <- c(0, 1, 2, 3)
a

b <- c(rep(2, time=3))
b

b.1 <- rep(2, 4)
b.1

rep(c(0, 2), time=2)

rep(c(0, 2), each=2)

a + b.1

seq(2, 8, by=2)  # четные

seq(1, 9, by=2)  # нечетные

seq(1, 10, length.out=5)


rnorm(n=50, mean=0, sd=1)  # задаем нормальное распределение


rpois(100, 10)  # распределение Пуассона


rbinom(100, 10, 0.5)  # биномиальное распределение


# Текстовый вектор

letters

LETTERS

paste(letters, set="_", seq(1, 26))

?ISOdate
format(ISOdate(2019, 9, 1:30), "%d")

format(ISOdate(2019, 9, 1:30), "%b")

paste(format(ISOdate(2019, 9, 1:30), "%d"), set="_", rep("sep", 30))


# Прочие основные и востребованные функции

a <- c(rep(1, 10))
a
class(a)

class(letters)

class(2 != 0)

class(factorial)

d <- c(rep("a", 4), rep("b", 6), rep("c", 2))
sample(d)

d.table <- table(d)
d.table

r <- seq(1, 10)
r

sum(r)

r_mean <- sum(r) / length(r)
r_mean

mean(r)


# Функции set.seed(), sort()

set.seed(42)

popul <- rnorm(100)
popul

popul <- round(popul, 3)
popul

sort(popul)

sort(popul, decreasing=TRUE)


# sample(), data.frame(), head(), order(), $

set.seed(42)
lets <- sample(letters, 100, replace=TRUE)
lets

df <- data.frame(lets, popul)
head(df, 10)

# отсортируем строки датафрейма по возрастанию чисел
ind <- order(df$popul)
ind

df_new <- df[ind,]
head(df_new, 10)


# Другие действия с векторами

g <- seq(31, 45)
g

length(g)

g[1]

g[c(1, 5, 15)]

g[-c(1, 5, 15)]

g[1:5]

h <- 1:15

g + h

g * 2


# Логические функции и операторы

5 > 6

61 < 100

2 != 3

2 == 2

6 > 7 | 8 > 9

6 > 7 & 8 > 9

((TRUE == FALSE) & (1 == 1)) & 100 == 100

((TRUE == FALSE) & (1 == 1)) | 100 == 100


# Построим свой dataframe

weight <- c(78, 56, 67, 48, 69, 90)
height <- c(170, 160, 165, 159, 170, 185)
sex <- (c(rep("F",3),rep("M" ,3)))
sex

df_1 <- data.frame(weight, height, sex)
str(df_1)

df_2 <- data.frame(weight, height, sex, stringsAsFactors=TRUE)
str(df_2)

factor(sex)


# Стоим матрицу

m <- 1:30
m

dim(m) <- c(10, 3)
m

class(m)

# другой способ
y <- 1:50
m <- matrix(y, 10, 5)
m

rownames(m)

colnames(m)

names <- LETTERS[1:10]
rownames(m) <- names
colnames(m) <- paste("day", 1:5)

m


# Датасеты

data()  # Информация о встроенных датасетах

# Загрузим датасет с ирисами
head(iris, 10)

sum(is.na(iris))

head(is.na(iris), 10)

dim(iris)

str(iris)

unique(iris[,5])

levels(iris$Species)

head(iris[iris$Sepal.Width>3.0,])

head(iris[iris$Sepal.Width > 3.0 & iris$Petal.Width > 1.4,])

percentage <- prop.table(table(iris$Species)) * 100
cbind(freq=table(iris$Species), percentage=percentage)

summary(iris)


# Разделим датасет на вектор x и целевую переменную y

x <- iris[,1:4]
y <- iris[,5]

par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}          

plot(y)

# download packages
# C:\Users\Pc\AppData\Local\Temp\RtmpkHKOrR\downloaded_packages
install.packages("ellipse")
install.packages("caret")

library("caret")

featurePlot(x=x, y=y, plot="ellipse")


# Обучим модельку

install.packages("randomForest")

data_split <- createDataPartition(iris$Species, p=0.8, list=FALSE)

test_data <- iris[-data_split,]  # Оставляем 20% датасета для тестирования
train_data <- iris[data_split,]  # На остальных 80% будем обусать модель

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

set.seed(7)
fit.rf <- train(Species~., data=train_data, method="rf", metric=metric, trControl=control)

set.seed(7)
fit.rf <- train(Species~., data=train_data, method="rf", metric=metric, trControl=control)

fit.rf$resample

predictions <- predict(fit.rf, test_data)
confusionMatrix(predictions, test_data$Species)
