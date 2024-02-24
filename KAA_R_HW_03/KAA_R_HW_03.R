# Урок 3. Разведочный анализ данных в R/ ДЗ
install.packages("dplyr")
install.packages("ggplot2")
library("dplyr")
library("ggplot2")

library(httr)
# Скачаем файл "cardio_train.csv"
filename <- "cardio_train.csv"
url <- "https://drive.google.com/uc?authuser=0&id=1qPKIRO3GfGNQK7rUFpZQWQRaNhTjHvJP&export=download"
GET(url, write_disk(filename, overwrite=TRUE))

df = read.csv(filename, sep=";")
head(df, 3)

dim(df)

df <- df %>% mutate(age_years=trunc(age/365))
head(df, 3)

# Чтобы понять, с какими данными мы имеем дело, возьмем такие характеристики из описательной статистики,
# как среднее арифметическое и среднее квадратичное отклонение
cat(mean(df$ap_hi), mean(df$ap_lo))
cat(sd(df$ap_hi), sd(df$ap_lo))

install.packages("rafalib")
library(rafalib)

mypar(1,2)
box_lo <- boxplot(df$ap_lo)
box_hi <- boxplot(df$ap_hi)


# Выбросы

# *Давление, опасное для жизни
# *Верхние границы: 200/140
# *Нижние границы: 70/50

ap_lo_capt <- "Нижнее давление"
ap_hi_capt <- "Верхнее давление"

# Построим боксплоты без учета выбросов, которые скорее всего были ошибкой ввода
mypar(1, 2)
box_lo <- boxplot(df$ap_lo[df$ap_lo<200 & df$ap_lo>20])
title(ap_lo_capt)
box_hi <- boxplot(df$ap_hi[df$ap_hi<300 & df$ap_hi>40])
title(ap_hi_capt)

median(df$ap_lo)

quantile(df$ap_lo, 0.25)

sort(df$ap_lo)[17501:35000]


# Воспользуемся функцией filter(), %>% из пакета dplyr, чтобы подготовить датасет без грубейших ошибок ввода. 
# Присвоим новому набору имя tidy_set с помощью <-

tidy_set <- df %>% filter((ap_lo<200 & ap_lo>20) & (ap_hi<300 & ap_hi>40))
head(tidy_set, 3)


# Сравним размер прежнего набора с размером нового набора с обработанными данными
# Посмотрим значения среднего арифметического и sd для двух переменных ap_lo и ap_hi
dim(tidy_set)

cat(mean(tidy_set$ap_hi), mean(tidy_set$ap_lo))

cat(sd(tidy_set$ap_hi), sd(tidy_set$ap_lo))


# После того, как проанализировали данные с помощью боксплота, взглянем на гистограммы для двух величин.

# Визуализация данных с помощью боксплота дала возможность увидеть все значения переменной и определить грубейшие выбросы (аутлайеры).
# Теперь по гистограмме видно, что распределения стали больше
mypar(2, 2)
hist(df$ap_lo, main=ap_lo_capt)
hist(tidy_set$ap_lo, main=paste(ap_lo_capt, "tidy_set"))
hist(df$ap_hi, main=ap_hi_capt)
hist(tidy_set$ap_hi, main=paste(ap_hi_capt, "tidy_set"))


# Одним из самых распространенных методов проверки нормальности является QQ график
# В основе лежит идея сравнить теоретические квантили с квантилями СВ
qqnorm(tidy_set$ap_lo, main=ap_lo_capt)
qqline(tidy_set$ap_lo, col="red", lwd=2)
qqnorm(tidy_set$ap_hi, main=ap_hi_capt)
qqline(tidy_set$ap_hi, col="red", lwd=2)
abline(h=160, col="green")

# По графикам видно, что верхние значения нижнего артериального давления лежат выше, чем предполагалось нормальным распределением,
# а нижние - ниже
# Для верхнего артериального давления верхние значения лежат слишком высоко

# Для равнения давления у мужчин и женщин, удобно построить боксплоты для каждой подгруппы, используя функцию split()
mypar(1,2)
groupss_lo <- split(tidy_set$ap_lo, tidy_set$gender)
boxplot(groupss_lo)
title(ap_lo_capt)
groupss_hi <- split(tidy_set$ap_hi, tidy_set$gender)
boxplot(groupss_hi)
title(ap_hi_capt)
# Вывод: распределение в каждой подгруппе и для мужчин и женщин схожи. Можем предположить,что давление от пола не зависит

# Вернемся к исследованию СВ без учета пола пациента
# Построим график плотности распределения для нижнего и верхнего артериального давления использую функцию density() и plot()
plot(density(tidy_set$ap_lo), col=2, lwd=2, main=ap_lo_capt)
plot(density(tidy_set$ap_hi), col=3, lwd=2, main=ap_hi_capt)
# Мы ожидали увидеть нормальное распределение с одним пиком, но их больше!

# Разберемся в причине этих множественных пиков:
plot(density(tidy_set$ap_lo), col=1, lwd=2, main=ap_lo_capt)
abline(v=70, col="red")
abline(v=60, col="red")
abline(v=80, col="red")
sort_lo <- sort(tidy_set$ap_lo)
cut_1 <- sort_lo[sort_lo>65 & sort_lo<75]
cut_1
# Вывод: пики образуются за счет округления показателей давления или отсутствия электронных приборов, 
# которые дают большую точность измерения, чем механические

# Приблизительно так должно выглядеть истинное нормальное распределение давления. Чтобы это изобразить, используем аргумент adjust
plot(density(tidy_set$ap_lo, adjust=10), col=1, lwd=2, main=ap_lo_capt)

# Чтобы произвести наглядное сравнение двух случайных величин, удобно их нанести на один график. 
# Они будут иметь единый общий масштаб и шкалы


# Сравним распределение нижнего и верхнего артериального давления
# С помощью функции legend() обозначим, каким СВ соответствуют графики

plot(density(tidy_set$ap_lo, adjust=10), col=1, lwd=2, main=ap_lo_capt)
lines(density(tidy_set$ap_hi, adjust=10), col=3, lwd=2, lty=2)
legend("topright", c("ap_lo", "ap_hi"), col=c(2, 3), lty=c(1, 2))


# Скаттерплот
plot(
  tidy_set$ap_lo,
  tidy_set$ap_hi,
  col=2,
  main=paste(
    "correlation=",
    signif(cor(tidy_set$ap_lo, tidy_set$ap_hi), 2)
  )
)

# На предыдущих графиках мы видим средние значения, медианы, дисперсии, но не можем проследить взаимосвязь двух величин
# Скаттерплот позволяет нам это увидеть. Скаттерплот также позволяет проследить, есть ли разделение на группы по каким-либо признакам

# По данному графику можно проследить тренд: с ростом нижнего артериального давления растет и нижнее артериальное давление
plot(
  tidy_set$ap_hi,
  tidy_set$ap_lo,
  pch=21,
  bg=as.numeric(factor(tidy_set$gender)),
  xlab=ap_hi_capt,
  ylab=ap_lo_capt
)
legend(
  "topright",
  levels(factor(tidy_set$gender)),
  col=seq(along=levels(factor(tidy_set$gender))),
  pch=15,
  cex=1.5
)

plot(
  tidy_set$weight,
  tidy_set$height,
  pch=21,
  bg=as.numeric(factor(tidy_set$gender)),
  xlab="Вес",
  ylab="Рост"
)
legend(
  "topright",
  levels(factor(tidy_set$gender)),
  col=seq(along=levels(factor(tidy_set$gender))),
  pch=15,
  cex=1.5
)
# Вывод: показатели артериального давления не смогут объяснить половую принадлежность пациента,
# в то время, как рост и вес хорошо разделены по полу

# Скаттерплот также можно построить и для датафрейма
# Это позволит определить, какие признаки несут больше всего информации
mini_set <- tidy_set[,3:5]
head(mini_set)

plot(mini_set, pch=21, bg=mini_set$gender)

mini_set_1 <- tidy_set[,c(3,6,7)]
head(mini_set_1)

plot(mini_set_1, pch=21, bg=mini_set_1$gender)
