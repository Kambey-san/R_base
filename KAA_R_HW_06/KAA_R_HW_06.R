'Урок 6. Статистический анализ в R. Anova. Регрессионный анализ/ ДЗ'

install.packages("rafalib")
install.packages("effsize")
install.packages("BSDA")
install.packages("pwr")
library(httr)
library(dplyr)
library(rafalib)
library(effsize)
library(BSDA)
library(pwr)

# Скачаем файл "cardio_train.csv"
filename <- "cardio_train.csv"
url <- "https://drive.google.com/uc?authuser=0&id=1qPKIRO3GfGNQK7rUFpZQWQRaNhTjHvJP&export=download"
GET(url, write_disk(filename, overwrite=TRUE))

df <- read.csv(filename, sep=";")
head(df, 3)

df$age_years <- trunc(df$age / 365.25)

tidy_set <- df %>% filter((ap_lo<200 & ap_lo>20) & (ap_hi<300 & ap_hi>40))
head(tidy_set)

tidy_set <- tidy_set[tidy_set$ap_hi > tidy_set$ap_lo,]
head(tidy_set)

dim(tidy_set)

set.seed(1)
ind <- sample(seq(1, nrow(tidy_set)), 100)
ind

ts <- tidy_set[ind,]
head(ts)

dim(ts)

plot(ts$ap_lo, ts$ap_hi, xlim=c(0,120), ylim=c(0,200), col="red", lwd=2)
# вывод: прослеживается линейная зависимость между нижним и верхним давлением пациента


# Функции lm() и predict()
# Построим парную линейную регрессию, в качестве независимой переменной возьмем нижнее давление пациента
fitm <- lm(ts$ap_hi ~ ts$ap_lo)
fitm

hi_hat <- 32.961 + 1.121 * ts$ap_lo
hi_hat


# Функция predict() упрощает вычисления оценочного параметра, ее особенно удобно использовать, когда имеем дело не с одним Х
as.numeric(predict(fitm, ts))

# Причина различий оценочных значений y кроется в округлении вычислений

signif(fitm$coefficients, 10)

32.96074004 + 1.121065203*ts$ap_lo

plot(seq(1:length(ts$ap_hi)), ts$ap_hi, col="red", type="l")
lines(seq(1, length(hi_hat)), hi_hat, col="blue", type="l")
legend("topleft", c("ts$ap_hi", "hi_hat"), col=c(2,3), lty=c(1,1))
summary(fitm)


'Residuals:
Одним из важных условий для построения модели линейной регрессии является предположение,
что ошибки следуют нормальному распределению'
qqnorm(fitm$residuals)
qqline(fitm$residuals)


'Coefficients:
С помощью t-статистики Стьюдента можно проверить значимость коэффициентов построенной модели'
summary(fitm)
'R дает значения t-статистики, p-value и с помощью «*» отмечает наиболее значимые коэффициенты.
В строке Signif.codes приведена расшифровка обозначений: например, «***» соответствуют p-value, лежащей между нулем и 0.001'


'Residual Standard Error:
RSE вычисляется следующим образом'
rse <- sqrt(sum(residuals(fitm)^2) / fitm$df.residual)
rse


'R-squared:
Коэффициент детерминации. Показывает, какую часть изменчивости величины y описала построенная модель.
Посчитать эту величину можно следующим образом:'
Rs <- cor(ts$ap_hi, ts$ap_lo)^2
Rs
# С увеличением числа предикторов коэффициент детерминации растет


'Adjusted R-squared:
Решает эту проблему. Если добавленные новые предикторы не вносят весомого вклада в модель,
то этот параметр будет падать, в противном случае - расти'
R_adj <- 1-((1-Rs)*((100-1)/(100-1-1)))
R_adj


'F-statistic:
Позволяет оценить значимость построенной модели в целом'
tsn <- ts[,-c(1,2)]
head(tsn)

fit <- lm(tsn$ap_hi~., data=tsn)
summary(fit)



'ANOVA
Дисперсионный анализ (ANOVA - analysis of variance) используется, 
когда мы хотим выяснить влияние одного или нескольких факторов (качественных переменных) на количественную переменную (отклик).

Используем, чтобы избежать множественных сравнений, которые приводят к росту вероятности ошибки первого рода.

При использовании поправок на множественные сравнения, растет вероятность ошибки второго рода с ужесточением уровня значимости.'

plot(
  tidy_set$gluc, tidy_set$ap_hi, cex=1, col=tidy_set$gluc,
  xlab="уровень глюкозы", ylab="верхнее давление", xlim=c(0.7, 3.5)
)


'Сбалансированные и несбалансированные данные
Если данные имеют разное количество наблюдений в группе, то мы имеем дело с несбалансированными данными'
# работаем с выборкой
set.seed(1)
ind <- sample(seq(1, nrow(tidy_set)), 100)
ind

ts <- tidy_set[ind,]
head(ts)

table(ts$gluc)

table(ts$gender, ts$gluc)

#Вывод: стараемся делать одинаковые выборки


'Задача: проведем исследование влияния 2-х факторов: пол и уровень глюкозы на верхнее давление
1. Берем выборки. Соблюдаем условие случайности и независимости'

head(tidy_set)

set.seed(1)
# g1 - gluc == 1; s1 - gender == 1
# g2 - gluc == 2; s2 - gender == 2
g1_s1 <- sample(tidy_set$ap_hi[tidy_set$gluc==1 & tidy_set$gender==1], 20)
g1_s1
g1_s2 <- sample(tidy_set$ap_hi[tidy_set$gluc==1 & tidy_set$gender==2], 20)
g1_s2

g2_s1 <- sample(tidy_set$ap_hi[tidy_set$gluc==2 & tidy_set$gender==1], 20)
g2_s1
g2_s2 <- sample(tidy_set$ap_hi[tidy_set$gluc==2 & tidy_set$gender==2], 20)
g2_s2

g3_s1 <- sample(tidy_set$ap_hi[tidy_set$gluc==3 & tidy_set$gender==1], 20)
g3_s1
g3_s2 <- sample(tidy_set$ap_hi[tidy_set$gluc==3 & tidy_set$gender==2], 20)
g3_s2


# строим датафрейм

# новый вектор "gender_new" и "gluc_new"
gender_new <- c(rep(1,20), rep(2,20), rep(1,20), rep(2,20), rep(1,20), rep(2,20))
gender_new

gluc_new <- c(rep(1,40), rep(2,40), rep(3,40))
gluc_new

sam_s <-c(g1_s1, g1_s2, g2_s1, g2_s2, g3_s1, g3_s2)
sam_s

anovaframe <- data.frame(sam_s, gender_new, gluc_new)  # соблюдаются случайность и независимость
head(anovaframe, 25)

table(anovaframe$gender_new, anovaframe$gluc_new)


'2) Разведочный анализ
2.1) Для наглядности представим данные графически'
boxplot(
  sam_s~gender_new, data=anovaframe, boxwex=0.15, at=1:2-0.3,
  subset=gluc_new==1, col="green", main="EDA ANOVA",
  xlab="пол", ylab="верхнее давление",
  xlim=c(0.5, 2.5), ylim=c(0, 200)
)
boxplot(
  sam_s~gender_new, data=anovaframe, add=TRUE,
  boxwex=0.2, at=1:2-0.1, subset=gluc_new==2, col="orange"
)
boxplot(
  sam_s~gender_new, data=anovaframe, add=TRUE,
  boxwex=0.2, at=1:2+0.15, subset=gluc_new==3, col="brown"
)
legend(
  "bottomleft",
  c("gluc=1", "gluc=2", "gluc=3"),
  fill=c("green", "orange", "brown")
)
# На графике есть неоднородность дисперсий


'2.2. Помимо визуальной оценки однородности дисперсий (п. 2.1) проверим гомоскедастичность с помощью специальных критериев:'

# Критерий Бартлетта
bartlett.test(list(g1_s1, g1_s2, g2_s1, g2_s2, g3_s1, g3_s2))
# Принимаем нулевую гипотезу на уровне значимости 0.05.
# Статистически значимых различий между дисперсиями выборок нет.
# Все условия соблюдены. Теперь можно приступать непосредственно к самому дисперсионному анализу

'2.3) Проверим предположение о нормальности распределений с помощью qq-графика'
mypar(2,3)
qqnorm(g1_s1)
qqline(g1_s1)
qqnorm(g1_s2)
qqline(g1_s2)
qqnorm(g2_s1)
qqline(g2_s1)
qqnorm(g2_s2)
qqline(g2_s2)
qqnorm(g3_s1)
qqline(g3_s1)
qqnorm(g3_s2)
qqline(g3_s2)
# Вывод: отклонения небольшие. Это ок, т.к. одинаковые объемы выборок!


# Сбалансированные данные НЕ влияют на порядок включения факторов в модель
summary(aov(sam_s~gender_new + gluc_new + gender_new:gluc_new, data=anovaframe))

summary(aov(sam_s~gluc_new + gender_new + gluc_new:gender_new, data=anovaframe))

# Чтобы не прописывать эффект взаимодействия, факторы в модели указывают с помощью знака «*»

summary(aov(sam_s~gender_new * gluc_new, data=anovaframe))

'Резюмируем: взаимодействие факторов «уровень глюкозы» и «пол»,
а также сами факторы не оказывают значимого эффекта на давление пациента на уровне значимости 0.05.'