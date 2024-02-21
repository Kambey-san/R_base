# Урок 2. Обработка данных для анализа/ ДЗ

# Условие if-else
library(cluster)
head(votes.repub)
colMeans(votes.repub)

if (mean(votes.repub[,30], na.rm = TRUE) > 60) {
  print("Республиканцы набрали высокий процент голосов")
} else {
  print("Республиканцы набрали < 60% голосов")
}

if (mean(votes.repub[,1], na.rm = TRUE) > 60) {
  print("Республиканцы набрали высокий процент голосов")
} else {
  print("Республиканцы набрали < 60% голосов")
}


# Есть похожая функция ifelse()
x <- c(1, 0, 0, 0, 1, 0)
ifelse(x != 0, "Yes", "No")

ifelse(
  colMeans(votes.repub, na.rm = TRUE) > 60,
  "Республиканцы набрали высокий процент голосов",
  "Республиканцы набрали < 60% голосов"
)


# Пишем собственную функцию¶
# Для написания функции воспользуемся набором, содержащим данные о продажах мячей в первые дни 2018 года

# *date – год-месяц-день
# *ball – теннисные мячи продавались по одному, а также распакованными по 3 и 5 мячей
# *price – цены

date_list <- c(rep("2018-01-01", 4), rep("2018-01-05", 6))
ball_list <- c(rep(1, 3), rep(3, 4), rep(5, 3))
price_list <- c(rep(90, 3), rep(150, 4), rep(390, 3))

df <- data.frame("date"=date_list, "ball"=ball_list, "price"=price_list)
head(df, 10)

# Сохраним датасет в файл
write.csv(df, "df.csv", row.names=FALSE)

# Загрузим датасет из файла
df <- read.csv("df.csv", header=TRUE, sep=",")
head(df, 10)

str(df)

unique(df$ball)

unique(df$price)

# Для работы с датами загружаем пакет "lubridate"
install.packages("lubridate")
library(lubridate)

class(df$date)

dayn = ymd(df$date)
dayn

# Напишем функцию, которая считает, на какую сумму продано мячей за определенный день
# Поскольку дни в месяцах совпадают, нам нужна функция с аргументами, например, месяц и день
sum_balls <- function(dataset, m, d) {
  sum(dataset$price[month(dataset$date) == m & day(dataset$date) == d])
}

sum_balls(df, 1, 1)

# Используем цикл for, чтобы вывести сумму за определенный день
for (i in 1:5) {
  print(sum_balls(df, 1, i))
}


# Функции lapply() и sapply()
head(lapply(votes.repub, sum))
sapply(votes.repub, sum)


# tapply()
l <- function(x) {
  diff(range(x))
}

tapply(Orange$circumference, Orange$Tree, l)

search()

# С помощью :: показываем R, из какого пакета нам нужна функция
lubridate::dmy("01/01/1970")


# Метод Монте-Карло
runs <- 100000
# runif samples from a uniform distribution
xs <- runif(runs, min=-0.5, max=0.5)
ys <- runif(runs, min=-0.5, max=0.5)
in.circle <- xs^2 + ys^2 <= 0.5^2
mc.pi <- (sum(in.circle)/runs)*4
plot(
  xs, ys, pch='.',
  col=ifelse(in.circle, "blue", "grey"),
  xlab='', ylab='', asp=1,
  main=paste("MC Approximation of Pi =", mc.pi)
)

# Пакет "dplyr" и оператор pipe
install.packages("dplyr")
library(dplyr)

iris %>% dim

head(iris %>% filter(Species=="versicolor"))
head(iris %>% filter(Species=="versicolor") %>% select(Petal.Length))

vc <- as.numeric(iris %>% filter(Species=="versicolor") %>% select(Petal.Length) %>% unlist)
vc

# найдем длину лепестков , только для "virginica". И с помощью оператора %in% посмотрим, 
# сколько совпадений в длине лепестков двух разных видов цветка
vg <- as.numeric(iris %>% filter (Species=="virginica") %>% select(Petal.Length) %>% unlist)
vg

vc %in% vg

sum(vc %in% vg)

# Сохраняем набор данных в формате CSV
a <- c(1, 2, 3)
b <- c(0, 0, 0)

nm <- c("1_row", "2_row", "3_row")

df <- data.frame(a, b)
df

df["rownames"] = nm
df

df <- df[c("rownames", "a", "b")]
df

filename <- "new_dataframe.csv"
write.csv(df, file=filename)


# Аргументы функции read.csv()
read.csv(filename)
read.csv(filename, sep=";")
read.csv(filename, sep=",", nrows=5, row.names=1)
read.csv(filename, sep=",", nrows=5, header=FALSE)


# Работа с Excel файлами
install.packages("readxl")
install.packages("httr")
library(readxl)

library(httr)
# Скачаем файл "Financial Sample.xlsx"
filename <- "Financial Sample.xlsx"
url <- "https://go.microsoft.com/fwlink/?LinkID=521962"
download_path <- "Financial Sample.xlsx"
GET(url, write_disk(filename, overwrite=TRUE))

head(readxl::read_excel(filename))


# Импорт данных, взятых из интернет-ресурса и их обработка
install.packages("rvest")
library(rvest)

url1 <- paste0("https://cdas.cancer.gov/datasets/nlst/")
url1

page <- read_html(url1)
page

tabl <- page %>% html_nodes("table")
tabl

tabl <- tabl[[1]] %>% html_table
head(tabl)

tabl <- tabl %>% setNames(c("data", "description"))
head(tabl, 3)

# Рассмотрим отдельный столбец датафрейма
head(tabl["description"])

install.packages("stringr")
library(stringr)

desc_new <- stringr::str_replace(tabl$description, "\\d*.", "")
head(desc_new)

desc_new <- gsub("\n", "", desc_new)
desc_new <- trimws(desc_new)
head(desc_new)

# Сохраним обработанные данные в наш датафрейм
tabl["description"] = desc_new
dplyr::tibble(tabl)

