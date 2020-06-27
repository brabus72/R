## Домашняя работа №3

install.packages('rafalib')

library(rio)
library(dplyr)
library(rafalib)

dat<-import('cardio_train.csv')
dim(dat)
str(dat)

?mutate

dat$age/365
trunc(dat$age/365) # отсекает дробную часть
dat<-dat %>% mutate(age_years=(trunc(dat$age/365)))


# добавили столбец 'age_years'

head(dat)

mypar(1,2)
hist(log(dat$ap_hi))
hist(log(dat$ap_lo))

mean(dat$ap_hi)
sd(dat$ap_hi)

mean(dat$ap_lo)
sd(dat$ap_lo)

box_lo<-boxplot(dat$ap_lo)
box_lo<-boxplot(dat$ap_hi)


# принимаем решение, что делать с выбросами

box_lo<-boxplot(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])
title('Нижнее давление')
box_lo<-boxplot(dat$ap_lo[dat$ap_lo<300 & dat$ap_lo>40])
title('Верхнее давление')

min(dat$ap_hi[dat$ap_hi<300 & dat$ap_hi>40])
min(dat$ap_hi)

dat$ap_hi[dat$ap_hi<0]


# разбираемся как строится боксплот

median(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])
quantile(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])
quantile(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20],0.25)


# как получаем квартиль

sort(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])
length(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])

tr<-68994*25/100
ind_p25<-trunc(tr)+1
ind_p25

sort(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])[ind_p25]


# разберёмся почему 1й квартиль совпадает с медианой

68994/2
sort(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])[c(34497,34498)]
sort(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])[(17249:34498)]

head(sort(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])[(17249:34498)], 20)

# воспользуемся функцией filter(), %>% из пакета dplyr, чтобы 
# подготовить датасет без грубейших ошибок ввода

tidy_set<-dat %>% filter((ap_lo<200 & dat$ap_lo>20) & (ap_hi<300 & dat$ap_hi>40))
head(tidy_set)
# сравним число значений переменных в 1-ом и новом датасэте
dim(tidy_set)
dim(dat)
nrow(dat)
ncol(dat)
# сравним старые и новые стандартные отклонения и среднее арифметическое
# верхнее давление
mean(tidy_set$ap_hi)
mean(dat$ap_hi)
sd(tidy_set$ap_hi)
sd(dat$ap_hi)
# нижнее давление
mean(tidy_set$ap_lo)
mean(dat$ap_lo)
sd(tidy_set$ap_lo)
sd(dat$ap_lo)

# после небольшой обработкиданных взглянем на данные с помощью гистограммы
mypar(2,2)

hist(dat$ap_lo, main = 'нижнее давление', xlab = 'aplo_dat', ylab = 'частота')
hist(tidy_set$ap_lo, main = 'нижнее давление.new', xlab = 'aplo_tidy', ylab = 'частота')

hist(dat$ap_hi, main = 'нижнее давление', xlab = 'aphi_dat', ylab = 'частота')
hist(tidy_set$ap_hi, main = 'нижнее давление.new', xlab = 'aphi_tidy', ylab = 'частота')

# QQ-график позволяет проверить данные на нормальность
# в основе лежит идея сравнить теоретические квантили с квантилями случайной величины
mypar(1,2)
qqnorm(tidy_set$ap_lo, main = 'нижнее давление.tidy')
qqline(tidy_set$ap_lo, col='red', lwd=2)

mypar(1,2)
qqnorm(tidy_set$ap_hi, main = 'верхнее давление.tidy')
qqline(tidy_set$ap_hi, col='blue', lwd=2)
abline(h=160, col='green')
# вывод: по нижнему давлению, верхние и нижние значения лежат слишком
# слишком высоко и низко соответственно, чем предполагалось нормальным распредлением
# по верхнему давлению

# сравним верхнее и нижнее давление по полу

mypar(1,2)
head(tidy_set)
groupss_lo<-split(tidy_set$ap_lo, tidy_set$gender)
str(groupss_lo)
boxplot(groupss_lo)
title('нижнее давление')

groupss_hi<-split(tidy_set$ap_hi, tidy_set$gender)
str(groupss_hi)
boxplot(groupss_hi)
title('верхнее давление')
