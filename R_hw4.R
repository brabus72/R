## Домашняя работа №4


# генерируем выборку, 50 значений
set.seed(4)
samp<-rnorm(50,7,3)
samp

# проверка выборка на нормальность
qqnorm(samp)
qqline(samp)

# находим z
z<-qnorm(0.975)
z
SE<-3/sqrt(50)
SE # стандартная ошибка среднего
lolv<-mean(samp)- z*SE
uplv<-mean(samp)+ z*SE
CI<- c(lolv, uplv)
CI
7>= CI[1] & 7 <= CI[2]

# t-критерий

bigpar(1,3)
set.seed(3)
plot(7 +c(-4,4),c(1,1),type='n',
     xlab = 'mu', ylab = 'доверительный интервал', 
     ylim = c(1,100), main = 'N=10')  # for N=10
abline(v=7, col='brown', lwd=2)
for (i in 1:100) {
  sam <- rnorm(10,7,3)
  SE <- sd(sam)/sqrt(10)
  CI <-c(mean(sam)-z*SE, mean(sam)+z*SE)
  catch <-
    7>=CI[1]&7<=CI[2]
  7 >= CI[1] & 7 <= CI[2]
  color <- ifelse(catch,'blue','red')
  lines(CI, c(i,i), col=color, lwd=2)
}

set.seed(3)
plot(7 +c(-3,3),c(1,1),type='n',
     xlab = 'mu', ylab = 'доверительный интервал', 
     ylim = c(1,100), main = 'N=50')  # for N=50
abline(v=7, col='brown', lwd=2)
for (i in 1:100) {
  sam <- rnorm(50,7,3)
  SE <- sd(sam)/sqrt(50)
  CI <-c(mean(sam)-z*SE, mean(sam)+z*SE)
  catch <-
    7>=CI[1]&7<=CI[2]
  7 >= CI[1] & 7 <= CI[2]
  color <- ifelse(catch,'blue','red')
  lines(CI, c(i,i), col=color, lwd=2)
}
  
set.seed(3)
plot(7 +c(-3,3),c(1,1),type='n',
     xlab = 'mu', ylab = 'доверительный интервал', 
     ylim = c(1,100), main = 'N=100')  # for N=100
abline(v=7, col='brown', lwd=2)
for (i in 1:100) {
  sam <- rnorm(100,7,3)
  SE <- sd(sam)/sqrt(100)
  CI <-c(mean(sam)-z*SE, mean(sam)+z*SE)
  catch <-
    7>=CI[1]&7<=CI[2]
  7 >= CI[1] & 7 <= CI[2]
  color <- ifelse(catch,'blue','red')
  lines(CI, c(i,i), col=color, lwd=2)
}


## распределение Стьюдента-t

library(rafalib)

set.seed(3)

sam<-rnorm(10,7,3)
mypar(1,1)
qqnorm(sam, col=1, lwd=2)
qqline(sam, col='red')

bigpar(1,2)
t <- qt(0.975,9)
set.seed(3)
plot(7 +c(-4,4),c(1,1),type='n',
     xlab = 'mu', ylab = 'доверительный интервал', 
     ylim = c(1,100), main = 't-критерий')  # for N=10
abline(v=7, col='brown', lwd=2)
for (i in 1:100) {
  sam <- rnorm(10,7,3)
  SE <- sd(sam)/sqrt(10)
  CI <-c(mean(sam)-t*SE, mean(sam)+t*SE)
  catch <-
    7>=CI[1]&7<=CI[2]
  7 >= CI[1] & 7 <= CI[2]
  color <- ifelse(catch,'blue','red')
  lines(CI, c(i,i), col=color, lwd=2)
}

set.seed(3)
plot(7 +c(-4,4),c(1,1),type='n',
     xlab = 'mu', ylab = 'доверительный интервал', 
     ylim = c(1,100), main = 'z-критерий')  # for N=10
abline(v=7, col='brown', lwd=2)
for (i in 1:100) {
  sam <- rnorm(10,7,3)
  SE <- sd(sam)/sqrt(10)
  CI <-c(mean(sam)-z*SE, mean(sam)+z*SE)
  catch <-
    7>=CI[1]&7<=CI[2]
  7 >= CI[1] & 7 <= CI[2]
  color <- ifelse(catch,'blue','red')
  lines(CI, c(i,i), col=color, lwd=2)
}

z
c(mean(sam)-z*SE, mean(sam)+z*SE)
c(mean(sam)-t*SE, mean(sam)+t*SE)

library(rio)
library(dplyr)
library(rafalib)

tidy_set <- dat %>% filter((ap_lo<200&ap_lo>20)&(ap_hi<300&ap_hi>40))
head(tidy_set)
head(tidy_set[tidy_set$ap_hi < tidy_set$ap_lo,])

.tidy_set<-tidy_set[tidy_set$ap_hi>tidy_set$ap_lo,]
dim(.tidy_set)
dim(tidy_set)
.women<-.tidy_set$ap_lo[.tidy_set$gender==1]
.men<-.tidy_set$ap_lo[.tidy_set$gender==2]

mypar(1,2)
qqnorm(.men, main = 'мужчины')
qqline(.men)
qqnorm(.women, main = 'женщины')
qqline(.women)

infer<-.tidy_set%>%group_by(gender)%>%summarise(
  mu=mean(ap_lo),
  k=qt(0.975,length(ap_lo)-1),
  se=sd(ap_lo)/sqrt(length(ap_lo)),
  lowlevel=mean(ap_lo)-k*se,
  hilevel=mean(ap_lo)+k*se)
infer

ci_w<-c(infer[1,5], infer[1,6])
ci_w<-as.numeric(c(infer[1,5], infer[1,6]))
ci_w

ci_m<-c(infer[2,5], infer[2,6])
ci_m<-as.numeric(ci_m)
ci_m

# изобразим графически интервальные оценки для мужчин и женщин

plot(mean(.women),
     col=2, 
     lwd=2, 
     xlim = c(0.5,2.5), 
     ylim = c(78,84), 
     ylab='среднее диастолическое давление',
     main = 'интервальная оценка среднего диастолического давления для мужчин и женщин')
interval=c(80.75, 80.92)
lines(x=c(1,1), y=interval, col='red', lwd=3)
points(1.5,mean(.men), col=3, lwd=2)
interval_1=c(82.05, 82.29)
lines(x=c(1.5,1.5), y=interval_1, col='blue', lwd=3)
legend('topleft',c('women', 'men'), fill = c('red', 'blue'))

post

qnorm(0.95)
sig= signif(0.3/sqrt(20),2)
sig
9.0 + 1.645*0.067
(9.11-9.3)/0.067
1-pnorm(-2.835)

install.packages('asbio')
z_test <- power.z.test(sigma = 0.3, n=20, alpha = 0.05, effect = 0.3, test = 'one.tail')
z_test
z_test$power
z_test$n
z_test$power

power.z.test(sigma = 0.3, alpha = 0.05, effect = 0.3, test = 'one.tail')$n
power.z.test(sigma = 0.3, n=6, alpha = 0.05, effect = 0.3, test = 'one.tail')$power

# проведём двусторонний тест
power.z.test(sigma = 0.3, n=20, alpha = 0.05, effect = 0.3, test = 'two.tail', strict = 'TRUE')

# тест из пакета 'BSDA'
(mean(post)-9)*sqrt(20)/0.3

install.packages('BSDA')
library(BSDA)
z.test(post, alternative = 'g',mu=9,sigma.x = 0.3)
z.test(post, alternative = 'two.sided',mu=9,sigma.x = 0.3)

t.test(sample(.men,20),sample(.women,20),var.equal = TRUE)

t.test(sample(.men,20),sample(.women,20),var.equal = FALSE)

# посчитаем статистику d Коэна
library(effsize)
cohen.d(d=.men,.women)

d<-cohen.d(d=.men,.women)$estimate
d

d<-(mean(.men) - mean(.women))/s.pool
d

install.packages('pwr')
library('pwr')
pwr.t2n.test(n1=length(.women), n2=length(.men), d=d, sig.level = 0.05, alternative = 'two.sided')
t.test(.men,.women, alternative = 'two.sided')

((mean(.men) - mean(.women))/mean(.women))/mean(.women)*100 # размер эффекта в %
ci<-t.test(.men,.women, alternative = 'two.sided')$conf.int
(ci/mean(.women))*100 # доверительный интервал для ES

pwr.t2n.test(n1=20, power = 0.8, d=0.8, sig.level = 0.05, alternative = 'two.sided') # объём выборок, чтобы найти сильное различие

t.test(sample(.men,20),sample(.women,35)) # не обнаружили большого эффекта
t.test(sample(.men,20), mu=82)

