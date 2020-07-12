## Домашняя работа №5

.tidy_set<-tidy_set[tidy_set$ap_hi>tidy_set$ap_lo,]
head(.tidy_set)
dim(.tidy_set)
set.seed(1)

ind<-sample(seq(1,nrow(.tidy_set)),100)
ind

ts<-.tidy_set[ind,]
head(ts)
dim(ts)
plot(ts$ap_lo,ts$ap_hi, xlim = c(0,120), ylim = c(0,200), col='red', lwd=2)


# построим парную линейную регрессию

fitm<-lm(ts$ap_hi~ts$ap_lo)
fitm

hi_hat<-32.961 + 1.121*ts$ap_lo
hi_hat
as.numeric(predict(fitm,ts))

? predict

signif(fitm$coefficients,10)
32.960740 + 1.21065*ts$ap_lo

plot(seq(1:length(ts$ap_hi)),ts$ap_hi, col='red', type='l')
lines(seq(1,length(hi_hat)), hi_hat, col='blue', type='l')
legend('topleft', c('ts$ap_hi', 'hi_hat'), col=c(2,3), lty = c(1,1))
summary(fitm)

qqnorm(fitm$residuals)
qqline(fitm$residuals)

summary(fitm)

rse<-sqrt(sum(residuals(fitm)^2) / fitm$df.residual)
rse
summary(fitm)

Rs<-cor(ts$ap_hi, ts$ap_lo)^2
Rs

R.adj<- 1-((1-Rs)*((100-1)/(100-1-1)))
R.adj
summary(fitm)


# сравним Multiple R-squared

tsn<-ts[,-c(1,2)]
tsn

fit<-lm(tsn$ap_hi ~ . , data = tsn)
fit

summary(lm(tsn$ap_hi~tsn$ap_lo+tsn$weight+tsn$cardio))

plot(.tidy_set$gluc, .tidy_set$ap_hi, cex=1, col=.tidy_set$gluc, 
     xlab = 'уровень глюкозы', ylab = 'верхнее давление', xlim = c(0.7,3.5))
points(rep(0.8,3), c(m1,m2,m3), col=c(1,2,3), lwd=2)


# сбалансированные и несбалансированные данные

set.seed(1)
ind<-sample(seq(1,nrow(.tidy_set)), 100)
ind

ts<-.tidy_set[ind,]
head(ts)

table(ts$gluc)
table(ts$gender, ts$gluc)

library(rio)
library(dplyr)
library(rafalib)

tidy_set <- dat %>% filter((ap_lo<200&ap_lo>20)&(ap_hi<300&ap_hi>40))
.tidy_set<-tidy_set[tidy_set$ap_hi>tidy_set$ap_lo,]
head(.tidy_set)
set.seed(1)

s1.g1<-sample(.tidy_set$ap_hi[.tidy_set$gluc==1&.tidy_set$gender==1], 20)
s1.g1

s2.g1<-sample(.tidy_set$ap_hi[.tidy_set$gluc==1&.tidy_set$gender==2], 20)
s2.g1

s1.g2<-sample(.tidy_set$ap_hi[.tidy_set$gluc==2&.tidy_set$gender==1], 20)
s1.g2

s2.g2<-sample(.tidy_set$ap_hi[.tidy_set$gluc==2&.tidy_set$gender==2], 20)
s2.g2

s1.g3<-sample(.tidy_set$ap_hi[.tidy_set$gluc==3&.tidy_set$gender==1], 20)
s1.g3

s2.g3<-sample(.tidy_set$ap_hi[.tidy_set$gluc==3&.tidy_set$gender==2], 20)
s2.g3


# новый вектор 'gender.new' и 'gluc.new'

gender.new<-c(rep(1,20), rep(2,20), rep(1,20), rep(2,20), rep(1,20), rep(2,20))
gender.new

gluc.new<-c(rep(1,40),rep(2,40),rep(3,40))
sam_s<-c(s1.g1, s2.g1, s1.g2, s2.g2, s1.g3, s2.g3)
anovaframe<- data.frame(sam_s, gender.new, gluc.new) # соблюдаются случайность и независимость
head(anovaframe,25)

table(anovaframe$gender.new, anovaframe$gluc.new)


# немного разведочного анализа

boxplot(sam_s ~ gender.new, data = anovaframe,
        boxwex = 0.15, at = 1:2 - 0.3,
        subset = gluc.new == '1', col=5,
        main = 'EDA ANOVA',
        xlab = 'sex',
        ylab = 'верхнее давление',
        xlim = c(0.5, 2.5), ylim = c(0, 200))
boxplot(sam_s ~ gender.new, data = anovaframe, add = TRUE,
        boxwex = 0.2, at = 1:2 - 0.1,
        subset = gluc.new == '2', col = '2')
boxplot(sam_s ~ gender.new, data = anovaframe, add = TRUE,
        boxwex = 0.2, at = 1:2 + 0.15,
        subset = gluc.new == '2', col = 'brown')
legend('bottomleft', c('gluc=1', 'gluc=2', 'gluc=3'),
       fill = c('5', '2', 'brown'))


# проверка нормальности распределения

mypar(2,3)
qqnorm(s1.g1)
qqline(s1.g1)
qqnorm(s1.g2)
qqline(s1.g2)
qqnorm(s1.g3)
qqline(s1.g3)
qqnorm(s2.g1)
qqline(s2.g1)
qqnorm(s2.g2)
qqline(s2.g2)
qqnorm(s2.g3)
qqline(s2.g3)


# критерий <Бартлета> 

bartlett.test(list(s1.g1, s2.g1, s1.g2, s2.g2, s1.g3, s2.g3))

summary(aov(sam_s~gender.new + gluc.new + gender.new:gluc.new, data = anovaframe))
summary(aov(sam_s~gluc.new + gender.new + gluc.new:gender.new, data = anovaframe))

summary(aov(sam_s~gender.new*gluc.new, data = anovaframe))

