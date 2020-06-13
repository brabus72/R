## Домашняя работа №1


getwd() # вывод расположения директории

x_1 <- c(1, 2, 3) # вектор
x_1

10-6

x <- 1:100
x
x[1:10]

10 - 8


## объекты

x.1 <- 17 # переменная
x.1

rm(x.1)
x.1


## основные функции

1+6
1-6
10*2
10**2
10**4
10^3
sqrt(25)
log(8)
log2(8)
log10(8)

help(log)

logb(x_1, base = exp(2))

log(exp(1))

exp(1)

factorial(5)


## работа функции

sqrt(log2(25))


## установка пакетов

search()

install.packages("effsize")



writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

Sys.which("make")

install.packages("jsonlite", type = "source")



install.packages("abctools")

install.packages("ada", type = "source")

install.packages("ada")

install.packages("arfima")


library(abctools)

library(ada)

library(arfima)


## сложение векторов

a<-c(0, 1, 2, 3)
a
a[4]
b<-c(rep(2, 3))
b

a+b # fail
b.1<-c(rep(2, 4))
b.1

a+b.1
a


# последовательность

seq(2, 8, by = 2) # чётные
seq(1, 1, by = 2) # нечётные
?seq
seq(1, 10, length.out = 5)
a
s<-1:10
s

set.seed(1)
rnorm(50, mean = 0, sd = 1)

set.seed(1)
rna<-rnorm(50, mean = 0, sd = 1)

rpois(100,10)
?rpois

set.seed(1)
rbinom(100,10,0.5)

rep(c(0,2),time=2) # раз
rep(c(0,2),each=2) # каждый

set.seed(2)
popul<-rnorm(100)
popul

popul.1<-round(popul,4)
popul.1

sort(popul.1)
sort(popul.1,decreasing = TRUE)
popul.s<-sort(popul.1,decreasing = TRUE)
popul.s

lets<-sample(letters,100,replace = TRUE)
lets

df<-data.frame(lets,popul.1)
head(df)

ind<-order(df$popul.1)
ind

df.new<-df[ind,]
head(df.new,15)


## текстовые векторы

letters
LETTERS
?paste
paste(letters, set="_", seq(1,26))
paste(letters, sep="_", seq(1,26))
paste0(letters, set="_", seq(1,26))
paste0(letters, sep="_", seq(1,26))

?ISOdate()

ISOdate(2019,9,1:30)
ISOdate(2019,9,15:30)
format(ISOdate(2019,9,1:30), "%d")
format(ISOdate(2019,9,1:30), "%b")
paste(format(ISOdate(2019,9,1:30), "%d"),set="_",format(ISOdate(2019,9,1:30), "%b"))


## прочие функции
# обучающие наборы данных

data()
data(package = .packages(all.available = TRUE))
search()

install.packages("cluster")
library(cluster)
animals
?animals
animals$gro
animals[2,]
animals[,6]

5 > 6
5 < 100
21 != 2
3 == 3

10 >= 10
6 > 7 | 10 < 12
6 > 7 | 10 > 12 | 12 < 18
6 > 7 | 10 > 12 | 12 > 18

6 < 7 & 10 < 12
10 > 12 & 12 < 18

((T == F)&(1 == 1)) & 100 == 100
((T == F)&(1 == 1)) | 100 == 100

is.na(animals)
sum(is.na(animals))

animals

head(animals,3)
animals$gro
animals$gro[!is.na(animals$gro)]
animals$gro[is.na(animals$gro)]
sum(animals$gro[!is.na(animals$gro)])
sum(animals$gro[is.na(animals$gro)])

?iris
head(iris,10)
tail(iris)
dim(iris)
str(iris)
unique(iris)
unique(iris$Species)
levels(iris$Species)
iris[1,1]
head(iris)
traindat<-iris[1:5,c(2,3,5)]
traindat[traindat$Sepal.Width>3,]
traindat[traindat$Sepal.Width>3 & traindat$Petal.Length>1.4,]


## строим dataset

weight<-c(70,80,90,73,84,96)
height<-c(170,160,165,159,170,185)
sex<-c(rep("F",3),rep("M",3))
sex
class(sex)

df.1<- data.frame(weight,height,sex,stringsAsFactors = F)
df.1
str(df.1)
?data.frame
df.2<- data.frame(weight,height,sex,stringsAsFactors = T)
df.2
str(df.2)
unique(sex)
factor(sex)
levels(factor(sex))


## строим матрицу

m<-1:30
m
z<-c(10,3)
z
?dim
dim(m)
dim(m)<-z
m
class(m)

nrow0 <- function(m) dim(m)[1]
nrow0
class(nrow0)

# 2-й способ

y<-1:50
y
mt<-matrix(y,10,5)
mt

imena<-LETTERS[1:10]
imena

cbind(imena,mt)
class(cbind(imena,mt))

rownames(mt)
colnames(mt)
rownames(mt, do.NULL = T, prefix = "row")
rownames(mt)<-imena
mt

colnames(mt, do.NULL = TRUE, prefix = "col")
colnames(mt)<-paste("day",1:5)
mt
mt[,4]
