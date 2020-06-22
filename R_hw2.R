## Домашняя работа №2


library(cluster)
data()
?votes.repub
str(votes.repub)
head(votes.repub)
?votes.repub
votes.repub[1:6,1:10]
colMeans(votes.repub)
colMeans(votes.repub, na.rm = TRUE)

# в 30-м столбце нет пропущенных значений

if(mean(votes.repub[,25])>60){
  print('республиканцы набрали высокий процент голосов')
}else{
  print('республиканцы набрали менее 60% голосов')
}


# в 7-м столбце есть пропущенные значений

if(mean(votes.repub[,7], na.rm = TRUE)>60){
  print('республиканцы набрали высокий процент голосов')
}else{
  print('республиканцы набрали менее 60% голосов')
}


?ifelse
x<-c(3,0,0,0,1,0)
x
ifelse(x!=0,'Yes','No')

ifelse(colMeans(votes.repub, na.rm = TRUE)>60,
       'республиканцы набрали высокий процент голосов',
       'республиканцы набрали менее 60% голосов')

getwd()

dat<-read.csv('dat.csv')
head(dat)
str(dat)
dat$d.date
levels(dat$d.date)
unique(dat$ball)
unique(dat$price)
dim(dat)
datn<-dat[,-1]
rm(dath)
datn
head(datn,10)
str(datn)

install.packages('lubridate')
library(lubridate)
?lubridate
class(datn$d.date)
datn$d.date

dayn<-ymd(datn$d.date)
class(dayn)
dayn
year(dayn)
month(dayn)
day(dayn)

datn$price
datn$price[day(datn$d.date)==1]
sum(datn$price[day(datn$d.date)==1])
f.1<-function(d){
  sum(datn$price[day(datn$d.date)==d])
}
f.1(2)
f.1(3)

sum(f.1(1),f.1(2),f.1(3))
sum(datn$price)

f.2<-function(m,d){
  sum(datn$price[month(datn$d.date)==m & day(datn$d.date)==d])
}
f.2(1,1)
f.2(1,3)
# второго месяца в наборе данных нет
f.2(2,1)

f.1(1)
f.1(2)
for(i in 1:3){
  print(f.1(i))
}
# рассмотрим функции lapply(), sapply()

head(votes.repub)
?apply
apply(votes.repub,1,mean)
?lapply
lapply(votes.repub,sum)
sapply(votes.repub,sum)
class(sapply(votes.repub,sum))

# сначала введём функции range(), diff()

a<-c(1,2,3,5,8)
diff(a)
range(a)
min(a)
max(a)
diff(range(a))
l<-function(x){diff(range(x))}
l(a)
tapply(Orange$circumference, Orange$Tree, l)

search()
effsize::cohen.d()

a<-c(1,1,1,1,1,0,0,0)
b<-c(1,1,1,1,1,0,0,0,0,0,0,0)
sam.a<-sample(a,2)
sam.b<-sample(b,4)
sam.a
sam.b
c(sam.a,sam.b)
s<-sum(c(sam.a,sam.b))
s

mc<-function(m,n){sam.a<-sample(a,m)
sam.b<-sample(b,n)
s<-sum(c(sam.a,sam.b))
}
mc
m<-replicate(100,mc(2,4))
m
mean(m==3)

m.1<-replicate(10000,mc(2,4))
m.1
mean(m.1==3)

m.1<-replicate(100000,mc(2,4))
m.1
mean(m.1==3)

m.1<-replicate(1000000,mc(2,4))
m.1
mean(m.1==3)

###

install.packages("dplyr")
library(dplyr)
iris %>% dim 
iris %>% filter(Species=='versicolor')

iris %>% filter(Species=='versicolor') %>% select(Petal.Length)

iris %>% filter(Species=='versicolor') %>% select(Petal.Length) %>% unlist()

vc<-as.numeric(iris %>% filter(Species=='versicolor') %>% select(Petal.Length) %>% unlist())
vc

vg<-as.numeric(iris %>% filter(Species=='virginica') %>% select(Petal.Length) %>% unlist())
vg

sum.iris<-(vc %in% vg)
sum(sum.iris)


###

a<-c(1,2,3)
b<-c(0,0,0)
nm<-c('1_row', '2_row', '3_row')
nm

df<-data.frame(a,b)
df

df.1<-'rownames<-'(df,nm)
df.1


###

write.csv(df.1, file = 'new_frame.csv')
read.csv('new_frame.csv')

head(read.csv('new_frame.csv'))
head(read.csv2('new_frame.csv'))

install.packages("readxl")
library(readxl)
?read_xls
read_xls('book_1.xls')

install.packages('rio')
library(rio)
install_formats(csvy, feather, fst, hexView, readODS, rmatio, xml2)
import('book_1.xls')

install.packages('rvest')
library(rvest)

url<-paste0('https://biometry.nci.nih.gov/cdas/datasets/nlst/')
url
htl<-read_html(url)

tabl<-htl %>% html_nodes('table')
tabl
tabl<-tabl[[1]] %>% html_table
head(tabl)
class(tabl)
tabl<- tabl %>% setNames(c('data', 'discription'))

tabl$data
class(tabl$data)
t1<-sub(pattern = '\n            \n            ',replacement = '',tabl$data)
t1

t1[1]
strsplit(t1[1],split = NULL)
class(strsplit(t1[1],split = NULL))
length(class(strsplit(t1[1],split = NULL)))
vec<-unlist(strsplit(t1[1],split = NULL))
vec
class(vec)
length(vec)
vec[1:10]
vec[1:15]
substr(t1[1],start = 6,stop = 15)
d_n<-substr(t1,6,15)
d_n

d_f<-paste0(d_n,set = '_',1:15)
d_f

discription<-tabl[,2]
discription[1]
dis_new<-sub('\n                \n                    ',replacement = '',discription)
dis_new

dis_new<-substr(dis_new,start = 3,stop = nchar(dis_new))
dis_new

discription<-paste0('T',dis_new[1:9])
dis_new2<-c(discription,dis_new[10:15])
dis_new2

data.frame(tolower(d_f),dis_new2)

library(dplyr)
data_frame(tolower(d_f),dis_new2)
tibble(data_frame(tolower(d_f),dis_new2))
