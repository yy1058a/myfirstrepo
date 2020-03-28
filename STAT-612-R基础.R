# for STAT-612 class use!
8+4+12
8*(8-4)+8
8^2
8**2
# **和^ 用法一样
64^.5
#开方用^小数
1:20
x<-1:20

q()
y
#快速关闭窗口

x<-c(TRUE, FALSE, TRUE)
y<-c(FALSE,TRUE, TRUE)

x
sum(x)

y<-seq(from = 33, to =639, length.out=100)
sum(y)

x[9]

x[9:150]

z<-c(x,y)
length(z)

sum(log(z))

library(tidyverse)
library(ggplot2)
view(mpg)
?mpg
ggplot(data = mpg)+
  geom_point(mapping = aes(x = cty, y = displ))+
  geom_smooth(mapping = aes(x=cty, y=displ))

ggplot(data = mpg)+
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy))+
  geom_smooth(mapping = aes(x=displ, y=hwy))

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut))

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=clarity))

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=color))


#```{R}
#ggplot(data = mpg)+
#  geom_point(mapping = aes(x = displ, y = hwy, color=model))+
#  geom_smooth(mapping = aes(x=displ, y=hwy))
#```

View(mpg)
?mpg

filter(mpg,class=="compact", year==2008) %>%
  print(n=20)
#filter()筛选符合要求的数据，不会隐藏其他变量，n=20表示输出前二十行

filter(mpg,manufacturer=="dodge",class=="pickup",year>1999)%>%
  print(n=20)

filter(mpg, class=="pickup", manufacturer=="ford", year==2008)%>%
  print(n=2)

arrange(mpg,desc(cty))
#arrange排序函数，降序用desc，联合排序直接再加一个变量,desc表示按变量降序


select(mpg,model,trans)
#select选择函数，只看符合要求的列

select(mpg, -(model:cyl))%>%
  print(n=10)
#不展示model到cyl之间的列

mutate( )
#添加新列

x<-select(mpg, cty, hwy)
x
mutate(x, cty/hwy)

select(mpg, class)%>%
  print(class=="compact")
filter(mpg, class=="compact", year==1999, manufacturer=="audi")
select(mpg, hwy ,cty)%>%
  mutate(hwy/cty)

by_modelandyear<-group_by(mpg, model, year)
by_modelandyear
summarise(by_modelandyear, mean(cty, na.rm = TRUE))
#summarise()

#安装nycflight13包使用: install.packages("nycflights13"), library("nycflights13"), library(tidyverse)

by_nycflight13<-s(flights, year, month, day)
summarise(by_nycflight13, mean(dep_delay, na.rm = TRUE), average)

mean(flights$dep_time, na.rm = TRUE)

NA<10 
NA==4 

library(dplyr)
df<-tibble(x=c(5,15,4,7,NA))
arrange(df,x)
arrange(df,desc(x))

flights%>%
  group_by(year,month,day)%>%
  summarize(mean=mean(dep_delay,na.rm = TRUE))

library(ggplot2)

not_cancelled<-flights%>%
  filter(!is.na(dep_delay),!is.na(arr_delay))

delays<-not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay))
ggplot(data=delays, mapping = aes(delay))+
  geom_freqpoly(binwidth=10)

group_by(not_cancelled, dest)%>%
  summarise(carriers=n())%>%
  arrange(desc(carriers))

add_adjacent<-function(x){
  i<-1:(length(x)-1)
  y<-x[i]+x[i+1]
  y
}

library(ggplot2)
ggplot(mpg)+
  geom_point(mapping=aes(displ,cty,color = class,shape =class))+
  scale_shape_manual(values=seq(0,15))
##当自动分类的形状大于6时，就会出问题，所以这时候要用第三行参数来手动设定。

library(dplyr)
filter(mpg,manufacturer=="dodge",model=="durango 4wd")

filter(mpg,cty<10,hwy<16)

arrange(mpg,desc(displ))

arrange(mpg,cty)%>%
  print(n=30)##第四题，print(n=)，注意使用方法

filter(mpg,manufacturer=="ford"&year=="1999"&cty<16&hwy<16)%>%

  
mpg%>%
  filter(manufacturer=="ford",model=="mustang")%>%
  select(manufacturer,model,cty,hwy)%>%
  mutate(difference=hwy-cty)

mpg%>%
  filter(manufacturer=="ford")%>%
  group_by(model)%>%
  summarise(mean=mean(cty))%>%##注意这里要给Mean赋值
  arrange(desc(mean))##arrange的使用后方法

ggplot(mpg)+
  geom_point(mapping = aes(displ,hwy))+
  facet_wrap(~class,nrow=2)##根据class进行分面 wrap(缠绕)→水平分割，分为norw两行（？）

ggplot(mpg)+
  geom_point(mapping = aes(displ,hwy))+
  facet_grid(drv~cyl)##双变量分面 grid(网格)→纵向分割drv,水平分割cyl

ggplot(mpg)+
  geom_point(mapping = aes(displ,cty))+
  facet_wrap(~cyl,nrow=2)

ggplot(midwest)+
  geom_bar(mapping=aes(state))

midwest%>%
  count(state)

ggplot(midwest)+  ##使用直方图，binwidth用于改变条宽,细分更小的区间
  geom_histogram(mapping=aes(popamerindian),binwidth = 250)

midwest%>%  ##创建对应直方图所用的表格
  count(cut_width(popamerindian,250))

less<-midwest%>%  ##先筛选出需要的范围数据，再作图
  filter(popamerindian<1000)
ggplot(less)+ 
  geom_histogram(mapping=aes(popamerindian),binwidth = 25)

faithful%>%
  summarise(mean=mean(eruptions,na.rm=TRUE))
ggplot(faithful)+ 
  geom_histogram(mapping=aes(x =eruptions),binwidth = .25)

ggplot(diamonds)+
  geom_histogram(mapping = aes(y),binwidth = .5)+
  coord_cartesian(ylim = c(0,50)) ##选定y轴范围观察，可视为放大

ggplot(diamonds)+
  + geom_bar(mapping = aes(clarity))

ggplot(diamonds)+
  + geom_histogram(mapping = aes(depth),binwidth = .5)

diamonds%>%
  count(cut_width(depth,.5))

ggplot(mpg)+
  geom_histogram(mapping = aes(cty),binwidth = 2)

boxplot  ##箱线图 IQR=Q3-Q1，小于Q1-1.5IQR或者 大于Q3+1.5IQR定义为离群值

ggplot(mpg,mapping = aes(class,cty))+
  geom_boxplot()+
  coord_flip()  ##用于更改已有图形的横纵向

ggplot(mpg)+
  geom_boxplot(mapping = aes(x=reorder(class,hwy,Fun=median),
                             y=hwy))

tapply(mpg$cty,mpg$class, mean)

tribble(~number,~letter,~greek, ##手动输入表格
        1,"a","alpha",
        2,"b","beta",
        3,"c","gamma")  ##tribble是一种格式，不同于df

as.tibble(iris) ##把表格改成tribble
as.data.frame(flights) ##把tribble改成df

tibble(number=c(1,2,3),
       letter=c("a","b","c"),
       greek=c("alpha","beta","gamma"))

is.tibble(iris) ##判断是否为tibble

attributes(flights) 
str(flights)  ##观察数据的静态组成结果
glimpse(flights)  ##tibble自带的查看数据参数

tibble(   ##tibble的输入比使用tribble更简单
  x = 1:10,
  y = 3,
  z = x+y
)

boxplot(mtcars$disp)

ggplot(mtcars,aes(y = mpg))+
  geom_boxplot()
summary(mtcars$mpg)

boxplot(mtcars$wt)
summary(mtcars$wt)

NAlist<-which(rowSums(is.na(starwars_Gender)) > 0)
starwars_Gender[NAlist,] ##提取有缺失值的行

library(tidyverse)
install.packages("readr")
library(readr)
L,M,O,P
5,7,10,14
1,3,21,9
0,5,11,5

read_csv('L,M,O,P
5,7,10,14
1,3,21,9
0,5,11,5') #csv更适合R读取的文件格式

read_csv2('L;M;O;P
5;7;10;14
1;3;21;9
0;5;11;5') #csv2适用于分号间隔的情况

read_delim('L;M;O;P
5;7;10;14
1;3;21;9
0;5;11;5',delim=";") #使用 delim识别分隔符

read_csv("this line is not needed
this line is also not needed
L;M;O;P
5;7;10;14
1;3;21;9
0;5;11;5", skip=2) #skip表示跳过前两行

read_csv("L,M,N,O\n5,7,10,14\n1,3,21,9\n0,5,11,5")
#\n表示回车，注意斜条方向，第一行自动默认为列名称

read_csv("5,7,10,14\n1,3,21,9\n0,5,11,5",col_names = FALSE)
#col_names = FALSE 表示不自动将第一行选取为列名称

read_csv("5,7,10,14\n1,3,21,9\n0,5,11,5",col_names = c("L","M","N","O"))
#col_names = c() 同样也可以用来添加列名称

parse_integer(c("12","23","5"))  #integer整数
parse_integer(c("12","23","5.1","A"))  #A和5.1不是整数因此会表现为NA

parse_double(c("10","5.21")) #double双整型,自动统一输出格式
parse_number(c("$10","5.21","123,456")) #只输出数字内容

parse_integer(c("123,456","101")) #有逗号存在无法确定整数
parse_number(c("123,456","101")) #只输出数字内容，去除符号
parse_double(c("123,456","101")) #逗号无法确认双整型

library(tidyverse)
library(tidyr)
library(dplyr)
View(who)

table1
table4a

#gather 原变量名(属性名)做键(key)变量值做值(value),行列颠倒了,
#当行(列标题)可以视为一个变量的时候，用gather整合成列,key会变成行
tidy4a<-table4a%>%
  gather("1999","2000",key="year",value="case")

tidy4b<-table4b%>%
  gather("1999","2000",key = "year", value = "population")

left_join(tidy4a,tidy4b) #以左边tidy4a的key为标准的join

#spread 分开一个列
#当一个列包含有限种类的时候，可以把它按种类的分成多列
table2
spread(table2, key=type, value=count)

stocks<-tibble(
  year=c(2015,2015,2016,2016),
  half=c(1,2,1,2),
  return=c(1.88 ,8.59, 0.92,0.17)
)

stocks
stocks%>%
  spread(half,return)
#separate 把有符号的列分成两列：
table3
table3%>%
  separate(rate,into=c("case","population"))

table3%>%
  separate(rate,into=c("case","population"),sep="/") #sep="/"用于指定分隔符

table3%>%
  separate(rate,into=c("case","population"),convert=TRUE) #convert用于修改列属性

#unite 第一个参数为数据集，第二个参数为合并之后的列名称，参数into=c()用于选定要合并的列
table5%>%
  unite("year",into=c("century","year"),sep = "") 

#missing value (explicit, implicit)
stocks<-tibble(
  year=c(2015,2015,2015,2015,2016,2016,2016),
  qtr=c(1,2,3,4,2,3,4),
  return=c(1.88,0.59,0.35,NA,0.52,0.17,2.66)
)

#fill() 把NA值自动填充

----------------------------------
  
library(readr)
library(tidyverse)
x <- read_csv("C:/Users/yinyiping/Desktop/tidypracticedata.csv")
x<-x%>%
  mutate(AGE=parse_number(AGE))

x1<-x%>%
  select('Student Name',"Student ID","GENDER","AGE",everything())

x1%>%
  gather(`Assign 1`:`Assign 6`,key = "Assignments",value = "score")

x2<-x1%>%
  separate("Midterm",into = c("Midterm Scorce","Midterm Grade"),sep = "/")

---------------------------------------
  
iris <- read.csv("C:/Users/yinyiping/Desktop/Copy of iris.csv",header=FALSE)
names(iris)<-c("sepal_length","sepal_width","petal_length","petal_width","species") 
View(iris)

---------------------------------------
  
library(nycflights13)
# 要使用的数据集：flights, atmos, airlines, plans, airports, weather

atmos%>%
  count()%>%
  filter(n>1)

planes%>%
  count(tailnum)%>%
  filter(n>1)  #用于检测tailnum适不适合做key。有大于1的，说明不是唯一，不适合

install.packages("nasaweather")
library(nasaweather)
atmos%>%
  count(lat,long,year,month)%>%
  filter(n>1)

————————————————————————————————————————————————————————————————
#内外键
library(nycflights13)
library(tidyverse)
airlines%>%
  count(carrier)%>%
  filter(n>1)   #主键的意义是不能有超过一个
#外键可以n>1

————————————————————————————————-————————————————————————————————
#join table
x<-tribble(
  ~key,~val_x,
  1,'x1',
  2,'x2',
  3,'x3'
)
y<-tribble(
  ~key,~val_y,
  1,'y1',
  2,'y2',
  4,'y4'
)

x%>%
  inner_join(y,by="key")  #丢弃两个表格都没有的key值

—————————————————————————————————

#chapter11
install.packages("stringr")
library(stringr)
'abc'  #字符串
c('abc')  #常储存在向量中
x<-c('two','four','six','eight')
x
writeLines(x)  #导出没有引号的字符串

#想要导出”(双引号的情况)：
#方法一：使用相同符号的时候
y<-"\""
y
writeLines(y)
#方法二：使用不同的符号
z<-'"'
z
writeLines(z)

#输出\符号的方法(❀这好像是叫转译符)
x<-c("\"",'"',"\\")  #输出\要用两个\\，因为防止单个\会直接发挥作用。
x
writeLines(x)   #可以用于纵向输出想要的字符串
x<-c("Dolphins","Jets","Patriots","Bills")
writeLines(str_sub(x,2,3))
————————————————————————————————————————————
#字符串长度
str_length('The show starts now')
str_length(c('maine','iowa', NA))  #NA值依旧会返回

#combineing strings 合并字符串,或加入分隔符合并。❀合并几个字符
str_c('a','b','c')
str_c("123",'45',sep=";")

str_c("a",c("xy","xz","xp"),"b")

#collapsing a vector of strings  ❀与上面的区别：用于一组向量
str_c(c('x','y','z'),collapse = ',')
str_c(c('mno',"npq"),collapse = "/")
      
#subsetting strings  输出字符的指定长度  subsetting子集
x<-c("apple","banana","pear")
str_sub(x,1,3)  #输出第一到第三

#Modifying 
x<-c("georgia","maine","texas")
str_sub(x,1,1)<-str_to_upper(str_sub(x,1,1)) #转换大写
x

_____________________________________________

x<-c("DENVER","PRLANDO","HOUSTON","CHICAGO","DOVER")
str_view(x,"DE") 
str_view(x,"O$")  #金钱符号表示以什么为结尾
str_view(x,"^H")  #^符号表示以什么为开头
str_view(x,"^HOUSTON$")
str_view(x,".O.")  #查找符合 .O. 格式的字符，用.点来做未知填充
str_view(words, "^[aeuioy].*[^aeuioy]$", match = T) #元音开头辅音结尾 .*表示中间个数不限
#"[aeuioy]元音 [^aeuioy]辅音(除了元音)
str_view(c("abc","a.c","bef"),"a\\.c") #想要匹配“.”需要使用“\\.”
str_subset(words,"tion$|ing$" ) #选择tion或者ing结尾的

writeLines("a\\b")
writeLines("a\\\\b")  #匹配一个“\”要使用“\\”

str_view("a\\b","a\\\\b")  #写在代码中的都是转译了符号的模式
                          #只有writeLines()的输出结果是纯文本

writeLines("\"'\\")  #❀使用\转译单双引号,另外文本也要前后加双引号

stringr::words
str_view(stringr::words,"^k",match = TRUE) #match后缀，只看符合要求
str_view(stringr::words,"^.....$",match = TRUE) #长度选择
str_view(stringr::words,"^[the]",match = TRUE) #选择t.h,e其中之一开头x

x<-c("Apple","banana","pear")
str_detect(x,'e')  #detect发现有e的并返回T/F
sum(str_detect(words,"t$"))
str_subset(words,"[ux]$")
-------------------------------------
library(tidyverse)
data1<-c("East","West","East","North","North","East","West","West","West")
print(data1)
print(is.factor(data1))
factor_data1<-factor(data1)
print(is.factor(factor_data1))

x1<-c("Dec","Apr","Jan","Mar")
month_levels<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
y1<-factor(x1,levels = month_levels)
y1
sort(x1)

gss_cat
gss_cat%>%
  count(race)

ggplot(gss_cat,aes(race))+
  geom_bar()
