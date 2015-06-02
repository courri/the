##模拟标准正态分布随机变量的取值
mean(abs(rnorm(100)))
##光标放在这行程序上，你可以直接点击 run 来运行这行程序。或者 Ctrl+Enter 来运行它


rnorm(10)
#随机变量，10个数，服从标准正态分布


x <- c(1,2,4)
x

q <- c(x,x,x,8)
q


x[3]

x[2:3]


mean(x)


sd(x)


y <- mean(q)

y


##===========================================


data()  #####得到内置数据集的一个list


Nile


mean(Nile)

sd(Nile)

hist(Nile)

q()  ### quit 程序


##===========================================
函数入门
先定义下面的函数：奇数计数函数。你可以全选这个函数本身，然后 Ctrl+Enter，载入之。


oddcount <- function(x)  {
  k <- 0  # assign 0 to k
  for (n in x)  {
    if (n %% 2 == 1) k <- k+1  # %% is the modulo operator
  }
  return(k)
}


x

oddcount(x)


q

oddcount(q)


oddcount(c(1,2,3,7,9))



38%%7


##===========================
1.3.1变量的作用域


oddcount(c(1,2,3,7,9))

n



z<-c(2,6,7)
oddcount(z)



f<-function(x) return(x+y)
y<-3
f(5)




##==========================
1.3.2默认参数


g<-function(x,y=2,z=T) return(x+y)
g(12,z=FALSE)



#######################################
#######################################
#######################################
1.4 R语言中一些重要的数据结构


##================
1.4.1向量，R语言中的战斗机



x<-8 # 标量，其实是一元向量
x
x[1]


向量的一般定义方式
v <- c(1,2,3,7,9)
v
v[2:4]
v[4]

##================
1.4.2字符串

x<-c(5,12,13)
x
length(x)
mode(x)



y<-"pokaikai"   ##一个字符串的向量
y
length(y)
mode(y)


z<-c("pokaikai","29 28","haoxiaoyi")  ##3个字符串的向量
z
length(z)
mode(z)



一些字符串操作函数：

u<-paste("pokaikai","喜欢","haoxiaoyi")
u



v<-strsplit(u," ")
v




##===========================
1.4.3矩阵

矩阵的例子

m<-rbind(c(1,4),c(2,2)) ## row bind ，按行绑定
m

m%*%c(1,1) ##矩阵乘法，运算符是  %*%  ！！！记住啦



双下标来索引矩阵

m[1,2]

m[2,2]


m[1,] #取出第一行
m[,2] #取出第二列



##======================
1.4.4列表

x<-list(u=2,v="abc")
x
x$u  ##列表x之中的组件 u




hist(Nile)
hn <- hist(Nile)
print(hn)
hn
str(hn)  ## str是结构的意思： structure



##========================
1.4.5数据框

#下面创建的是一个简单的数据框
d <- data.frame(list(kids=c("Jack","Jill"),ages=c(12,10)))
d
d$ages

##注意啦！数据框一般是通过读取Excel之类的文件创建的哦


##========================
类

hn <- hist(Nile)
print(hn)
#你可以看到，这里的属性attribute，是histogram类

Nile
summary(Nile)
plot(Nile)

##========================
1.5扩展案例：考试成绩的回归分析
examsquiz <- read.table("ExamsQuiz.txt",header=FALSE)
examsquiz
class(examsquiz)
head(examsquiz)
tail(examsquiz)
lma <- lm(examsquiz[,2]~examsquiz[,1])
attributes(lma)
str(lma)

lma$coef
lma
summary(lma)


用期中成绩和测验成绩 预测 期末成绩
lmb <- lm(examsquiz[,2]~examsquiz[,1] + examsquiz[,3])
attributes(lmb)
str(lmb)
lmb$coef
lmb
summary(lmb)


##========================
1.6 启动和关闭R

getwd()   #get working directory

setwd("pokai")  #设置工作路径 为  pokai文件夹

getwd()

setwd("..")  #      ..   表示返回上一层目录，于是回到原来的目录

getwd()   #可以看看效果如何

?Startup

##========================
1.7 获取帮助

help(seq)
seq(17)
seq(1,19,by = 2)
seq(0,100,20)

?seq

?"<"

?"for"


##
  
example(seq)

example(persp)  # 如果Rstudio里面看不到，你可以去R软件试试看，应该可以

##
  
help.search("multivariate normal")  #查找 多元正态分布

??"multivariate normal"


help.search("determinant") # 行列式

##
?mvrnorm

help(package = MASS)

?files


##

第一章结束了。
@kaikai
2015年5月22日12:51:10