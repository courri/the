第2章  向量

2.1 标量、向量、数组与矩阵

##2.1.1 添加或删除向量元素

x <- c(88,5,12,13)
x <- c(x[1:3], 168, x[4])
x


##获取向量长度

x <- c(1,2,4)
length(x)


first1 <- function(x){
  for (i in 1:length(x)){
    if (x[i] == 1) break
  }
  return(i)
}
#你可以敲上面的函数，也可以直接全选，Ctrl+enter即可

#下面调用它

y <- c(2,3,4,1,5,1,9)
first1(y)



x<-c()
x
length(x)
1:length(x)


##2.1.3 作为向量的矩阵和数组

m<-rbind(c(1,2),c(3,4)) ## row bind ，按行绑定
m
10:13
m+10:13  ##这里是按列向量来算的


##2.2 声明

z<-3
yy[1]<-5   ##无法正常工作的语句
yy[2]<-12  ##无法正常工作的语句
yy         ##无法正常工作的语句


yy<-vector(length = 2)
yy[1]<-5   ##现在，可以正常工作的语句
yy[2]<-12  ##正常工作的语句
yy         ##正常工作的语句

zz<-c(5,12)
zz


x<-c(1,5)
x
x<-"abc"
x




##2.3循环补齐

c(1,2,4)+c(10,10,10,10,10,10)

x<-rbind(c(1,4),c(2,5),c(3,6)) ## row bind ，按行绑定
x
x+c(1,2)
x+c(1,2,1,2,1,2)



##2.4 常用的向量运算

#2.4.1向量的运算和逻辑运算
2+3
"+"(2,3)


x<-c(1,2,4)
x+c(5,0,-1)

"+"(x,c(5,0,-1))

x*c(5,0,-1)

x/c(5,4,-1)

x%%c(5,4,-1)


#2.4.2向量索引

y<-c(1.2, 3.9, 0.4, 0.12)
y[c(1,3)]
y[2:3]
v<-3:4
y[v]


x<-c(4,2,17,5)
y<-x[c(1,1,3)]
y


z<-c(5,12,13)
z[-1]
z[-1:-2]
z[-3:-2]

z<-c(5,12,13)
z[1:(length(z)-1)]


z[-length(z)]


#2.4.3用 ： 运算创建向量

5:8
5:1

i<-5
1:i  -1
1:(i-1)

#2.4.4使用seq()创建向量


seq(from=12,to=30,by=3)
seq(12,30,3)

seq(from=1.1,to=2,length=10)

seq(10)

x<-c(5,12,13)
seq(x)


x<-c(5,12,13,14,15,16)
seq(x)


x<-NULL
x
seq(x)


#2.4.5使用rep()重复向量常数
x<-rep(8,4)
x
rep(c(5,12,13),3)
rep(1:3,2)
rep(c(5,12,13),each=8)


##2.5  使用 all() 和  any()

x<-1:10
x
any(x>88)
all(x>88)
all(x>0)
any(x>8)
x>8
x%%2==1  ##奇数


2.5.1扩展案例：寻找连续出现1的游程 runs

findruns <- function(x,k){
  n <- length(x)
  runs <- NULL
  for (i in 1:(n-k+1)){
    if (all(x[i:(i+k-1)]==1)) runs <- c(runs,i)
  }
  return(runs)
}

测试
y<-c(1,0,0,1,1,1,0,1,1)
findruns(y,3)
findruns(y,2)
findruns(y,6)


findruns1 <- function(x,k) {
  n <- length(x)
  runs <- vector(length=n)  #_label~runsalloc@
  count <- 0
  for (i in 1:(n-k+1)) {
    if (all(x[i:(i+k-1)]==1)) {
      count <- count + 1
      runs[count] <- i #_label~runsfill@
    }
  }
  if (count > 0) {
    runs <- runs[1:count] #_label~runsredefine@
  } else runs <- NULL
  return(runs)
}

y<-c(1,0,0,1,1,1,0,1,1)
findruns1(y,3)
findruns1(y,2)
findruns1(y,6)


#2.5.2扩展案例：预测离散时间值时间序列



x <- c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
       0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,
       0,1,1,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,
       0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,1,0,0,
       0,0,0,0,0,1,1,0,0,0,1,1,0,0,0,0,0,1,1,0,
       0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,
       1,1,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,0,0,0,
       0,1,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,
       0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,1,1,
       0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,0,0,
       0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,
       0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,
       1,1,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,
       1,1,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,
       0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,
       0,0,0,0,1,1,0,0,0,1,1,0,0,0,0,0,1,1,0,0,
       0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,
       1,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,0,0,0,0,
       1,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,
       0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,
       0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,
       0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,0,
       0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,
       1,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,
       1,1,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,
       0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1)

x


preda <- function(x,k) {
  n <- length(x)
  k2 <- k/2
  # the vector pred will contain our predicted values
  pred <- vector(length=n-k)
  for (i in 1:(n-k)) {
    if (sum(x[i:(i+(k-1))]) >= k2) pred[i] <- 1 else pred[i] <- 0 #_label~sumx@
  }
  return(mean(abs(pred-x[(k+1):n])))
}



a = vector(length = 15)  # 用来参考的时间段：2天... 半个月15天。
for(i in 2:15){
  a[1] = 1 # 第一个数，随便初始化成1，就好
  a[i] = preda(x,i)
}


print(a)


which.min(a)  # 找到差距最小的a[i] , 所对应的i值



##重写代码pred，b版

predb <- function(x,k) {
  n <- length(x)
  k2 <- k/2
  pred <- vector(length=n-k)
  sm <- sum(x[1:k])
  if (sm >= k2) pred[1] <- 1 else pred[1] <- 0
  if (n-k >= 2) {
    for (i in 2:(n-k)) {
      sm <- sm + x[i+k-1] - x[i-1] #_label~update@
      if (sm >= k2) pred[i] <- 1 else pred[i] <- 0
    }
  }
  return(mean(abs(pred-x[(k+1):n])))
}


测试：

x

b = vector(length = 15)  # 用来参考的时间段：2天... 半个月15天。
for(i in 2:15){
  b[1] = 1 # 第一个数，随便初始化成1，就好
  b[i] = predb(x,i)
}


print(b)


which.min(b)  # 找到差距最小的a[i] , 所对应的i值

##  重写代码pred，c版

predc <- function(x,k) {
  n <- length(x)
  k2 <- k/2
  # the vector red will contain our predicted values
  pred <- vector(length=n-k)
  csx <- c(0,cumsum(x))
  for (i in 1:(n-k)) {
    if (csx[i+k] - csx[i] >= k2) pred[i] <- 1 else pred[i] <- 0
  }
  return(mean(abs(pred-x[(k+1):n])))
}



测试：

x


c = vector(length = 15)  # 用来参考的时间段：2天... 半个月15天。
for(i in 2:15){
  c[1] = 1 # 第一个数，随便初始化成1，就好
  c[i] = predc(x,i)
}



print(c)




which.min(c)  # 找到差距最小的a[i] , 所对应的i值




#####咱们小小的温习一下子

example(which.min)

which.min(c(1,2,3,4,5,6,6,8,0,10,11,12,13,100))

which.max(c(1,2,3,4,5,6,6,8,0,10,11,12,13,100))

x<-rep(1,9)

x

cumsum(x)


################################################学到这里，你最好休息一下哦
################################################学到这里，你最好休息一下哦
################################################学到这里，你最好休息一下哦
################################################学到这里，你最好休息一下哦
################################################学到这里，你最好休息一下哦
################################################学到这里，你最好休息一下哦
明天再继续学习吧




2.6向量化运算符

2.6.1 向量输入，向量输出

u<-c(5,2,8)
v<-c(1,3,9)
u>v

w <- function(x) return(x+1)
w(u)


sqrt(1:9)


y<-c(1.2, 3.9, 0.4)
z <- round(y)
z

round(1.2)

y<-c(12,5,13)
y+4

'+'(y+4)

"+"(y+4)


f <- function(x,c) return((x+c)^2)
f(1:3,0)
f(1:3,1)

f(1:3,1:3)


f <- function(x,c) {
  if(length(c)!=1) stop("vector c not allowed")
  return((x+c)^2)
}

f(1:3,1) #这里c的length = 1，可以！

f(1:3,1:3)  # 这里不允许c是vector



2.6.2 向量输入、矩阵输出

z12 <- function(z) return(c(z,z^2)) 
x<- 1:8
z12(x)


matrix(z12(x),ncol=2)


sapply(1:8,z12)



2.7     NA 与 NULL 值

##2.7.1 NA 的使用

x <- c(88,NA,12,168,13)
x
mean(x)
mean(x,na.rm=T)


x<-c(88,NULL,12,168,13)
mean(x)



x<- c(5,NA,12)
mode(x[1])
mode(x[2])


y<-c("abc", "def", NA)
mode(y[2])
mode(y[3])


##2.7.2 NULL 的使用


#下面创建偶数向量
z <- NULL
z
for (i in 1:10) if(i %% 2 ==0) z<- c(z,i)
z


#下面是寻找个位数的偶数的另外的方法

seq(2,10,2)

2*  1:5


#这里看一下NA与NULL的区别
z <- NA
z
for (i in 1:10) if(i %% 2 ==0) z<- c(z,i)
z


u<- NULL
length(u)

v<-NA
length(v)



##2.8筛选

##生成筛选索引

z<-c(5,2,-3,8)

w<- z[z^2 >8]
w



z<-c(5,2,-3,8)
z
z*z>8

#一个更具针对性的例子
z<-c(5,2,-3,8)
j <- z*z >8
j
y <-c(1,2,30,5)
y[j]


#更简洁的写法
z<- c(5,2,-3,8)
y <-c(1,2,30,5)
y[z*z>8]


#下面是另一个例子，表现筛选的
x<-c(1,3,8,2,20)
x[x>3] <- 0
x



##2.8.2  使用subset()函数筛选

x<-c(6,1:3,NA,12)
x
x[x>5]
subset(x,x>5)


## 2.8.3 选择函数which

z<-c(5,2,-3,8)
which(z*z >8)
which.min(z)
which.max(z)



first1a <- function(x) return(which(x==1)[1])
x<- c(2,2,2,2,1,2,2,2,1)
first1a(x)


#2.9向量化的 ifelse()函数

#   ifelse(bool,True_u,False_v)——————这个用法，略抽象啊。。。

example(ifelse)

x <- c(6:-4)
sqrt(x)
ifelse(x >= 0, sqrt(x), print("得到纯虚数"))


x<-1:10
x
y<- ifelse(x%%2 == 1,print( "奇数"),print("偶数"))
y


#这里有另一个例子

x<-c(5,2,9,12)
ifelse(x>6, 2*x,3*x)

################################################学到这里，你最好休息一下哦
################################################学到这里，你最好休息一下哦
################################################学到这里，你最好休息一下哦
################################################学到这里，你最好休息一下哦
################################################学到这里，你最好休息一下哦
################################################学到这里，你最好休息一下哦
明天再继续学习吧




##重要的一节！！！ 扩展案例--度量相关性

# findud() converts vector v to 1s, 0s, representing an element
# increasing or not, relative to the previous one; output length is 1
# less than input

#这里的字母 ud，表示的是up上升和 down下降

findud <- function(v) {
  vud <- v[-1] - v[-length(v)]  #_label~findvud1@
  return(ifelse(vud > 0,1,-1))  #_label~findvud2@
}


udcorr <- function(x,y) {
  ud <- lapply(list(x,y),findud)
  return(mean(ud[[1]] == ud[[2]]))
}


x <- c(5,12,13,3,6,0,1,15,16,8,88)
y <- c(4,2,3,23,6,10,11,12,6,3,2)
udcorr(x,y)


#向量的 滞后 运算 diff()

u<- c(1,6,7,2,3,5)
u
diff(u) # 滞后一期
diff(u,2) #滞后2期
sign(diff(u))




##扩展案例：对鲍鱼数据集重新编码



g <- c("M", "F", "F", "I", "M", "M", "F")
g
ifelse(g == "M",1,ifelse(g == "F",2,3))


m<- which(g == "M")
f<- which(g == "F")
i<- which(g == "I")
m
f
i


grps<- list()
for (gen in c("M","F","I"))  grps[[gen]]<-which(g == gen)

grps


setwd("pokai")
getwd()


aba <- read.csv("abalone.data",header=T,as.is=T)
grps <- list()
for (gen in c("M","F")) grps[[gen]] <- which(aba[,1]==gen)
abam <- aba[grps$M,]
abaf <- aba[grps$F,]
plot(abam$Length,abam$Diameter)
plot(abaf$Length,abaf$Diameter,pch="x",new=FALSE)


#上面的代码，可以用么？？还是画不出来呢？？







### 2.10  测试向量相等
x<- 1:3
y<- c(1,3,4)
x==y
all(x==y)

identical(x,y)


x<- 1:2 ## : 这个符号产生的是整数
y<-c(1,2)
x==y
identical(x,y)
typeof(x)
typeof(y)



##2.11  向量元素的名称


x<- c(1,2,4)
names(x)
x

names(x)<-c("a","b","ab")
names(x)
x


names(x)<- NULL
x

names(x)<-c("a","b","ab")
x
x["b"]



### 2.12 关于c()的更多内容


c(5,2,"abc")


c(5,2,list(a=1,b=4))


#扁平化的效果

c(5,2,c(1.5, 6))

c(c(5, 2), c(1.5, 6))




第二章结束啦！！！！！！！！！！！
开心！！！

