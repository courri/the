getwd()
setwd("pokai")
getwd()


y<-matrix(c(1,2,3,4), nrow=2,ncol=2)
y


y<-matrix(c(1,2,3,4), nrow=2)
y


y[,2]

y<-matrix(nrow=2,ncol=2)
y[1,1]<-1
y[2,1]<-2
y[1,2]<-3
y[2,2]<-4
y

y<-matrix(c(1,2,3,4,5,6), nrow=2,byrow=T)
y


##3.2一般矩阵运算

y<-matrix(c(1,2,3,4), nrow=2)
y

y%*%y

3*y


y+y


##

z<-matrix(c(1,1,1,2,1,0,3,0,0,4,0,0), nrow=4,byrow=T)
z

z[,2:3]  #提取2,3列


y<-matrix(c(11,12,21,22,31,32), nrow=3,byrow=T)
y
y[2:3,]  #提取2,3行
y[2:3,2] #提取2,3行，进一步第2列的22,32


y<-matrix(c(1:6), nrow=3)
y

y[c(1,3),]<-matrix(c(111,333,666,888),nrow=2)#这里给1,3行赋值
y

x<-matrix(nrow=3,ncol=3)
y<-matrix(c(4,5,2,3),nrow=2)
y
x[2:3,2:3]<-y
x


y<-matrix(c(1:6), nrow=3)
y

y[-2,] #去掉第二行

y[,-2] #去掉第二列


##一个有趣的扩展案例： 图像操作
install.packages("pixmap") 
library("pixmap")
mtrush1<- read.pnm("mtrush1.pgm")
mtrush1
plot(mtrush1)


str(mtrush1)

mtrush1@grey[28,88]

mtrush2<-mtrush1
mtrush2@grey[84:163,135:177]<-1
plot(mtrush2)


#随机噪声
# adds random noise to img, at the range rows,cols of img; img and the
# return value are both objects of class pixmap; the parameter q
# controls the weight of the noise, with the result being 1-q times the
# original image plus q times the random noise
blurpart <- function(img,rows,cols,q) {
  lrows <- length(rows)
  lcols <- length(cols)
  newimg <- img
  randomnoise <- matrix(nrow=lrows, ncol=ncols,runif(lrows*lcols))
  newimg@grey <- (1-q) * img@grey + q * randomnoise
  return(newimg)
}


mtrush3 <- blurpart(mtrush1,84:163,135:177,0.65)
plot(mtrush3)  ##似乎并不能显示？？


##3.2.4 矩阵元素筛选

x<- matrix(c(1,2,3,2,3,4),nrow=3)
x

x[x[,2]>=3,]
#详细分析
j<-x[,2]>=3
j
x[j,]


z<-c(5,12,13)
x[z%%2==1,]


m<-matrix(c(1:6),nrow=3)
m
m[m[,1]>1 & m[,2]>5,]
#你可以看到:
m[,1]>1 & m[,2]>5


m<-matrix(c(5,2,9,-1,10,11),nrow=3)
m
which(m>2)



#3.2.5 生成协方差矩阵


makecov <- function(rho,n){
  m<-matrix(nrow=n,ncol=n)
  m<-ifelse(row(m)==col(m),1,rho)
  return(m)
}

makecov(0.2, 3)


#3.3 对矩阵的行和列调用函数

z<- matrix(c(1:6),nrow=3)
z
apply(z,2,mean)

colMeans(z)
rowMeans(z)


f<- function(x) x/c(2,8)
y<-apply(z,1,f)
y
t(y)


copymaj<-function(rw,d){
  maj<-sum(rw[1:d])/d
  return(if(maj>0.5) 1 else 0)
}

x<-matrix(c(1,1,1,0,0,1,0,1,1,1,0,1,1,1,1,1,0,0,1,0),nrow=4)

x
apply(x,1,copymaj,3)
apply(x,1,copymaj,2)


#扩展案例： 寻找异常值






findols<- function(x){
  findol<-function(xrow){
    mdn<-median(xrow)
    devs<-abs(xrow-mdn)
    return(which.max(devs))
  }
  return(apply(x,1,findol))
}


rs<-matrix(c(1:99,1000),nrow=20)

rs

findols(rs)

#增加或删除矩阵的行或列

x<-c(12,5,13,16,8)

x

x<-c(x,20)
x

x<-c(x[1:3],20,x[4:6])
x

x <- x[-2:-4]
x


one<-rep(1,4)
one
z<-matrix(c(1,2,3,4,1,1,0,0,1,0,1,0),nrow=4)
z


cbind(one,z)


cbind(1,z)


q<-cbind(c(1,2),c(3,4))
q


m<-matrix(c(1:6),nrow=3)
m
m<-m[c(1,3),] #取得1，3行
m

##扩展案例：找到图中距离最近的一对端点

我觉得这个函数并不是很好理解，总之有个地方略费解。
看看即可。


# returns the minimum value of d[i,j], i != j, and the row/col attaining
# that minimum, for square symmetric matrix d; no special policy on ties
mind <- function(d) {
  n <- nrow(d)
  # add a column to identify row number for apply()
  dd <- cbind(d,1:n)  #_label~ddline@
  wmins <- apply(dd[-n,],1,imin)  #_label~dapp@
  # wmins will be 2xn, 1st row being indices and 2nd being values
  i <- which.min(wmins[2,]) #_label~xxx1@
  j <- wmins[1,i]  #_label~xxx2@
  return(c(d[i,j],i,j))  #_label~xxx3@
}

# finds the location, value of the minimum in a row x
imin <- function(x) {  #_label~imin@
  lx <- length(x)
  i <- x[lx]  # original row number
  j <- which.min(x[(i+1):(lx-1)])  #_label~wmx@
  k <- i+j  #_label~iplusj@
  return(c(k,x[k]))
}


q<-matrix(c(0,12,13,8,20,12,0,15,28,88,13,15,0,6,9,8,28,6,0,33,20,88,9,33,0),nrow=5)
q
mind(q)




#3.5 向量与矩阵的差异



z<-matrix(1:8,nrow=4)
z
length(z)
class(z)
attributes(z)
dim(z)
nrow(z)
ncol(z)


nrow<-function(x) dim(x)[1]


library("Matrix", lib.loc="C:/Program Files/R/R-3.0.2/library")

Hilbert(6)