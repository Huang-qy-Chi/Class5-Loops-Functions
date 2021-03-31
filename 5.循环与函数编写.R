rm(list = ls())

#for循环示例1：图1
#这个例子是for循环最常见的用法，迭代器使用数字计数
vec <- rep(0,1000)              #输出层     
for(i in 1:length(vec)){        #迭代器
  a = rnorm(100)
  vec[i] = median(a)            #循环体
  cat("  The loop has been carried out",i,"times.\r")  
}
median(vec)

#for循环示例2
#迭代器的另一种选择，根据表格或定义元素的“长度”确定迭代器，但总体上还是按照数循环

set.seed(191)
chart <- data.frame(a = rnorm(10), b = runif(10),
                    c = rexp(10), d = rchisq(10, df = 2))

out <- rep(0,4)                #输出层
for(i in seq_along(chart)){    #迭代器
  out[i] = median(chart[[i]])  #循环体
}
out

#等效写法，当表格维数增高后不具备操作性
out2 <- c(median(chart$a), median(chart$b), median(chart$c), median(chart$d))
out2

#反复转存写法(不推荐)
out3 <- c()                          #输出层
for(i in seq_along(chart)){          #迭代器
  out3 = c(out3,median(chart[[i]]))  #循环体
}
out3

#案例：梯度下降法，以y=x^2为例
library(ggplot2)
x = runif(200,-5,5)
y = x^2
xy <- as.data.frame(cbind(x,y))
ggplot(xy, aes(x = x, y = y)) + 
  geom_line(colour = "red")

#学习率为0.1
#repeat函数
x=5         #初始位置
repeat{
  x = x - 0.01*2*x        #直接使用求导后的表达式
  if(abs(0.2*x) < 10^-5){  #跳出循环条件：常用柯西收敛准则判断收敛
    break
  }  
}
x

#while函数
x=5
while(abs(0.2*x) > 10^-5){
  x = x-0.01*2*x      
}
x

#针对更加一般的函数：多为初等函数，以及凸函数，
#可以用于计算的求导用函数
library(Deriv)  
set.seed(114)
f1 <- function(x) 0.4*log(x)+3*sin(x)  #f1显然是一个初等函数
df1 <- Deriv(f1,"x")      #这里的df1是f1的导函数  
df1(8)                    #计算f1在8处的导数值
library(help = "Deriv")   
help(package = "Deriv")

#作图：仿照对x^2的绘图作出f1的图像？



#以repeat为例构造的梯度下降，起始点选择为2
i = 0
x0 = 2
repeat{
  x0 = x0 - 0.01*df1(x0)
  i = i+1                                 #i用于计数
  if(abs(0.01*df1(x0))<10^-3|i>=500){ #设置了脱出条件
    break
  }
  cat("  循环了",i,"次.\r")
}

x0


#更换迭代开始的点
i = 0
x0 = 9     #不同起始点会带来不同的局部极小值
repeat{
  x0 = x0 - 0.01*(df1(x0))
  i = i+1
  if(abs(0.01*df1(x0))<10^-3||i>=500){
    break
  }
  cat("循环进行了",i,"次.\r")
}

x0

#泛型函数
methods(plot)

getAnywhere(plot.ecdf)


#编写函数
#标准化
fun2 <- function(x){
  x = x-mean(x)   #中心化
  x = x/sd(x)     #标准化
  x
}
v1 <- c(2432,4334,2478,2627)
fun2(v1)




#以n个人中至少两人同一天生日概率为模型编写函数
fun3 <- function(n){
  if(n<=365&n>=0){
    pro <- 1-prod(365:(365-n+1))/365^n #至少两人同一天的概率
  }else if(n<0){
    pro = 0
  }else if(n>365){
    pro = 1
  }
  return(pro)
}
fun3(-1)

fun3(20)

fun3(70)   #实际上程序存在问题：人数再增多会导致R无法正常计算，因为R中计算无穷存在“上限”

fun3(367)

fun3(20,70) #注意输入元素过多会报错

#批量建模与批量输出
library(purrr)
result <- as.data.frame(map(20:70, fun3))   #map函数的批量输出
lapply(20:70,fun3)  #lapply批量输出
sapply(20:70,fun3)  #输出为列表

#性能测试
library(microbenchmark)
asq <- rnorm(1000)
microbenchmark(sum(asq),fun2(asq))   #运行时间比较

