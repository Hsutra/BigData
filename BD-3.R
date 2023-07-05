time = (integer(10))
num = (1:10)
res = double(10)
t0 = strptime(Sys.time(),"%Y-%m-%d %H:%M:%S") 

#1
xA = seq(100,200,by = 5)
res[1] = sum(xA)
t1 = strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")
time[1] = t1 - t0

#2
res[2] = (length(xA))
res[3] = mean(xA)
t2 = strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")
time[2] = t2 - t1

#3
xB = rnorm(res[2]+7, mean = 5, sd = 1)
res[4] = round(sd(xB),0)
t3 = strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")
time[3] = t3 - t2

#4
arr = array(xA,dim = c(5,res[2]/5))
s = 0
for (i in arr)
  s = s + sin(i)
res[5] = round(s,4)
t4 = strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")
time[4] = t4 - t3

#5
xA = xA[!xA %in% c(200)]
matr = matrix(xA,nrow=5)
matr = matr[-2,]
matr = matr[-4,]
res[6] = ncol(matr) + nrow(matr)
t5 = strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")
time[5] = t5 - t4

#6
x = rep(c(TRUE,FALSE),5)
l = list(x,x,x)
t6 = strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")
time[6] = t6 - t5

#7
t7 = strptime(Sys.time(),"%Y-%m-%d %H:%M:%S")
res[7] =identical(arr,matr) 
time[7] = t7 - t6

#8

#9

df = data.frame(num,res,time)
colnames(df) = c("num","res","time")











