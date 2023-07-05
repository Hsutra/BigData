x = c(1,2,3,4,5,6)
sex = c('f','f','m','f','m','f')
sex.f = factor(sex)
w = c(12,36,76,12,87,67)
plot(x,w,pch = as.numeric(sex.f),col= as.numeric(sex.f))
legend("topleft", pch=1:2, col=1:2, legend = levels(sex.f))
m = c("L","XXL","M","L","M","M")
m.f = factor(m)
m.o = ordered(m.f, levels = c("XXL","L","M"))
h = c(9,10,NA,NA,8,10)
mean(h,na.rm = TRUE)
mean(na.omit(h))
h[is.na(h)] = mean(h,na.rm = TRUE)
h
names(w) = c("a","b","c","d","e","f")
w
df = data.frame(wieght = w, height = x, size = m.o, sex = sex.f)
w
x
m.o
df
df$wieght
df[[1]]
df[,1]
df[, "wieght"]
df[df$sex == 'f',]

