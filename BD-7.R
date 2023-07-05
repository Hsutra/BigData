# Задание 1.

# импорт данных (все спортсмены по всем видам спорта)
df <- read.table('C:/Users/artus/OneDrive/Документы/athlete_events.csv', sep=",", header=TRUE)
View(df)

v <- c("Name", "Sex", "Weight", "Sport")
df1 <- df[which(df[, "Sport"]=="Sailing"), v]
View(df1)
# удаление строчек с пустым значением поля "Вес"
df1<-df1[-which(is.na(df1[, "Weight"])),]
df1<-unique(df1)
View(df1)
x<-df1[1:nrow(df1), "Weight"]

# Тест Стьюдента.
t.test(x, mu=70, conf.int=TRUE)

# Тест Уилкоксона.
wilcox.test(x, mu=mean(x), conf.int=TRUE)

# Тест Шапиро-Уилкса (Shapiro-Wilk test).
shapiro.test(x)

# квантильно-квантильный график
qqnorm(x)
qqline(x, col=4, lwd=2)

# гистограмма 
x2<-seq(min(x), max(x), length=length(x))
fun<-dnorm(x2, mean=mean(x), sd=sd(x))
hist(x, freq=FALSE)
lines(x2, fun, col=2, lwd=2)

#Задание 2.
df2<-df[which(df[, "Sport"] %in% c("Sailing", "Football")), v]
df2<-df2[-which(is.na(df2[, "Weight"])),]
df2<-df2[which(df2[,"Sex"]=="F"),]
View(df2)

#install.packages("ISwR")
library(ISwR)

shapiro.test(df2[,"Weight"])

bartlett.test(Weight~Sport, data=df2)

t.test(Weight~Sport,data = df2)

