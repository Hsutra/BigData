df = read.csv("Топ-10 серий смешариков.csv",sep = ";",header = T,fileEncoding = "cp1251")

#Среднее значение
mean(df[,3])

#Медиана
median(df[,3])

#Мода
getmode <- function(x) 
{                      
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}
getmode(df[,3])

#Сортировка по выбранному признаку - среднему значению
c1 = sapply(na.omit(df[,3:12]), mean, 2)
c2 = c1[order(c1, decreasing = TRUE)]

c3 = colSums(df[3:12] >= 7) #Кол-во человек поставивших оценку 7 и выше

#Гистограмма
hist(c3, main="Кол-во человек поставивших оценку 7 и выше", xlab="Количество людей", ylab="Частота")

  #Боксплот
boxplot(c3, main="Боксплот")

boxplot(df[,3:12],main="Боксплот", col=rainbow(60),las=2)
