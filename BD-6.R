# 6.1

# Чтение данных
filials <- read.table("C:/Users/artus/OneDrive/Документы/Филиалы.dat", header = TRUE,";", dec=',', fileEncoding="CP1251")

# Стандартизация данных
filials2 <-filials[,-c(1,4,5,7,8)]
filials2[,1:4]<-sapply(filials2[,1:4],as.numeric)
filials[,-c(1,4,5,7,8)]<-sapply(filials[,-c(1,4,5,7,8)],as.numeric)
filials2 <- scale(filials2, center = TRUE, scale = TRUE)
maxs <- apply(filials2[,1:5], 2, max)
mins <- apply(filials2[,1:5], 2, min)
filials2 <- scale(filials2, center = mins, scale = maxs - mins)
filials3<-data.frame(filials[,c(1,4,5,7,8)],filials2)

# создание матрицы попарных расстояний (по умолчанию - Евклидово расстояние)
dist.filials <- dist(filials3 [,6:10])

# проводим кластерный анализ, результаты записываются в список clust.filials, hclust ожидает матрицу расстояния, а не исходные данные.
clust.filials<- hclust(dist.filials, "ward.D")

# построение дендрограммы
plot(clust.filials, labels = filials$НАЗВАНИЕ)
rect.hclust(clust.filials, k = 4, border="red")
k = 4
# разделим Филиалы на 4 кластера, вектор groups содержит номер кластера, в который попал классифицируемый объект 
groups <- cutree(clust.filials, k) 

# вывод филиалов соответсвенно сформированным кластерам 
filials[groups==1, 1]
filials[groups==2, 1]
filials[groups==3, 1]
filials[groups==4, 1]

# для каждой группы определяем средние значения характеристик и строим датафрейм

# в 1-ом кластере
g1<-colMeans(filials[groups==1, c(2,3,6,9,10)])
# во 2-ом кластере
g2<-colMeans(filials[groups==2, c(2,3,6,9,10)])
# в 3-ем кластере
g3<-colMeans(filials[groups==3, c(2,3,6,9,10)])
# во 4-ом кластере
g4<-colMeans(filials[groups==4, c(2,3,6,9,10)])

# для общего графика
# в 1-ом кластере
g5<-colMeans(filials3[groups==1,6:10])
# во 2-ом кластере
g6<-colMeans(filials3[groups==2, 6:10])
# в 3-ем кластере
g7<-colMeans(filials3[groups==3, 6:10])
# в 4-ом кластере
g8<-colMeans(filials3[groups==4, 6:10])

# делаем дата фрейм из векторов групп кластеров
df<-data.frame(g1,g2,g3,g4);
df1<-t(df); 

df2<-data.frame(g5,g6,g7,g8);
df3<-t(df2);

barplot(as.matrix(df3), col=c("blue","red","green","yellow"))
legend("topright",
       legend = rownames(df3),
       fill = c("blue","red","green","yellow"),
       border = "black",
       cex=0.8)

barplot(df1[,1], ylim=range(pretty(c(0,max(df1[,1])))), 
        main="Площадь", 
        col=c("blue","red","green","yellow"))
legend("topright",
       legend = rownames(df1),
       fill = c("blue","red","green","yellow"),
       border = "black",
       cex=0.8)

barplot(df1[,2], ylim=range(pretty(c(0,max(df1[,2])))), 
        main="ПРОХОДИМОСТЬ", 
        col=c("blue","red","green","yellow"),legend=rownames(df1))

barplot(df1[,3], ylim=range(pretty(c(0,max(df1[,3])))), 
        main="МЕТРО", 
        col=c("blue","red","green","yellow"),legend=rownames(df1))

barplot(df1[,4], ylim=range(pretty(c(0,max(df1[,4])))), 
        main="ЦЕНЫ", 
        col=c("blue","red","green","yellow"),legend=rownames(df1))

barplot(df1[,5], ylim=range(pretty(c(0,max(df1[,5])))), 
        main="ПРОДАЖИ", 
        col=c("blue","red","green","yellow"),legend=rownames(df1))


# построение диаграммы "Каменная осыпь"
plot(1:19, clust.filials$height, type='b',xlab="Номер компоненты",ylab = "Собственное значение") 


library (lattice)

df <-read.table("C:/Users/artus/OneDrive/Документы/Филиалы.dat", header = TRUE,";", dec=',', fileEncoding="CP1251")

df[,c(2,3,6,9,10)]<-sapply(df[,c(2,3,6,9,10)],as.numeric)

my_data <- df
my_data["Group"]<-groups

# вывод графика рассеяния с минимальным количеством параметров с выделением имени
xyplot(ПРОДАЖИ ~ ПРОХОДИМ,group = Group, data = my_data,auto.key = TRUE,pch = 20,cex = 1.5)

# боксплот, отражающий характеристики классов типов 
boxplot(ПРОДАЖИ ~ Group, data =my_data, xlab="КЛАСТЕРЫ",ylab = "ПРОДАЖИ", frame = FALSE, col=c("blue","red","green","yellow"))

library(scatterplot3d)

# построение трехмерного графика наших классов
cloud(ПРОДАЖИ~ЦЕНЫ*МЕТРО, group = Group, data = my_data, auto.key = TRUE,pch = 20,cex = 1.5) 

# 6.2
#install.packages("klaR")
library(klaR)

my_data$Group<- c(as.factor(groups))

naive_df <- NaiveBayes(my_data$Group ~ ., data = my_data) 
naive_df$tables 
naive_df$tables$ПРОДАЖИ
naive_df

# построение графиков по Байесу
opar=par() 
layout(matrix(c(1,2),2,2, byrow = TRUE)) 
plot(naive_df,lwd = 2, legendplot=TRUE,cex=0.1)
#восстановление
par=opar

set.seed(1234)
ind <- sample(2, nrow(my_data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- my_data[ind==1,]
testData <- my_data[ind==2,] 
nrow(trainData)
nrow(testData)
nrow(my_data)


library(party)
myFormula <- Group ~ ПРОХОДИМ + ЦЕНЫ + ПРОДАЖИ
df_ctree <- ctree(myFormula, data=trainData)
df_ctree
table(predict(df_ctree),trainData$Group)
predict(df_ctree)
plot(predict(df_ctree))
plot(df_ctree)


# алгоритм Random Forest 
library(randomForest) 
rf <- randomForest(Group ~ .,data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$Group)
print(rf)

