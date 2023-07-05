#США, Китай, Россия, Казахстан, Белоруссия

library(rvest)

#Сбор данных
url_21<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021')
url_20<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020')
url_19<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2019')
url_18<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2018')
url_17<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2017')
url_16<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2016')
url_15<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015')
url_14<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014')

nodes_21<-html_nodes(url_21, 'table')
nodes_20<-html_nodes(url_20, 'table')
nodes_19<-html_nodes(url_19, 'table')
nodes_18<-html_nodes(url_18, 'table')
nodes_17<-html_nodes(url_17, 'table')
nodes_16<-html_nodes(url_16, 'table')
nodes_15<-html_nodes(url_15, 'table')
nodes_14<-html_nodes(url_14, 'table')

df_21<-html_table(nodes_21[[2]])%>%as.data.frame()
df_20<-html_table(nodes_20[[2]])%>%as.data.frame()
df_19<-html_table(nodes_19[[2]])%>%as.data.frame()
df_18<-html_table(nodes_18[[2]])%>%as.data.frame()
df_17<-html_table(nodes_17[[2]])%>%as.data.frame()
df_16<-html_table(nodes_16[[2]])%>%as.data.frame()
df_15<-html_table(nodes_15[[2]])%>%as.data.frame()
df_14<-html_table(nodes_14[[2]])%>%as.data.frame()

# присвоения имен строкам датафрейма 
rownames(df_21)<-df_21[, 2]
rownames(df_20)<-df_20[, 2]
rownames(df_19)<-df_19[, 2]
rownames(df_18)<-df_18[, 2]
rownames(df_17)<-df_17[, 2]
rownames(df_16)<-df_16[, 2]
rownames(df_15)<-df_15[, 2]
rownames(df_14)<-df_14[, 2]

# выбор столбцов в датафрейме с оценками общего качетсва жизни
df_21<-df_21[, 3:11]
df_20<-df_20[, 3:11]
df_19<-df_19[, 3:11]
df_18<-df_18[, 3:11]
df_17<-df_17[, 3:11]
df_16<-df_16[, 3:11]
df_15<-df_15[, 3:11]
df_14<-df_14[, 3:11]

# выбор стран согласно варианту
v = c("United States", "China", "Russia", "Kazakhstan", "Belarus")
years<-2014:2021
# оценка индекса качества жизни
s = 'Quality of Life Index'
QLI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names = 2014:2021
)
colnames(QLI) = v

min<-min(QLI, na.rm=TRUE)
max<-max(QLI, na.rm=TRUE)
plot(years, QLI$'United States', xlab='Года', ylab='Индекс качества жизни', ylim=c(min,max), main='Оценка индекса качества жизни', col='blue', type='b')
lines(years, QLI$'China', type='b', col='red')
lines(years, QLI$'Russia', type='b', col='green')
lines(years, QLI$'Kazakhstan', type='b', col='yellow')
lines(years, QLI$'Belarus', type='b', col='orange')
legend('bottomright', cex=0.5, v, fill=c('blue', 'red', 'green', 'yellow', 'orange'))

# оценка индекс покупательной способности
s<-'Purchasing Power Index'
PPI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPI)=v
min<-min(PPI, na.rm=TRUE)
max<-max(PPI, na.rm=TRUE)
plot(years, PPI$'United States', xlab='Года', ylab='Индекс покупательной способности', ylim=c(min, max), main='Оценка индекса покупательной способности', col='blue', type='b')
lines(years, PPI$'China', type='b', col='red')
lines(years, PPI$'Russia', type='b', col='green')
lines(years, PPI$'Kazakhstan', type='b', col='yellow')
lines(years, PPI$'Belarus', type='b', col='orange')
legend('bottomright', cex=0.5, v, fill=c('blue', 'red', 'green', 'yellow', 'orange'))

# оценка индекс безопасности
s<-'Safety Index'
PPI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPI)=v
min<-min(PPI, na.rm=TRUE)
max<-max(PPI, na.rm=TRUE)
plot(years, PPI$'United States', xlab='Года', ylab='Индекс безопасности', ylim=c(min, max), main='Оценка индекса безопасности', col='blue', type='b')
lines(years, PPI$'China', type='b', col='red')
lines(years, PPI$'Russia', type='b', col='green')
lines(years, PPI$'Kazakhstan', type='b', col='yellow')
lines(years, PPI$'Belarus', type='b', col='orange')
legend('bottomright', cex=0.5, v, fill=c('blue', 'red', 'green', 'yellow', 'orange'))


# индекс здравоохранения
s<-'Health Care Index'
PPI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPI)=v
min<-min(PPI, na.rm=TRUE)
max<-max(PPI, na.rm=TRUE)
plot(years, PPI$'United States', xlab='Года', ylab='Индекс дравоохранения', ylim=c(min, max), main='Оценка индекса здравоохранения', col='blue', type='b')
lines(years, PPI$'China', type='b', col='red')
lines(years, PPI$'Russia', type='b', col='green')
lines(years, PPI$'Kazakhstan', type='b', col='yellow')
lines(years, PPI$'Belarus', type='b', col='orange')
legend('bottomright', cex=0.5, v, fill=c('blue', 'red', 'green', 'yellow', 'orange'))

# индекс стоимости жизни
s<-'Cost of Living Index'
PPI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPI)=v
min<-min(PPI, na.rm=TRUE)
max<-max(PPI, na.rm=TRUE)
plot(years, PPI$'United States', xlab='Года', ylab='Индекс стоимости жизни', ylim=c(min, max), main='Оценка индекса стоимости жизни', col='blue', type='b')
lines(years, PPI$'China', type='b', col='red')
lines(years, PPI$'Russia', type='b', col='green')
lines(years, PPI$'Kazakhstan', type='b', col='yellow')
lines(years, PPI$'Belarus', type='b', col='orange')
legend('bottomright', cex=0.5, v, fill=c('blue', 'red', 'green', 'yellow', 'orange'))

# Какая-то батва индекс
s<-'Property Price to Income Ratio'
PPI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPI)=v
min<-min(PPI, na.rm=TRUE)
max<-max(PPI, na.rm=TRUE)
plot(years, PPI$'United States', xlab='Года', ylab='Соотношение цены недвижимости к доходу', ylim=c(min, max), main='Оценка соотношения цены недвижимости к доходу', col='blue', type='b')
lines(years, PPI$'China', type='b', col='red')
lines(years, PPI$'Russia', type='b', col='green')
lines(years, PPI$'Kazakhstan', type='b', col='yellow')
lines(years, PPI$'Belarus', type='b', col='orange')
legend('bottomright', cex=0.5, v, fill=c('blue', 'red', 'green', 'yellow', 'orange'))


# Индекс времени в пути на работу
s<-'Traffic Commute Time Index'
PPI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPI)=v
min<-min(PPI, na.rm=TRUE)
max<-max(PPI, na.rm=TRUE)
plot(years, PPI$'United States', xlab='Года', ylab='Индекс времени в пути на работу', ylim=c(min, max), main='Оценка индекса времени в пути на работу', col='blue', type='b')
lines(years, PPI$'China', type='b', col='red')
lines(years, PPI$'Russia', type='b', col='green')
lines(years, PPI$'Kazakhstan', type='b', col='yellow')
lines(years, PPI$'Belarus', type='b', col='orange')
legend('bottomright', cex=0.5, v, fill=c('blue', 'red', 'green', 'yellow', 'orange'))


# Индекс загрязнения
s<-'Pollution Index'
PPI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPI)=v
min<-min(PPI, na.rm=TRUE)
max<-max(PPI, na.rm=TRUE)
plot(years, PPI$'United States', xlab='Года', ylab='Индекс загрязнения', ylim=c(min, max), main='Оценка индекса загрязнения', col='blue', type='b')
lines(years, PPI$'China', type='b', col='red')
lines(years, PPI$'Russia', type='b', col='green')
lines(years, PPI$'Kazakhstan', type='b', col='yellow')
lines(years, PPI$'Belarus', type='b', col='orange')
legend('bottomright', cex=0.5, v, fill=c('blue', 'red', 'green', 'yellow', 'orange'))

#----
url<-read_html("https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/")
selector_name<-".addressItem--single"
fnames<-html_nodes(url, selector_name) %>% html_text()%>%as.array(); fnames
fpos<-html_nodes(url, selector_name) %>% html_text()%>%as.array();fpos

selector_name<-".post-list-item-title-link"
fnames_addr<-html_nodes(url, selector_name) %>% html_attr("href"); fnames_addr
data = data.frame(fnames[1:37],fpos,fnames_addr[1:37]);data
colnames(data) = c("Название","Адрес","Ссылка")                                          
View(data)
