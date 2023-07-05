library(igraph)
N<-7

#1
G_size<-sample((N+10):((N%/%10+5)**2+5*N), 1)
g<-graph.ring(n=G_size)
ecount(g)
vcount(g) 
plot(g, edge.arrow.size=.2, vertex.size=13)
g[]

#2
g1<-graph.empty()+vertices(1:G_size, color='yellow')
g1<-g1+edges(sample(V(g1), 2*8*N, replace=TRUE), color='red')
plot(g1, edge.arrow.size=.2, vertex.size=13)
g1[]
g1<-g1+edges(sample(V(g1), 2*10*N, replace=TRUE), color='blue')
plot(g1, edge.arrow.size=.2, vertex.size=13)
g1[]

#3
v<-c(2*N+23, 2*N+20, 2*N+12, N+15, 2*N-1, N+8, 2*N, 2*N+1, N+7, N+13)
for (i in seq(1, length(v), 2)) {
  if (v[i] %in% V(g1) && v[i+1] %in% V(g1)) {
    g1<-add.edges(g1, c(v[i],v[i+1]), color='black')
  }
}
plot(g1, edge.arrow.size=.2, vertex.size=13)
neighbors(g1, V(g1)[N], mode='out')
incident(g1, V(g1)[N], mode='all')
if ((N+10) %in% V(g1) && (N+12) %in% V(g1)) {
  are.connected(g1, V(g1)[N+10], V(g1)[N+12])
}
g1[]

#4
x<-length(V(g1))+1
g1<-g1+vertices(x, color='green')
deg<-degree(g1, mode='all')
for (i in which(deg==max(deg))) {
  g1<-g1+edges(c(x,i, i,x), color='green')
}
v<-c(toupper(letters[1:26]), tolower(letters[1:26]))
mn<-min(length(V(g1)), length(v))
g1<-set_vertex_attr(g1, 'name', 1:mn, v[1:mn])
plot(g1, edge.arrow.size=.2, vertex.size1=13)
g1[]
v<-names(V(g1))
deg<-degree(g1, mode='all')
v[which(deg<5&deg>2)]

#5
coords<-layout_(g1, in_circle())
plot(g1, layout=coords, edge.arrow.size=.2)
coords<-layout_(g1, as_tree())
plot(g1, layout=coords, edge.arrow.size=.2)
g1<-graph.lattice(length=25,dim=1,nei=5, circular=FALSE)
plot(g1,vertex.size=2,vertex.label=NA,layout=layout.kamada.kawai,edge.arrow.size=.1)

#6
diameter(g1)
all_shortest_paths(g1, 1, to=V(g1), mode='all', weights=NULL)
deg<-degree(g1, mode='all')
plot(g1, edge.arrow.size=.2, vertex.size=deg)

#Часть 2 - задание 5
N <- 7
X <- c(0,1,0,0,0,1,1,
       1,0,1,0,0,0,0,
       0,1,0,0,1,1,0,
       0,0,0,0,0,0,0,
       0,0,1,0,0,1,0,
       1,0,1,0,1,0,1,
       1,0,0,0,0,1,0)
m <- matrix(X, nrow = N, ncol = N)
print(m)
color <- c(1,2,2,1,1,3,3)
print(color)
# создание неориентированного графа
g <- graph.adjacency(m, mode = "undirected")
V(g)$color <- "gray"
E(g)$color <- "gray"
plot(g)
# раскраска вершин
V(g)$color <- c("red", "blue", "green")[color]
plot(g)
# раскраска ребер
red <- V(g)[ color == "red" ]
blue <- V(g)[ color == "blue" ]
green <- V(g)[ color == "green" ]
E(g)[ red %--% red ]$color <- "red"
E(g)[ blue %--% blue ]$color <- "blue"
E(g)[ green %--% green ]$color <- "green"
plot(g)
# количество плохих мостов 
print(length(E(g)[ color == "gray" ]))
E(g)[ color == "gray" ]

