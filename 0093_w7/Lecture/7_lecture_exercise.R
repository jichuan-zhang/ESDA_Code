##_Week 7: Spatial network exercise 

##Exercise 1====
#use edge list to construct a nework
from <- c( 1,2,2,3,4)
to <- c(2,3,4,4,5)
edgelist<- data.frame( from, to)

library(igraph) #key package we will use
g<-graph.data.frame(edgelist, directed =FALSE) #make graph from edgelist
plot(g, vertex.size=20, vertex.frame.color='white', vertex.label.cex=1,
     edge.width=10) 

g2<-graph.data.frame(edgelist, directed =TRUE) #make graph from edgelist
plot.igraph(g2, vertex.size=20, vertex.frame.color='white', vertex.label.cex=1,
     edge.width=5, arrow.size = 50) 

##Exercise 2====
#construct a network and get the adjacency matrix
from <- c( 1,2,3,4,5,5,6)
to <- c(3,3,4,5,6,7,7)
edgelist<- data.frame( from, to)

g<-graph.data.frame(edgelist, directed =FALSE)
m <- as_adjacency_matrix(g)
View ( data.matrix(m) )
sum(m)


##Exercise 3====
from <- c(1,2,3,4,5,5,6)
to <- c(3,3,4,5,6,7,7)
edgelist<- data.frame(from, to)
g<-graph.data.frame(edgelist, directed =FALSE)
plot(g)

degree(g)
closeness(g)
betweenness(g)
page.rank(g)
