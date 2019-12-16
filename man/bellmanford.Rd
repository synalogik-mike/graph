\name{BellmanFord}
\alias{bellmanford}
\docType{package}
\title{
The Bellman Ford algorithm 
}
\description{
The Bellman–Ford algorithm is an algorithm that computes shortest paths from a single source vertex to all of the other vertices in a weighted Graph
}
\usage{
bellmanford(NumericMatrix graph, CharacterVector source) 
Where NumericMatrix Graph is the matrix returned by weigth () function and CharacterVector source 
is the name of the vertex node.
This function return a list of 3 element, the first element is the cost vector for every connected 
node to source the second vector is the parent vector that represent the parent nodes 
of i vector element and the third element is a flag that represent if there are negative cicle. 
If is set to 1 then the graph have negative cicle.
}
\examples{
\dontrun{
library("aRangodb")
library(Rcpp)
library("MyGraph")

#db connection and graph creation 
#create your own adjacency matrix with adjacencyMatrix() if you're in doubt
 ?adjacencyMatrix() will help you

input<-getAdjacencyTensor()
am<-adjacencyMatrix(input)

#query MUST return values on the edge and FROM and TO parameters

weigthMatrix<-weight(query,am)

BellmanFord <- belmanford(weigthMatrix,"city/Pontedera")
}
}
