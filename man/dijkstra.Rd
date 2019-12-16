\name{Dijkstra}
\alias{dijkstra}
\docType{package}
\title{
Dijkstra Algorithm 
}
\description{
function to create dijkstra Matrix and cost from the node passed as string
}
\usage{
s<-"graph_node_name"
dijkstra(NumericMatrix adjacencyweigth , s)

where NumericMatrix adjacencyMatrix is the matrix returned by adjacencyMatrix() function 
}
\examples{
\dontrun{
library("aRangodb")
library(Rcpp)
library("MyGraph")

#db connection and graph creation 
#create your own adjacency matrix with adjacencyMatrix() if you're in doubt ?adjacencyMatrix() 
will help you

input<-getAdjacencyTensor()
am<-adjacencyMatrix(input)

s<-"city/Pontedera"
dijkstra(am,s)
}
}