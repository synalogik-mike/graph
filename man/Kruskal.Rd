\name{Kruskal}
\alias{Kruskal}
\docType{package}
\title{
Kruskal algorithm
}
\description{
Return the MST even if graph have more than one component  
}
\usage{
kruskal <- Kruskal (NumericMatrix weigth, NumericMatrix adjacencyMatrix)
}
\examples{
\dontrun{
library("aRangodb")
library(Rcpp)
#db connection and graph creation 
#create your own adjacency matrix with adjacencyMatrix() if you're in doubt
 ?adjacencyMatrix() will help you

input<-getAdjacencyTensor()
am<-adjacencyMatrix(input)

#query MUST return values on the edge and FROM and TO parameters

weigthMatrix<-weight(query,am)

kruskal <- Kruskal(weigthMatrix,am)
}
}

