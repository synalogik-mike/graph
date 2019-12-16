\name{pageRank}
\alias{pageRank}
\docType{package}
\title{
The google pageRank Algorithm 
}
\description{
The Google pageRank algorithm 
returns a vector with the possibility that every single node has to be reached
}
\usage{
pageRank(NumericMatrix adjacencyMatrix)

where NumericMatrix adjacencyMatrix is the matrix returned by adjacencyMatrix() function 
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

vect<-pageRank(am)
}
}