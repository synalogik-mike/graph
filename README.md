# MyGraph: an Rcpp package for work with graph

## Table of Contents
1. [ Introduction. ](#introduction)
1. [ Prerequisite. ](#prerequisite)
1. [ ArangoDB basic concepts. ](#graphconcepts)
1. [ Install the package. ](#installation)
1. [ Usage examples. ](#usage)
    * [ Create Adjacency Matrix of Graph. ](#adj)
    * [ Create Adjacency Matrix of a graph with weighted edge . ](#weightAdj)
    * [ Warshall. ](#warsh)
    * [ Dijkstra. ](#dijkstra)
    * [ Number of Graph component. ](#component)
    * [ Kruskal. ](#kruskal)
    * [ Prim. ](#prim)
    * [ Pagerank. ](#pagerank)
    * [ Bellman Ford. ](#bellmanford)
    * [ Boruvka. ](#boruvka)
1. [ Functions and classes summary. ](#summary)
1. [ Bugs and known limitations. ](#bugslimitations)
1. [ Roadmap. ](#roadmap)
1. [ Citation. ](#citation)


## Introduction <a name="introduction"></a>
This package allows you to create adjacency matrix of weighted or not weighted, direct or not direct and strong or not strong graphs, and work with most famous graph algorithms.
In this case we use aRangodb library for create graph and retrieve data from them.

## Prerequisite <a name="prerequisite"></a>
aRangodb library is necessary for create the adjacency matrix of the graph.

* **aRangodb** https://gitlab.com/krpack/arango-driver

## ArangoDB basic concepts <a name="graphconcepts"></a>
Since that ArangoDB is a multimodel database, it works with graphs.
That said there is the possibility to commute this graph into adjacency matrix of the graph.
With aRangodb library there is also the possibility to return the graph desired part.



## Install the package <a name="installation"></a>
To install the latest version of the aRangodb package you have to run the following commands:

```R
devtools::install_gitlab("krpack/gRaph")
library(MyGraph)
```



## Usage examples <a name="usage"></a>
In the following sections are shown usage's examples of this package. The complete example, as R script, is located into the repository at the path "examples/packagetest.R".
Once installed you have to load the package:

```R
library(aRangodb)
library(MyGraph)
```

### Create Adjacency Matrix of Graph <a name="adj"></a>
 
 Connect to an ArangoDB server up running, load data into collections and create graph with edgem then do the traversal of the graph and use getAdjacencyTensor( ) function as show in .R file in the EXAMPLES folder .

```R
adjacency <- all.graph$getAdjacencyTensor()
matrix<-adjacencyMatrix(adjacency)
```

### Create Adjacency Matrix of a graph with weighted edge <a name="weightAdj"></a>

If you want to use a graph with weighted edge you have to the traversal of the graph and return the values stored on the edges. 
For do this you have to do an AQL query, this is possible with aRangodb library.

```R
pesato<- mydb %>% aql("FOR c IN city
                        FOR v,e IN OUTBOUND c._id
                        GRAPH 'distance'
                        OPTIONS {bfs: true, uniqueEdges : 'path', uniqueVertices : 'path'} FILTER e.distance != null RETURN {dist:e.distance, from:e._from, to:e._to }")
peso<- pesato()
MatrixPesata <- weight(peso,matrix)
```


### Warshall <a name="warsh"></a>

Warshall algorithm is an algorithm for finding shortest paths in a weighted graph with positive or negative edge weights (but with no negative cycles).

```R
warshall<-floydWarshall(MatrixPesata)
View(warshall)
```


### Dijkstra <a name="dijkstra"></a>
Dijkstra's algorithm is an algorithm for finding the shortest paths between nodes in a graph. 
In this case we passed at the function the name of the vertex node where Dijkstra start.


```R
s<-"city/Pontedera"
dijkstraMatrix<-dijkstra(MatrixPesata,s)
View(dijkstraMatrix)
```

### Number of Graph component <a name="component"></a>

This function return the number of component of direct or not direct graphs

```R
componentiConnesse<-component(matrix)
View(componentiConnesse)
```

### Kruskal  <a name="kruskal"></a>
Kruskal algorithm is a good algorithm for find Minimum Spanning Tree (MST) of a weighted graph, direct or not direct.

```R
kruskal<-Kruskal(MatrixPesata,matrix)
```


### Prim <a name="prim"></a>
Kruskal algorithm is a good algorithm for find Minimum Spanning Tree (MST) of a direct weighted graph.

```R
Prim<-prim(MatrixPesata,matrix)
```


### Pagerank  <a name="pagerank"></a>

The Google pageRank algorithm returns a vector with the possibility that every single node has to be reached

```R
pagerank<-pageRank(matrix)
```

### Bellman Ford  <a name="bellmanford"></a>
The Bellman–Ford algorithm is an algorithm that computes shortest paths from a single source vertex to all of the other vertices in a weighted digraph.

```R
bellmanFORD<-BellmanFord(MatrixPesata,s)
```

### Boruvka  <a name="boruvka"></a>
Borůvka's algorithm is a greedy algorithm for finding a minimum spanning tree in a graph for which all edge weights are distinct, or a minimum spanning forest in the case of a graph that is not connected.

```R
boruvka<-Boruvka(MatrixPesata,matrix)
```


## Functions and Class Summary <a name="summary"></a>

Create adjacency matrix of a graph direct or not direct.
The return type is matrix

```R
adjacency <- all.graph$getAdjacencyTensor()
matrix<-adjacencyMatrix(adjacency)
```

Create adjacency matrix of a weighted graph direct or not direct.
The return type is matrix.

```R
pesato<- mydb %>% aql("FOR c IN city
                        FOR v,e IN OUTBOUND c._id
                        GRAPH 'distance'
                        OPTIONS {bfs: true, uniqueEdges : 'path', uniqueVertices : 'path'} FILTER e.distance != null RETURN {dist:e.distance, from:e._from, to:e._to }")
peso<- pesato()
MatrixPesata <- weight(peso,matrix)
```

Return Warshall matrix :
```R
warshall<-floydWarshall(MatrixPesata)
```

Return Dijkstra matrix :
The returned type is "list" and the number of list elements depend by the number of components of the graph

```R
s<-"city/Pontedera"
Dijkstra<-dijkstra(MatrixPesata,s)
```
Return the number of component of a graph :

```R
comp <- component(matrix)
```

Return Prim matrix :
The returned type is "list" and the number of list elements depend by the number of components of the graph.

```R
Prim<-prim(MatrixPesata,matrix)
```

Return PageRank vector :
The returned type is "list" and the number of list elements depend by the number of component of the graph.

```R
pagerank<-pageRank(matrix)
```
Return Bellman Ford matrix :
The returned type is "list" and the number of list elements depend by the number of component of the graph.

```R
bellmanFORD<-BellmanFord(MatrixPesata,s)
```

Return Boruvka matrix :
The returned type is "list" and the number of list elements depend by the number of component of the graph.

```R
Boruvka<-boruvka(MatrixPesata,matrix)
```

## Bugs and Knonw Limitations <a name="bugslimitations"></a>


## Roadmap <a name="roadmap"></a>
The following is an uncomplete frequently-updated list to recap the major features in plan for the next versions.

* Version **0.0.1**:
    * First release of the package


## Citation <a name="citation"></a>

  Matteo Sabatino (2019). MyGraph: an Rcpp package for work with graph

A BibTeX entry for LaTeX users is

@Manual{
  title = {MyGraph: an Rcpp package for work with graph},
  author = {Matteo Sabatino},
  organization = {Kode Srl},
  address = {Pisa, Italia},
  year = {2019},
  url = {www.kode-solutions.net}
}
