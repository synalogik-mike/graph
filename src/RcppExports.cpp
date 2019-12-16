// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// dfs3
void dfs3(int x, NumericMatrix g, NumericVector vis1, int comp, int& cont);
RcppExport SEXP _MyGraph_dfs3(SEXP xSEXP, SEXP gSEXP, SEXP vis1SEXP, SEXP compSEXP, SEXP contSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type g(gSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vis1(vis1SEXP);
    Rcpp::traits::input_parameter< int >::type comp(compSEXP);
    Rcpp::traits::input_parameter< int& >::type cont(contSEXP);
    dfs3(x, g, vis1, comp, cont);
    return R_NilValue;
END_RCPP
}
// find1
int find1(NumericMatrix subset, int i);
RcppExport SEXP _MyGraph_find1(SEXP subsetSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type subset(subsetSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(find1(subset, i));
    return rcpp_result_gen;
END_RCPP
}
// Union
void Union(NumericMatrix subset, int x, int y);
RcppExport SEXP _MyGraph_Union(SEXP subsetSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type subset(subsetSEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    Union(subset, x, y);
    return R_NilValue;
END_RCPP
}
// boruvka
List boruvka(NumericMatrix graph, NumericMatrix adj, int comp);
RcppExport SEXP _MyGraph_boruvka(SEXP graphSEXP, SEXP adjSEXP, SEXP compSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< int >::type comp(compSEXP);
    rcpp_result_gen = Rcpp::wrap(boruvka(graph, adj, comp));
    return rcpp_result_gen;
END_RCPP
}
// Boruvka
List Boruvka(NumericMatrix matrixpeso, NumericMatrix adjacencymatrix);
RcppExport SEXP _MyGraph_Boruvka(SEXP matrixpesoSEXP, SEXP adjacencymatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type matrixpeso(matrixpesoSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type adjacencymatrix(adjacencymatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(Boruvka(matrixpeso, adjacencymatrix));
    return rcpp_result_gen;
END_RCPP
}
// adjacencyMatrix
NumericMatrix adjacencyMatrix(List input);
RcppExport SEXP _MyGraph_adjacencyMatrix(SEXP inputSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type input(inputSEXP);
    rcpp_result_gen = Rcpp::wrap(adjacencyMatrix(input));
    return rcpp_result_gen;
END_RCPP
}
// BellmanFord
List BellmanFord(NumericMatrix graph, CharacterVector source);
RcppExport SEXP _MyGraph_BellmanFord(SEXP graphSEXP, SEXP sourceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type source(sourceSEXP);
    rcpp_result_gen = Rcpp::wrap(BellmanFord(graph, source));
    return rcpp_result_gen;
END_RCPP
}
// dfs
void dfs(int x, NumericMatrix g, NumericVector vis1);
RcppExport SEXP _MyGraph_dfs(SEXP xSEXP, SEXP gSEXP, SEXP vis1SEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type g(gSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vis1(vis1SEXP);
    dfs(x, g, vis1);
    return R_NilValue;
END_RCPP
}
// component
int component(NumericMatrix m);
RcppExport SEXP _MyGraph_component(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(component(m));
    return rcpp_result_gen;
END_RCPP
}
// ricercamin
int ricercamin(NumericVector p, NumericVector u, int n1);
RcppExport SEXP _MyGraph_ricercamin(SEXP pSEXP, SEXP uSEXP, SEXP n1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type u(uSEXP);
    Rcpp::traits::input_parameter< int >::type n1(n1SEXP);
    rcpp_result_gen = Rcpp::wrap(ricercamin(p, u, n1));
    return rcpp_result_gen;
END_RCPP
}
// dijkstra
List dijkstra(NumericMatrix w, CharacterVector src);
RcppExport SEXP _MyGraph_dijkstra(SEXP wSEXP, SEXP srcSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type w(wSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type src(srcSEXP);
    rcpp_result_gen = Rcpp::wrap(dijkstra(w, src));
    return rcpp_result_gen;
END_RCPP
}
// floydWarshall
NumericMatrix floydWarshall(NumericMatrix graph);
RcppExport SEXP _MyGraph_floydWarshall(SEXP graphSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type graph(graphSEXP);
    rcpp_result_gen = Rcpp::wrap(floydWarshall(graph));
    return rcpp_result_gen;
END_RCPP
}
// find
int find(int index, NumericVector parenti);
RcppExport SEXP _MyGraph_find(SEXP indexSEXP, SEXP parentiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type index(indexSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type parenti(parentiSEXP);
    rcpp_result_gen = Rcpp::wrap(find(index, parenti));
    return rcpp_result_gen;
END_RCPP
}
// union1
int union1(int x, int y, NumericVector parenti);
RcppExport SEXP _MyGraph_union1(SEXP xSEXP, SEXP ySEXP, SEXP parentiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type parenti(parentiSEXP);
    rcpp_result_gen = Rcpp::wrap(union1(x, y, parenti));
    return rcpp_result_gen;
END_RCPP
}
// chiamakruskal
List chiamakruskal(NumericMatrix weigth, NumericMatrix adj, int comp);
RcppExport SEXP _MyGraph_chiamakruskal(SEXP weigthSEXP, SEXP adjSEXP, SEXP compSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type weigth(weigthSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< int >::type comp(compSEXP);
    rcpp_result_gen = Rcpp::wrap(chiamakruskal(weigth, adj, comp));
    return rcpp_result_gen;
END_RCPP
}
// Kruskal
List Kruskal(NumericMatrix matrixpeso, NumericMatrix adjacencymatrix);
RcppExport SEXP _MyGraph_Kruskal(SEXP matrixpesoSEXP, SEXP adjacencymatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type matrixpeso(matrixpesoSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type adjacencymatrix(adjacencymatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(Kruskal(matrixpeso, adjacencymatrix));
    return rcpp_result_gen;
END_RCPP
}
// weight
NumericMatrix weight(List input, NumericMatrix matr);
RcppExport SEXP _MyGraph_weight(SEXP inputSEXP, SEXP matrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type matr(matrSEXP);
    rcpp_result_gen = Rcpp::wrap(weight(input, matr));
    return rcpp_result_gen;
END_RCPP
}
// pagerank
NumericVector pagerank(NumericMatrix adjacencymatrix, int comp, int nodes);
RcppExport SEXP _MyGraph_pagerank(SEXP adjacencymatrixSEXP, SEXP compSEXP, SEXP nodesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adjacencymatrix(adjacencymatrixSEXP);
    Rcpp::traits::input_parameter< int >::type comp(compSEXP);
    Rcpp::traits::input_parameter< int >::type nodes(nodesSEXP);
    rcpp_result_gen = Rcpp::wrap(pagerank(adjacencymatrix, comp, nodes));
    return rcpp_result_gen;
END_RCPP
}
// dfs2
void dfs2(int x, NumericMatrix g, NumericVector vis1, int cont);
RcppExport SEXP _MyGraph_dfs2(SEXP xSEXP, SEXP gSEXP, SEXP vis1SEXP, SEXP contSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type g(gSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vis1(vis1SEXP);
    Rcpp::traits::input_parameter< int >::type cont(contSEXP);
    dfs2(x, g, vis1, cont);
    return R_NilValue;
END_RCPP
}
// component2
int component2(NumericMatrix adjacency, NumericVector vis);
RcppExport SEXP _MyGraph_component2(SEXP adjacencySEXP, SEXP visSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adjacency(adjacencySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vis(visSEXP);
    rcpp_result_gen = Rcpp::wrap(component2(adjacency, vis));
    return rcpp_result_gen;
END_RCPP
}
// pageRank
List pageRank(NumericMatrix adjacencymatrix);
RcppExport SEXP _MyGraph_pageRank(SEXP adjacencymatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adjacencymatrix(adjacencymatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(pageRank(adjacencymatrix));
    return rcpp_result_gen;
END_RCPP
}
// chiamaprim
List chiamaprim(NumericMatrix graph, NumericMatrix adj, int comp);
RcppExport SEXP _MyGraph_chiamaprim(SEXP graphSEXP, SEXP adjSEXP, SEXP compSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< int >::type comp(compSEXP);
    rcpp_result_gen = Rcpp::wrap(chiamaprim(graph, adj, comp));
    return rcpp_result_gen;
END_RCPP
}
// prim
List prim(NumericMatrix matrixpeso, NumericMatrix adjacencymatrix);
RcppExport SEXP _MyGraph_prim(SEXP matrixpesoSEXP, SEXP adjacencymatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type matrixpeso(matrixpesoSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type adjacencymatrix(adjacencymatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(prim(matrixpeso, adjacencymatrix));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MyGraph_dfs3", (DL_FUNC) &_MyGraph_dfs3, 5},
    {"_MyGraph_find1", (DL_FUNC) &_MyGraph_find1, 2},
    {"_MyGraph_Union", (DL_FUNC) &_MyGraph_Union, 3},
    {"_MyGraph_boruvka", (DL_FUNC) &_MyGraph_boruvka, 3},
    {"_MyGraph_Boruvka", (DL_FUNC) &_MyGraph_Boruvka, 2},
    {"_MyGraph_adjacencyMatrix", (DL_FUNC) &_MyGraph_adjacencyMatrix, 1},
    {"_MyGraph_BellmanFord", (DL_FUNC) &_MyGraph_BellmanFord, 2},
    {"_MyGraph_dfs", (DL_FUNC) &_MyGraph_dfs, 3},
    {"_MyGraph_component", (DL_FUNC) &_MyGraph_component, 1},
    {"_MyGraph_ricercamin", (DL_FUNC) &_MyGraph_ricercamin, 3},
    {"_MyGraph_dijkstra", (DL_FUNC) &_MyGraph_dijkstra, 2},
    {"_MyGraph_floydWarshall", (DL_FUNC) &_MyGraph_floydWarshall, 1},
    {"_MyGraph_find", (DL_FUNC) &_MyGraph_find, 2},
    {"_MyGraph_union1", (DL_FUNC) &_MyGraph_union1, 3},
    {"_MyGraph_chiamakruskal", (DL_FUNC) &_MyGraph_chiamakruskal, 3},
    {"_MyGraph_Kruskal", (DL_FUNC) &_MyGraph_Kruskal, 2},
    {"_MyGraph_weight", (DL_FUNC) &_MyGraph_weight, 2},
    {"_MyGraph_pagerank", (DL_FUNC) &_MyGraph_pagerank, 3},
    {"_MyGraph_dfs2", (DL_FUNC) &_MyGraph_dfs2, 4},
    {"_MyGraph_component2", (DL_FUNC) &_MyGraph_component2, 2},
    {"_MyGraph_pageRank", (DL_FUNC) &_MyGraph_pageRank, 1},
    {"_MyGraph_chiamaprim", (DL_FUNC) &_MyGraph_chiamaprim, 3},
    {"_MyGraph_prim", (DL_FUNC) &_MyGraph_prim, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_MyGraph(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}