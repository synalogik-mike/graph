#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pagerank(NumericMatrix adjacencymatrix,int comp, int nodes){
    //mi occorre matrice di adiacenza di un grafo orientato
    int n=adjacencymatrix.nrow();
    NumericVector archiuscenti(n);
    NumericVector pagerank(n);
    //CharacterVector rn=rownames(adjacencymatrix);
    //pagerank.names()=rn;
    int contarchi=0;
    int i,j;
    for(i=0;i<n;i++){
        pagerank(i)=0; //inizializzo vettore pagerank a 0
    }
    //int cont=0;
    for(i=0;i<n;i++){
        for(j=0;j<n;j++){
            if(adjacencymatrix(i,j)==comp){
                contarchi++;//conto gli archi uscenti da quel nodo
                //cont++;//conto il numero di archi
            }
        }
        archiuscenti(i)=contarchi;//inizializzo il vettore archi uscenti in base al numero di archi uscenti da quel nodo
        contarchi=0;
    }
    double percentuale=1.00/nodes; // calcolo percentuale come 1 fratto numero di nodi della componente
    double rank=0.00; //sommatoria rank
    
    for(i=0;i<n;i++){ //scorro la matrice di adiacenza
        for(j=0;j<n;j++){
            if(adjacencymatrix(j,i)==comp){ //se c'è un arco in ingresso al nodo i lo prendo in considerazione :
                rank+=percentuale/archiuscenti(j); //al rank del nodo i sommo la percentuale diviso il numero di archi uscenti dal nodo(j) (j nodo da cui arriva l'arco a i)
            }
        }
        pagerank(i)=rank;//al vettore page rank in posizione del nodo i associo il valore page rank
        rank=0.00;
    }
    return pagerank;
}

// [[Rcpp::export]]
void dfs2 (int x, NumericMatrix g, NumericVector vis1,int cont){
    int n1=g.nrow(); //numero di nodi
    vis1(x)=cont; //setto come visitato il nodo che ho passato come radice di dfs
    int j;
    for(j=0;j<n1;j++){
        if((g(x,j)==1)&(vis1(j)==0)){ //se non ho ancora visitato il nodo j e c'è un arco tra il nodo x (radice dfs) e j allora vuol dire che c'è un collegamento e quindi eseguo dfs su quel nodo.
            g(x,j)=cont; //metto il numero della componente al posto del semplice 1 = arco ci sarà componente = arco
            g(j,x)=cont;
            dfs2(j,g,vis1,cont);
            }
        }
    }


// [[Rcpp::export]]
int component2(NumericMatrix adjacency,NumericVector vis){
    int cont=0;
    int n=adjacency.nrow();
    
    NumericMatrix nonorientato(n,n); //mi serve un grafo non orientato per fare dfs
    int i,j;
    for(i=0;i<n;i++){
        for(j=0;j<n;j++){
            nonorientato(i,j)=0;
        }
    }
    for(i=0;i<n;i++){
        for(j=0;j<n;j++){
            if(adjacency(i,j)==1){
                nonorientato(i,j)=1;
                nonorientato(j,i)=1;
            }
        }
    }
    for(i=0;i<n;i++){
        vis(i)=0;
    }
    for(i=0;i<n;i++){
        if(vis(i)==0){
            cont++;
            dfs2(i,nonorientato,vis,cont);
        }
    }
    for(i=0;i<n;i++){
        for(j=0;j<n;j++){
            if((adjacency(i,j)==1)&(nonorientato(i,j)>0)){
                adjacency(i,j)=nonorientato(i,j); //modifico la matrice di adiacenza del grafo ORIENTATO con il numero della componente al posto del semplice 1, a me serve matrice adiacenza grafo ORIENTATO
            }
        }
    }
        if(cont==n){
        cont=0;
    }
    return cont;
}


// [[Rcpp::export]]
List pageRank (NumericMatrix adjacencymatrix){
    int dim=adjacencymatrix.nrow();
    NumericMatrix support(dim,dim);
    //CharacterVector rn,cn;
    NumericVector vis(dim);
    /*rn=rownames(adjacencymatrix);
    cn=colnames(adjacencymatrix);
    rownames(support)=rn;
    colnames(support)=cn;
     */
    int i,j;
    for(i=0;i<dim;i++){
        vis(i)=0;
        for(j=0;j<dim;j++){
            support(i,j)=adjacencymatrix(i,j);
        }
    }
    int n=component2(support,vis);
    List prl;
    if(n==0){
        NumericVector pr(dim);
        for(i=0;i<dim;i++){
            pr(i)=0.00;
        }
        prl=List::create(pr);
        
    }else{
        int contanodi=0;
        for(i=0;i<n;i++){
            for(j=0;j<dim;j++){
                if(vis(j)==i+1){
                    contanodi++;
                }
            }
            prl.push_back(pagerank(support,i+1,contanodi));
            contanodi=0;
        }
    }
    return prl;
}
