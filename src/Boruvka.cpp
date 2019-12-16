#include <Rcpp.h>
#include "component1.h"
using namespace Rcpp;


// [[Rcpp::export]]
void dfs3 (int x, NumericMatrix g, NumericVector vis1, int comp, int & cont){
    int n1=g.nrow(); //numero di nodi
    vis1(x)=1; //setto come visitato il nodo che ho passato come radice di dfs
    int j;
    for(j=0;j<n1;j++){
        if((vis1(j)==0)&(g(x,j)==comp)){ //se non ho ancora visitato il nodo j e c'è un arco tra il nodo x (radice dfs) e j allora vuol dire che c'è un collegamento e quindi eseguo dfs su quel nodo.
            cont++;
            dfs3(j,g,vis1,comp,cont);
        }
    }
}

// [[Rcpp::export]]
int find1(NumericMatrix subset, int i){
    if(subset(i,0)!=i){ //ricerco l'antenato del nodo i
        subset(i,0)=find1(subset,subset(i,0));
    }
    return subset(i,0);
}

// [[Rcpp::export]]
void Union(NumericMatrix subset, int x, int y){
    int xroot=find1(subset,x);//ricerco antenato di x
    int yroot=find1(subset,y);// ricerco antenato di y
    if(subset(xroot,1)<subset(yroot,1)){//se il rank del nodo antenato di x è minore del rank del nodo antenato di y
        subset(xroot,0)=yroot;//metto come parente del nodo antenato di x il nodo antenato di y
        
    }else{
        if(subset(xroot,1)>subset(yroot,1)){
            subset(yroot,0)=xroot;//altrimenti il viceversa
        }else{
            if(subset(xroot,1)==subset(yroot,1)){//se sono uguali ne posso prendere uno a caso
                subset(yroot,0)=xroot;
                subset(xroot,1)++;
            }
        }
    }
}


// [[Rcpp::export]]
List boruvka (NumericMatrix graph, NumericMatrix adj, int comp){
    int v=graph.nrow();
    NumericMatrix mst(v,v);//matrice albero MST
    int e=0; //contatore archi
    int i,j;
    int k=0;//indice per scorrere la matrice edge
    int start;//indice di partenza per dfs a contare il numero di nodi all'interno della componente
    List returned;
    NumericVector vis(v);
    for(i=0;i<v;i++){
        vis(i)=0; //inizializzo vettore visualizzati a 0 per dfs a contare numero nodi
    }
    for(i=0;i<v;i++){
        for(j=0;j<v;j++){
             if(adj(i,j)==comp){
                 start=i; //trovo il nodo di partenza per fare dfs e contare i nodi della componente
                 i=v;
                 j=v;
             }
         }
    }
    k=start;//cosi inizio a riempire vettore edge dall'indice giusto in caso di più componenti
    int nodi=1;//contatore nodi che verrà modificato con dfs 3
    for(i=start;i<v;i++){
        dfs3(i,adj,vis,comp,nodi); //conto il numero di nodi della componente
    }
    for(i=0;i<v;i++){
        for(j=0;j<v;j++){
            if(graph(i,j)>0){
                adj(j,i)=0; //dato che ho la matrice di adiacenza di un grafo non orientato me la trasformo in matrice di adiacenza di un grafo orientato
            }
        }
    }
    for(i=0;i<v;i++){
        for(j=0;j<v;j++){
            mst(i,j)=0; //inizializzo la matrice di adiacenza dell'albero finale
            if((graph(i,j)>0)&(adj(i,j)==comp)){
                e++;//conto gli archi contenuti nella componente
            }
        }
    }
    CharacterVector rn= rownames(graph);
    rownames(mst)=rn;
    colnames(mst)=rn;
    NumericMatrix edge (v,3); //matrice contenente gli archi
    for(i=0;i<v;i++){
        for(j=0;j<v;j++){
            if((graph(i,j)>0)&(adj(i,j)==comp)){
                edge(k,0)=i;//partenza arco
                edge(k,1)=j;//destinazione arco
                edge(k,2)=graph(i,j);//peso arco
                k++;
            }
        }
    }
    NumericMatrix subset (v,2);//matrice parenti e rank
    NumericVector cheapest(v);//matrice percorsi minimi
    for(i=start;i<v;i++){
        subset(i,0)=i;//parente nodo i
        subset(i,1)=0;//rank nodo i
        cheapest(i)=-1;//peso più basso dal nodo i
    }
    int contarchi=0; //contatore archi nell'albero
    int mstweight=0; //peso albero
    int set1,set2;
    while(contarchi<nodi-1){
        for(i=0;i<nodi;i++){
            cheapest(i)=-1;
        }
        for(i=0;i<v;i++){
            set1=find1(subset,edge(i,0));//trovo l'antenato del nodo da cui parte l'arco i
            set2=find1(subset,edge(i,1));//trovo l'antenato del nodo a cui arriva l'arco i
            
            if(set1!=set2){ //se non hanno antenato comune
                if((cheapest(set1)==-1)|(edge(cheapest(set1),2)>edge(i,2))){//controllo se non ho ancora preso in considerazione quell'arco o se l'arco che ho preso in considerazione ha peso minore dell'arco che avevo preso in considerazione precentemente
                    cheapest(set1)=i;//se è cosi a cheapest di indice i associo i
                }
                if((cheapest(set2)==-1)|(edge(cheapest(set2),2)>edge(i,2))){
                    cheapest(set2)=i;
                }
            }
        }
        for(i=0;i<v;i++){
            if(cheapest(i)!=-1){
                set1=find1(subset,edge(cheapest(i),0));//trovo l'antenato del nodo da cui parte l'arco i
                set2=find1(subset,edge(cheapest(i),1));//trovo l'antenato del nodo a cui arriva l'arco i
               
                if(set1!=set2){// se non hanno antenato comune
                    mstweight+=edge(cheapest(i),2); //incremento il contatore del peso totale dell'albero
                    mst(edge(cheapest(i),0),edge(cheapest(i),1))=1;//alla radice di adiacenza dell'albero aggiungo un arco
                    Union(subset,set1,set2);//aggiungo l'arco effettivo all'albero
                    contarchi ++;//incremento il contatore degli archi
            }
        }
    }
}
    returned=List::create(mst,mstweight);//ritorno la lista con il peso totale minimo dell'albero e la matrice di adiacenza di MST
return returned;
}



// [[Rcpp::export]]
List Boruvka(NumericMatrix matrixpeso, NumericMatrix adjacencymatrix){
    int dim=adjacencymatrix.nrow();
    NumericMatrix support(dim,dim); //matrice di supporto in cui copio i valori della matrice di adiacenza con gli 0 e gli 1 per mantenere inalterata la matrice di adiacenza
    CharacterVector rn, cn;
    rn=rownames(adjacencymatrix);
    cn=colnames(adjacencymatrix);
    int i,j;
    for(i=0;i<dim;i++){
        for(j=0;j<dim;j++){
            support(i,j)=adjacencymatrix(i,j); //inizializzo support con i valori della matrice di adiacenza
        }
    }
    //rownames(support)=rn;
    //colnames(support)=cn;
    int n=component1(matrixpeso, support);//conto il numero di componenti del grafo e mi segno nella matrice di adiacenza a quale componente sono associati gli archi
    List costi; //dichiaro lista costi contente costo minimo e matrice adiacenza albero di ogni componente
    if(n==0){ //se n è uguale a 0 non ho componenti e nemmeno archi
        NumericMatrix tree(dim,dim);
        rownames(tree)=rn;
        colnames(tree)=cn;
        for(i=0;i<dim;i++){
            for(j=0;j<dim;j++){
                tree(i,j)=0; // quindi riempio la matrice di adiacenza dell'albero di prim con tutti 0
            }
        }
        int c=0;
        List cost;
        List finale;
        cost=List::create(c,tree);
        finale=List::create(cost);
        return finale; //ritorno la matrice con tutti 0 e il costo che è 0
    }
    if(n==1){
        costi=List::create(boruvka(matrixpeso,support,1)); // se ho una sola componente chiamo subito prim.
    }else{ //se ho più componenti chiamo prim su ogni componente.
        
        int i;
        for(i=0;i<n;i++){
            costi.push_back(boruvka(matrixpeso,support,i+1)); //per ogni componente richiamo prim con la matrice di adiacenza dei pesi, la matrice di adiacenza modificata e il numero della componente
            
            
        }
        
    }
    return costi;
}
