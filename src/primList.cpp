#include <Rcpp.h>
#include "component1.h"
using namespace Rcpp;

// [[Rcpp::export]]
List chiamaprim(NumericMatrix graph,NumericMatrix adj, int comp){
    int contarchi=0; // il numero di archi in MST deve essere sempre minore di dim graph -1 altrimenti vorrebbe dire che avrei un ciclo
    int dim=graph.nrow(); //prendo dimensione grafo
    NumericVector selezionato(dim); //vettore nodi selezionati
    NumericMatrix treeMatrix(dim,dim); //matrice adiacenza albero
    CharacterVector rn, cn;
    rn=rownames(adj);
    cn=colnames(adj);
    List results; //lista che conterrà valore costo MST e matrice adiacenza MST
    int i,j,x,y;
    int mincost=0; //costo totale albero
    int cont=0; //contatore archi nella componente
    for(i=0;i<dim;i++){
        selezionato(i)=0; //inizializzo vettore selezionati a 0 o FALSE
    }
    int min;
    NumericMatrix nonorientato(dim,dim);
    for(i=0;i<dim;i++){
        for(j=0;j<dim;j++){
            nonorientato(i,j)=0;
            treeMatrix(i,j)=0; //inizializzo treeMatrix (matrice adiacenza albero) a 0
        }
    }
    rownames(treeMatrix)=rn;
    colnames(treeMatrix)=cn;
    for(i=0;i<dim;i++){
        for(j=0;j<dim;j++){
            if(graph(i,j)>0){
                nonorientato(i,j)=graph(i,j); //creo matrice adiacenza grafo non orientato
                nonorientato(j,i)=graph(i,j);
            }
            if(adj(i,j)==comp){
                cont++; //conto archi nella componente
            }
        }
    }
    int start;
    for(i=0;i<dim;i++){
        for(j=0;j<dim;j++){
            if(adj(i,j)==comp){
                start=i; //trovo l'indice di riga nel quale è contenuto il primo elemento comp e lo metto in a in modo da settare il giusto nodo di partenza per prim, se faccio partire sempre prim da 0 mi andrà bene per la prima componente ma non per la seconda.
                i=dim; //termino il for 
                j=dim;
            }
        }
    }
    selezionato(start)=1; //seleziono il vertice di partenza e lo setto a 1 o TRUE
    cont=cont/2; //dato che il grafo è non orientato il numero di archi effettivi sarà la metà di quelli contati
    
    if(cont>2){
        cont=cont-1; //se contarchi è minore di 1 e lo decremento di uno ...
    }
    
    while(contarchi < cont){
        min=2147483647;
        x=0;
        y=0;
        for(i=start;i<dim;i++){
             if(selezionato(i)==1){//se il nodo i è gia stato selezionato allora inizio a guardare tutti i nodi a cui è collegato il nodo i
                 for(j=0;j<dim;j++){
               
        if((selezionato(j)==0)&(adj(i,j)==comp)&(nonorientato(i,j)>0)){ //se il nodo j è collegato al nodo i e il nodo j non è ancora stato selezionato e il nodo j appartiene alla solita componente del nodo i
                        if(min>nonorientato(i,j)){//se il minimo attuale è maggiore del valore sull'arco di collegamento tra nodo i e nodo j
                            min=nonorientato(i,j);//aggiorno il minimo
                            x=i;//salvo gli indici del minimo
                            y=j;
                        }
                    }
                }
            }
        }
        selezionato(y)=1;//setto il nodo a cui si arriva da i che non era ancora stato selezionato come selezionato
        contarchi++;//aumento il contatore degli archi
        mincost+=min;//incremento il costo totale del MST
        treeMatrix(x,y)=1; //creo la matrice di adiacenza dell'albero MST
        treeMatrix(y,x)=1;
    }
    
    
    results=List::create(mincost,treeMatrix); //metto in lista il costo minimo e la matrice
    return results; //ritorno la lista
}
    

// [[Rcpp::export]]
List prim(NumericMatrix matrixpeso, NumericMatrix adjacencymatrix){
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
    rownames(support)=rn;
    colnames(support)=cn;
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
        costi=List::create(chiamaprim(matrixpeso,support,1)); // se ho una sola componente chiamo subito prim.
    }else{ //se ho più componenti chiamo prim su ogni componente.
        
        int i;
        for(i=0;i<n;i++){
            costi.push_back(chiamaprim(matrixpeso,support,i+1)); //per ogni componente richiamo prim con la matrice di adiacenza dei pesi, la matrice di adiacenza modificata e il numero della componente
            
            
        }
        
    }
    return costi;
}
