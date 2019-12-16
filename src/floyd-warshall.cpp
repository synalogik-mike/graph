#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericMatrix floydWarshall (NumericMatrix graph){
    
    int r=graph.nrow();
    NumericMatrix distance(r,r) ;
    int i, j, k;
    //inizializiamo la matrice finale con gli stessi valori del grafo iniziale per vedere la distanza più corta
    //CharacterVector rn=rownames(graph); //prendo i nomi delle righe di appo
    CharacterVector cn=colnames(graph); //prendo i nomi delle colonne di appo
    CharacterVector rn=rownames(graph);
    rownames(distance)=rn; //setto i nomi delle righe di distance con i nomi delle righe di graph
    colnames(distance)=cn; //setto i nomi delle colonne di distance con i nomi delle colonne di graph
    for(int i=0;i<r;i++){
        for(int j=0;j<r;j++){
            distance(i,j)=graph(i,j); //setto distance con gli stessi valori di graph
        }
    }
    
    for (k = 0; k < r; k++)
    {
        for (i = 0; i < r; i++)
        {
            for (j = 0; j < r; j++)
            {
                if ((distance(i,k) * distance(k,j) != 0) && (i != j))
                {   
                    if ((distance(i,k) + distance(k,j) < distance(i,j)))
                    {
                        distance(i,j) = distance(i,k) + distance(k,j);
                    }
                }
            }
        }
    }
    return distance;
}
