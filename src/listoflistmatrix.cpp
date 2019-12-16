
#include <Rcpp.h>
using namespace Rcpp;



//[[Rcpp::export]]
NumericMatrix weight (List input, NumericMatrix matr){
    int n=input.size();
    Rcpp::List appo; //creo la lista di appoggio per gestire meglio gli elementi di lista di lista
    int rows=matr.nrow();
    CharacterVector rn1=rownames(matr);
    CharacterVector name = rownames(matr);
    name.names()=clone(name);
    Rcpp::NumericMatrix m1(rows,rows);// dichiaro una matrice nxn
    int i,j;
    std::string rn,cn; //dichiaro due stringhe che mi serviranno per memorizzare i parametri from e to contenuti in lista

    for(i=0;i<rows;i++){
        for(j=0;j<rows;j++){
            m1(i,j)=matr(i,j); //setto la matrice finale con gli stessi valori di quella in input
        }
    }
    colnames(m1)=rn1;//setto nomi di righe e colonne della matrice finale con i parametri contenuti in name
    rownames(m1)=rn1;
    int x,y; //queste variabili mi servono per memorizzare gli indici di righe e colonne
    for(i=0;i<n;i++){
        appo=as<Rcpp::List>(input[i]);
        rn=as<std::string>(appo(1));
        cn=as<std::string>(appo(2));
        x=name.findName(rn); //ricerco l'indice con il nome dell'attributo che corrisponde a from all'interno di name
        y=name.findName(cn); //ricerco l'indice con il nome dell'attributo che corrisponde a to all'interno di name

        m1(x,y)=appo(0); //alla matrice con indice di riga X e indice di colonna Y associo l'elemento di appo (0) ovvero dist.
    }


    return m1;
     
     
}

