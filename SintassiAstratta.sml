datatype nomeCampo = nomeC of string;
datatype nomeVar = nomeV of string;
datatype nomeMetodo = nomeM of string;
datatype nomeClasse = nomeCl of string | 
						Object;

datatype tipo = intero | class of nomeClasse

and variabile = defvariabile of tipo * nomeVar

and campo = defCampo of tipo * nomeCampo * rigthvalue

and metodo = defMetodo of tipo * nomeMetodo *  variabile list * variabile list * comando list

and classe = defClass of nomeClasse * nomeClasse * campo list * metodo list

and comando =   assegnamentoVar of nomeVar * rigthvalue |
                assegnamentoCampo of rigthvalue * nomeCampo * rigthvalue|
                return of rigthvalue

and rigthvalue = isvariabile of nomeVar |
                isint of int    |
                kw of keyword |
                
                new of nomeClasse |
                accessocampo of rigthvalue * nomeCampo |
                chiamatametodo of rigthvalue * nomeMetodo  * rigthvalue list

and keyword = this | super | null
	
and programma = codice of classe list; (* main andrà in semantica *)