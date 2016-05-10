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
                accessocampo of rigthvalue * campo |
                chiamatametodo of rigthvalue * nomeMetodo  * rigthvalue list

and keyword = this | super | null
	
and programma = codice of classe list; (* main andrà in semantica *)
    
val esempio = codice( [
defClass(
        nomeCl "Classe1",
        Object,
        [
            defCampo ( intero, nomeC ("a"), isint 5 ),
            defCampo ( class Object, nomeC ("b"), kw null ) 
        ],
        [
            defMetodo ( intero, nomeM "metodo1", [defvariabile (intero, nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return (isvariabile (nomeV "input"))]) ,
            defMetodo ( class (nomeCl "Classe1"), nomeM "metodo2", [defvariabile (intero, nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return (isvariabile (nomeV "input"))]) 
        ]
        ),
defClass(
        nomeCl "Classe2",
        nomeCl "Classe1",
        [
          
            defCampo ( class (nomeCl "Classe1"), nomeC ("a"), new ( nomeCl "Classe1") ) ,
            defCampo ( intero, nomeC ("c"), isint 55 ),
            defCampo ( class (nomeCl "Classe1"), nomeC ("obj1"), new ( nomeCl "Classe1") ) 
        ],
        [
         defMetodo ( class (nomeCl "Classe1"), nomeM "metodo3", [defvariabile (intero, nomeV "input"),defvariabile (intero, nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return (isvariabile (nomeV "input"))]) ,
            defMetodo ( class (nomeCl "Classe2"), nomeM "metodo4", [defvariabile (intero, nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return (isvariabile (nomeV "input"))]) 

        ]
        ) 
]
);

fun stampaLista( [],  ind:string, sepchar:string, metodo) = "" |
	stampaLista( l::[], ind:string, sepchar:string, metodo) = ind ^ (metodo l) |
	stampaLista( l::lista, ind:string, sepchar:string, metodo) = ind ^ (metodo l) ^ sepchar ^ (stampaLista (lista, ind, sepchar, metodo));


fun stampaNomeCampo (nomeC s) = s;
fun stampaNomeVar (nomeV s) = s;
fun stampaNomeMetodo (nomeM s) = s;
fun stampaNomeClasse (nomeCl s) = s |
	stampaNomeClasse Object = "Object";


fun stampaTipo intero = "int"
	| stampaTipo (class a) = stampaNomeClasse a
and
	stampaVariabile ( defvariabile (t,n)) = (stampaTipo t ) ^ " " ^ (stampaNomeVar n)
and
	stampaCampo ( defCampo (t, n, v )) = (stampaTipo t ) ^ " " ^ (stampaNomeCampo n) ^ " = " ^ (stampaRightValue v)
and
	stampaMetodo ( defMetodo (t,n, args, locals, commands )) = 	(stampaTipo t ) ^ " " ^ (stampaNomeMetodo n) ^ "( " ^ (stampaLista(args,"", ", ", stampaVariabile )) ^ " ) " ^
		"\n{\n" ^ (stampaLista(locals,"    ", ";\n", stampaVariabile )) ^ "\n" ^ (stampaLista(commands,"    ", ";\n", stampaComando )) ^ "\n}"
and
	stampaClasse (defClass (n, nfrom, campi, metodi ) ) = 
		(stampaNomeClasse n) ^ " extends " ^ (stampaNomeClasse nfrom) ^
		"\n{\n" ^ (stampaLista(campi,"    ", ";\n", stampaCampo )) ^ "\n" ^ (stampaLista(metodi,"    ", "\n", stampaMetodo )) ^ "\n}\n"
and
	stampaComando  (assegnamentoVar (n,v)) = (stampaNomeVar n) ^ " = " ^ (stampaRightValue v)
	| stampaComando ( assegnamentoCampo ( left , n, right )) = ( stampaRightValue left) ^ "." ^ (stampaNomeCampo n ) ^ " = " ^ (stampaRightValue right)
	| stampaComando ( return r) = "return " ^ (stampaRightValue r)	
and 
	stampaRightValue ( isvariabile n) = stampaNomeVar n 
	| stampaRightValue ( isint i) = Int.toString i
	| stampaRightValue (kw this) = "this"
	| stampaRightValue (kw super)  = "super"
	| stampaRightValue (kw null) = "null"
	| stampaRightValue (new c) = "new " ^ (stampaNomeClasse c) ^ "()"
	| stampaRightValue (accessocampo (v,c)) = (stampaRightValue v) ^ "." ^ (stampaCampo c)
	| stampaRightValue (chiamatametodo (v,n, args)) = (stampaRightValue v) ^ "." ^ (stampaNomeMetodo n ) ^ "( " ^ (stampaLista(args, "",  ", ", stampaRightValue )) ^" )"
and
	stampaProgramma ( codice l ) = stampaLista( l, "", "\n", stampaClasse); 

