(********** ECCEZIONI **********)
(********** tipi **********)
exception VarNameNotValid  of string;
exception UnknownVar of string;

exception FieldNotFound of string;
exception MethodNotFound of string;
exception ClassNotFound of string;
exception ReturnNotFound of string;

exception TypeIsNotAClass;
exception ExprIsNotAVar;

exception TypeErrorField of string;
exception TypeErrorReturn of string;
exception TypeErrorAssignVar of string;
exception TypeErrorAssignField of string;

(********** esecuzione **********)
exception RuntimeErrorVarNotFoundInEnv
exception RuntimeErrorLocNotFoundInHeap
exception RuntimeErrorValIsNotObj
exception RuntimeErrorValIsNotInt
exception RuntimeErrorInitCampoNonTrovato


(********** NOMI **********)
datatype nomeCampo = nomeC of string;
datatype nomeVariabile = nomeV of string;
datatype nomeMetodo = nomeM of string;
datatype nomeClasse = nomeCl of string | Object;


(********** SINTASSI ASTRATTA **********)
datatype tipoSintattico = intS | classeS of nomeClasse

and variabileSintattica = defVarS of tipoSintattico * nomeVariabile

and campoSintattico = defCampoS of tipoSintattico * nomeCampo * espressioneSintattica

and espressioneSintattica = varExprS of nomeVariabile |
			                intExprS of int    |
			                thisS |
			                superS | 
			                nullS |		          
			                newS of nomeClasse |
			                accessoCampoS of espressioneSintattica * nomeCampo |
			                chiamataMetodoS of espressioneSintattica * nomeMetodo  * espressioneSintattica list

and comandoSintattico = assegnamentoVarS of espressioneSintattica * espressioneSintattica |
          			  	assegnamentoCampoS of espressioneSintattica * nomeCampo * espressioneSintattica|
            			returnS of espressioneSintattica

and metodoSintattico = defMetodoS of tipoSintattico * nomeMetodo *  variabileSintattica list * variabileSintattica list * comandoSintattico list

and classeSintattica = defClasseS of nomeClasse * nomeClasse * campoSintattico list * metodoSintattico list

and programmaSintattico = codiceS of classeSintattica list; (* main andrà in semantica *)


(********** TIPI E SINTASSI ASTRATTA TIPATA **********)
datatype contestoDeiTipi = buildContesto of (varPiu * tipoSemantico) list
and varPiu = varNome of nomeVariabile | this
and tipoSemantico = classeT of nomeClasse | intT | T;


(* il tipo semantico, se ridondante, è una traduzione di quello sintattico *)
datatype variabileTipata = defVarT of tipoSintattico * nomeVariabile * tipoSemantico

and campoTipato = defCampoT of tipoSintattico * nomeCampo * espressioneTipata * tipoSemantico 

and espressioneTipata = 	varExprT of nomeVariabile * tipoSemantico |
			                intExprT of int * tipoSemantico  |
			                thisT of tipoSemantico |
			                superT of tipoSemantico | 
			                nullT of tipoSemantico |		          
			                newT of nomeClasse * tipoSemantico | 
			                accessoCampoT of espressioneTipata * nomeCampo * tipoSemantico  |
			                chiamataMetodoT of espressioneTipata * nomeMetodo  * espressioneTipata list * tipoSemantico

and comandoTipato =  	assegnamentoVarT of espressioneTipata * espressioneTipata |
      			  		assegnamentoCampoT of espressioneTipata * nomeCampo * espressioneTipata|
        				returnT of espressioneTipata
        				
and metodoTipato = defMetodoT of tipoSintattico * nomeMetodo *  variabileTipata list * variabileTipata list * comandoTipato list

and classeTipata = defClasseT of nomeClasse * nomeClasse * campoTipato list * metodoTipato list

and programmaTipato = codiceT of classeTipata list; 


(********** ESECUZIONE **********)
datatype locazione = buildLoc of int ;
val currentLocInt: int ref = ref 0; (* DA PROVARE UNIFICAZIONE CON LA FUNZIONE *)
fun nextLoc () = (currentLocInt := (!currentLocInt) + 1; buildLoc (!currentLocInt));

datatype obj = 	istanza of nomeClasse * ((nomeClasse * nomeCampo * locazione ) list);
datatype valore = intV of int | objV of obj | nullV | noV;

datatype env = buildEnv of ((varPiu * valore) list); (* VALUTARE L'ASSOCIAIONE CON LOC, E NON VAL *)
datatype heap = buildHeap of ((locazione * valore) list);




