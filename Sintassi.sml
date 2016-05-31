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

and comandoSintattico = assegnamentoVarS of nomeVariabile * espressioneSintattica |
          			  	assegnamentoCampoS of espressioneSintattica * nomeCampo * espressioneSintattica|
            			returnS of espressioneSintattica

and metodoSintattico = defMetodoS of tipoSintattico * nomeMetodo *  variabileSintattica list * variabileSintattica list * comandoSintattico list

and classeSintattica = defClasseS of nomeClasse * nomeClasse * campoSintattico list * metodoSintattico list

and programmaSintattico = codiceS of classeSintattica list; (* main andrà in semantica *)


(********** SINTASSI ASTRATTA TIPATA **********) (* il tipo semantico, se ridondante, è una traduzione di quello sintattico *)
datatype tipoSemantico = classeT of nomeClasse | intT | T

and variabileTipata = defVarT of tipoSintattico * nomeVariabile * tipoSemantico

and campoTipato = defCampoT of tipoSintattico * nomeCampo * espressioneTipata * tipoSemantico 

and espressioneTipata = 	varExprT of nomeVariabile * tipoSemantico |
			                intExprT of int * tipoSemantico  |
			                thisT of tipoSemantico |
			                superT of tipoSemantico | 
			                nullT of tipoSemantico |		          
			                newT of nomeClasse * tipoSemantico | 
			                accessoCampoT of espressioneTipata * nomeCampo * tipoSemantico  |
			                chiamataMetodoT of espressioneTipata * nomeMetodo  * espressioneTipata list * tipoSemantico

and comandoTipato =  	assegnamentoVarT of nomeVariabile * espressioneTipata |
      			  		assegnamentoCampoT of espressioneTipata * nomeCampo * espressioneTipata|
        				returnT of espressioneTipata
        				
and metodoTipato = defMetodoT of tipoSintattico * nomeMetodo *  variabileTipata list * variabileTipata list * comandoTipato list

and classeTipata = defClasseT of nomeClasse * nomeClasse * campoTipato list * metodoTipato list

and programmaTipato = codiceT of classeTipata list; 