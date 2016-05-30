use "Sintassi.sml";
use "StruttureDati.sml";
use "Exception.sml";
use "PrintToJava.sml";
use "SemanticaStatica.sml";
use "SemanticaDinamica.sml";
use "ProgrammiEsempio.sml";

fun eval( programma ) =
	(
		print (stampaProgrammaS programma );
		let 
			val programmaTipato = programmaStoT( programma )
		in
			print (stampaProgrammaT programmaTipato );
			let
				val (res, h) = valutaProgramma programmaTipato
			in
				(print ("\nIl programma è stato correttamente eseguito ed ha restituito: " ^ (stampaVal( res ))^ "\n"); res)
			end
		end
				(* STATIC ERROR *)
		handle 	ClassExtNotValid x => ( print ("\nERROR: La classe <" ^ (stampaNomeClasse x) ^ "> non può estendere sé stessa.\n\n"); noV )

				| UnknownVarInMetodo( n, cla, m ) => ( print ("\nERROR: La variabile <" ^ (stampaNomeVar n) ^ "> utilizzata nel metodo <" ^ (stampaNomeMetodo m) ^ "> della classe <"^ (stampaNomeClasse cla) ^ "> non è stata definita.\n\n"); noV )			
				| UnknownVarInClasse( n, cla ) => ( print ("\nERROR: La variabile <" ^ (stampaNomeVar n) ^ "> utilizzata durante l'inizializzazione dei campi della classe <" ^ (stampaNomeClasse cla) ^ "> non è stata definita.\n\n"); noV )

				| VarNotInitializedInMetodo( n, cla, m ) => ( print ("\nERROR: La variabile <" ^ (stampaNomeVar n) ^ "> utilizzata nel metodo <" ^ (stampaNomeMetodo m) ^ "> della classe <" ^ (stampaNomeClasse cla) ^ "> non è stata inizializzata.\n\n"); noV )			
				| VarNotInitializedInClasse( n, cla ) => ( print ("\nERROR: La variabile <" ^ (stampaNomeVar n) ^ "> utilizzata durante l'inizializzazione dei campi della classe <" ^ (stampaNomeClasse cla) ^ "> non è stata inizializzata.\n\n"); noV )
				| CampoNotInitialized( cla, n ) => ( print ("\nERROR: Durante l'inizializzazione del campo <" ^ (stampaNomeCampo n) ^ "> della classe <" ^ (stampaNomeClasse cla) ^ "> vengono utilizzati uno o più campi non ancora inizializzati.\n\n"); noV )

				| FieldNotFound (x, cla) => ( print ("\nERROR: Il campo <" ^ (stampaNomeCampo x) ^ "> non è stato trovato nella gerarchia della classe <" ^ (stampaNomeClasse cla) ^ ">.\n\n"); noV )
				| MethodNotFound (x, cla) => ( print ("\nERROR: Il metodo <" ^ (stampaNomeMetodo x) ^ "> compatibile con gli argomenti passati non è stato trovato nella gerarchia della classe <" ^ (stampaNomeClasse cla) ^ ">.\n\n"); noV )			
				| ClassNotFound (x, x2) => ( print ("\nERROR: La classe <" ^ (stampaNomeClasse x) ^ ">  estende la classe non definita <" ^ (stampaNomeClasse x2) ^ ">.\n\n"); noV )			
				| ReturnNotFound (x, cla) => ( print ("\nERROR: Il metodo <" ^ (stampaNomeMetodo x) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> non contiene un comando di return.\n\n"); noV )

				| TypeIsNotAClassInMetodo(m, cla) => ( print ("\nERROR: L'espressione utilizzata nel metodo <" ^ (stampaNomeMetodo m) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  "> non risolve in un oggetto valido.\n\n"); noV ) 
				| TypeIsNotAClassInClasse( cla) => ( print ("\nERROR: L'espressione utilizzata durante l'inizializzazione dei campi della classe <" ^ (stampaNomeClasse cla) ^  "> non risolve in un oggetto valido.\n\n"); noV ) 
				
				| TypeIsNotAClassCampo ( ts, c, cla ) => ( print ("\nERROR: Il tipo <" ^ (stampaNomeTipoS ts) ^ "> del campo <" ^ (stampaNomeCampo c) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^  "> non rappresenta un tipo valido.\n\n"); noV )
				| TypeIsNotAClassMetodo ( ts, m, cla ) => ( print ("\nERROR: Il tipo di ritorno <" ^ (stampaNomeTipoS ts) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^  "> non rappresenta un tipo valido.\n\n"); noV )
				| TypeIsNotAClassArgs ( ts, v, m, cla ) => ( print ("\nERROR: Il tipo <" ^ (stampaNomeTipoS ts) ^ "> del parametro <" ^ (stampaNomeVar v) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^  "> non rappresenta un tipo valido.\n\n"); noV )
				| TypeIsNotAClassLocals ( ts, v, m, cla ) => ( print ("\nERROR: Il tipo <" ^ (stampaNomeTipoS ts) ^ "> della variabile <" ^ (stampaNomeVar v) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^  "> non rappresenta un tipo valido.\n\n"); noV )
				
				| TypeIsNotAClassNewInMetodo ( x, cla, m ) => ( print ("\nERROR: Nel metodo <" ^ (stampaNomeMetodo m) ^ "> della classe <" ^ (stampaNomeClasse cla) ^ "> viene effettuata un operazione di new(<" ^ (stampaNomeClasse x) ^  ">), ma <" ^ (stampaNomeClasse x) ^  "> non è un nome di classe definito.\n\n"); noV )
				| TypeIsNotAClassNewInClasse ( x, cla ) => ( print ("\nERROR: Durante l'inizializzazione dei campi della classe <" ^ (stampaNomeClasse cla) ^ "> viene effettuata un operazione di new(<" ^ (stampaNomeClasse x) ^  ">), ma <" ^ (stampaNomeClasse x) ^  "> non è un nome di classe definito.\n\n"); noV )

				| TypeErrorDefField ( ts, n, e , cla) => ( print ("\nERROR TYPE MISMATCH: Impossibile inizializzare il campo <" ^ (stampaNomeCampo n) ^ "> di tipo <" ^ (stampaNomeTipoS ts) ^ "> con un espressione di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e )) ^">, nella classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); noV )
				| TypeErrorReturn ( n, ts, e, cla) => ( print ("\nERROR TYPE MISMATCH: Impossibile tornare un espressione di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e )) ^ "> se il tipo di ritorno definito è <" ^ (stampaNomeTipoS( ts )) ^">, nel metodo <" ^ (stampaNomeMetodo n) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); noV )
				| TypeErrorAssignVar (n, e1, e2, cla, v) =>	( print ("\nERROR TYPE MISMATCH: Impossibile assegnare alla variabile <" ^ (stampaNomeVar v) ^ "> di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e1 ))^ "> un espressione di tipo <"^ (stampaNomeTipoT( estraiTipoSemantico e2 )) ^">, nel metodo <" ^ (stampaNomeMetodo n) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); noV )			
				| TypeErrorAssignField (n, e1, e2, e3, cla, c) => ( print ("\nERROR TYPE MISMATCH: Impossibile assegnare al campo <" ^ (stampaNomeCampo c) ^ "> di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e2 ))^ "> un espressione di tipo <"^ (stampaNomeTipoT( estraiTipoSemantico e3 )) ^">, nel metodo <" ^ (stampaNomeMetodo n) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); noV )
				| TypeErrorOverrideMismatch ( n, supert, superc, baset, basec ) => ( print ("\nERROR TYPE MISMATCH: Il metodo <" ^ (stampaNomeMetodo n) ^ "> nella classe <" ^ (stampaNomeClasse basec) ^ "> effettua un override del metodo definito nella classe <" ^ (stampaNomeClasse superc) ^  "> cambiando il tipo di ritorno da <" ^ (stampaNomeTipoS supert) ^ "> al tipo incompatibile <" ^ (stampaNomeTipoS baset) ^ ">.\n\n"); noV )

				| MultipleClasseDef ( cla ) => ( print ("\nERROR: La classe <" ^ (stampaNomeClasse cla) ^ "> è definita più volte.\n\n"); noV )
				| MultipleCampoDef ( n, cla ) => ( print ("\nERROR: Il campo <" ^ (stampaNomeCampo n) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> è definito più volte.\n\n"); noV )
				| MultipleMetodoDef ( n, cla ) => ( print ("\nERROR: Il metodo <" ^ (stampaNomeMetodo n) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> è definito più volte.\n\n"); noV )
				| MultipleArgsDef ( n, cla, m ) => ( print ("\nERROR: Il parametro <" ^ (stampaNomeVar n) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> è definito più volte.\n\n"); noV )
				| MultipleLocalsDef ( n, cla, m ) => ( print ("\nERROR: La variabile <" ^ (stampaNomeVar n) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> è definita più volte.\n\n"); noV )
				| MultipleLocalsArgsDef ( n, cla, m ) => ( print ("\nERROR: La variabile <" ^ (stampaNomeVar n) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> è già definita come parametro.\n\n"); noV )

				(* RUNTIME ERROR *)
				| MissingMain=> (print "\nNon è stato trovato il metodo main(), il programma non verrà eseguito!\n"; noV)
	);

eval( programmaStatDin1 );

(*
programmaStatDin1
programmaStatDin2
programmaWeird
programmaOverride0
programmaOverride1
programmaOverride2
programmaOverride3
programmaOverride4
programmaOverride5
programmaOverride6
programmaInizializzazione0
programmaInizializzazione1
programmaInizializzazione2
programmaVisibilita1
programmaVisibilita2
programmaCast1
programmaCast2
programmaCast3
programmaCampo1
programmaDouble
programmaOverload
programmaOverload1
programmaTEST
programmaTEST2
*)