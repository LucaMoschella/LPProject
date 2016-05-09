use "SintassiAstratta.sml";

exception NonTypedVar
exception FieldNotFound
exception MethodNotFound

exception ClassNotFound

(* SEMANTICA STATICA - GESTIONE DEI TIPI *)
datatype ContestoDeiTipi = tipiList of (Varpiu * Types) list
and Varpiu = varNome of nomeVar | varThis 
and Types = tyC of nomeClasse | myInt | T;


(* gestione stampa *)
fun stampaTypes (tyC s) = stampaNomeClasse( s )
	| stampaTypes ( myInt) = "int"
	| stampaTypes (T) = "T";
fun stampaVarPiu ( varNome n ) = stampaNomeVar n
	| stampaVarPiu (varThis) = "this";
fun stampaContesto ( tipiList []) = "\n"
	| stampaContesto ( tipiList( (v,t)::l)) = "( " ^ (stampaVarPiu v) ^ ":" ^ (stampaTypes t) ^ " ) ; " ^ (stampaContesto (tipiList l));


(* concatena contesto dei tipi *) 
fun concatenaContesto ( tipiList l1 , tipiList l2) = tipiList ( l1 @ l2);

(* cerca tipo di una variaible in un contesto dei tipi*)
fun equalVar ( varNome( nomeV v ), nomeV s ) = ( v = s )
	| equalVar (  varThis , nomeV s ) = ( "this" = s );

fun appoggioPerRicerca ( tipiList [] , nomeV s ) = raise NonTypedVar
	| appoggioPerRicerca ( tipiList ((n1,t1)::l), nomeV s ) = if (equalVar(n1, nomeV s)) then t1 else appoggioPerRicerca( tipiList l, nomeV s);
fun cercaTipoVariabile ( tipiList l, nomeV s ) = appoggioPerRicerca( tipiList(rev l), nomeV s);


(* %%%%%%%%%%%%%%%%% cerca la definizione di una classe in un programmo %%%%%%%%%%%%%%%%%%%%% *)
fun cercaClasseInProgramma ( programma, Object ) = defClass(Object, Object, [], [])	
	| cercaClasseInProgramma ( codice [], nomeCl nclasse ) = raise ClassNotFound
	| cercaClasseInProgramma ( codice (  (defClass (nomeCl c, ce , lv , lm)) ::l ), nomeCl nclasse ) = 
				if (nclasse = c) then defClass (nomeCl c, ce , lv , lm) else cercaClasseInProgramma( codice l, nomeCl nclasse)
	| cercaClasseInProgramma ( codice (  (defClass (Object, ce , lv , lm)) ::l ), nomeCl nclasse ) =  
				cercaClasseInProgramma( codice l, nomeCl nclasse);


(* %%%%%%%%%%%%%%%%% tipo di un campo (programma, nomecampo, cnomeclasse) %%%%%%%%%%%%%%%%%%%%% *)
fun cercaTipoCampoinClasse ( programma, defClass (Object, _ , _ , _), nomeC campo ) = raise FieldNotFound
	| cercaTipoCampoinClasse (programma,  defClass (nomeCl _, classeEstesa , [] , _), nomeC campo ) = cercaTipoCampoinClasse( programma, cercaClasseInProgramma(programma,classeEstesa ), nomeC campo)
	| cercaTipoCampoinClasse ( programma, defClass (nomeCl classe, ext , (defCampo( t, nomeC nc, r))::campi , metodi), nomeC campo ) =
			if( nc = campo ) then t else cercaTipoCampoinClasse(programma, defClass (nomeCl classe, ext , campi , metodi), nomeC campo );

fun ftype( programma, nomec, nomecl) = cercaTipoCampoinClasse(programma, cercaClasseInProgramma ( programma, nomecl ), nomec );


(* %%%%%%%%%%%%%%%%% tipo di un metodo (programma, nomemetodo, nomeclasse, [t1,...,tn] ) %%%%%%%%%%%%%%%%%%%%% *)
fun equalType ( intero, intero ) = true
	| equalType ( intero, class( c) ) = false
	| equalType ( class( c), intero ) = false
	| equalType ( class( Object), class( Object) ) = true
	| equalType ( class( Object), class( nomeCl c2) ) = false
	| equalType ( class( nomeCl c1), class( Object) ) = false
	| equalType ( class( nomeCl c1), class( nomeCl c2) ) = (c1 = c2);

fun parametriCompatibili( [], [] ) = true
	| parametriCompatibili( defvariabile(t,n)::l, [] ) = false
	| parametriCompatibili( [], t::l ) = false
	| parametriCompatibili( (defvariabile(t1,n))::l1, t2::l2 ) = if( not (equalType(t1,t2))) then false else parametriCompatibili(l1,l2 );

fun cercaTipoMetodoInClasse(programma, defClass (Object, _ , _ , _), nomeM metodo, parametri ) = raise MethodNotFound
	| cercaTipoMetodoInClasse(programma,  defClass (nomeCl _, classeEstesa , _ , []), nomeM metodo, parametri ) = cercaTipoMetodoInClasse( programma, cercaClasseInProgramma(programma,classeEstesa ), nomeM metodo, parametri)
	| cercaTipoMetodoInClasse ( programma, defClass (nomeCl classe, ext , campi , (defMetodo(t,nomeM m,args,locals,cmds))::metodi), nomeM metodo,parametri ) =
			if( (m = metodo) andalso (parametriCompatibili(args, parametri))) then t 
				else cercaTipoMetodoInClasse(programma, defClass (nomeCl classe, ext , campi , metodi), nomeM metodo, parametri );


fun mtype( programma, nomem, nomecl, tipi) = cercaTipoMetodoInClasse(programma, cercaClasseInProgramma ( programma, nomecl ), nomem, tipi );


(* TEST *)
val x=tipiList [(varNome (nomeV "i"), myInt), (varNome(nomeV "e"), myInt)];

val y=tipiList [(varNome (nomeV "e"), T), (varNome(nomeV "g"), T)];


print (stampaContesto x);
print (stampaContesto y);
print (stampaContesto (concatenaContesto (x,y)));
print (stampaContesto ( concatenaContesto (x,y)));
print( (stampaTypes (cercaTipoVariabile (concatenaContesto (x,y), nomeV "i"))) ^ "\n");


(* SEMANTICA DINAMICA - ESECUZIONE *)
(*
datatype Loc = id of int
and Obj = classeObj of nomeClasse * (( nomeClasse * nomeCampo * Loc ) list)
and Valori =  valInt of int | valObj of Obj | valNull | *
and Env = envList of (Varpiu * Valori) list
and Heap = heapList of (Loc * Valori) list;
	
*)

print (stampaTipo(ftype( esempio, nomeC "a", nomeCl "Classe1")) ^ "\n");
print (stampaProgramma esempio);
print (stampaTipo( mtype(esempio, nomeM "metodo3", nomeCl "Classe2", [intero,intero])) ^ "\n");
