use "SintassiAstratta.sml";

exception NonTypedVar
exception NonTypedThis
exception FieldNotFound
exception MethodNotFound
exception ClassNotFound

exception TypeIsNotAClass

(* SEMANTICA STATICA - GESTIONE DEI TIPI *)
datatype ContestoDeiTipi = tipiList of (Varpiu * Types) list
and Varpiu = varNome of nomeVar | varThis 
and Types = tyC of nomeClasse | myInt | T;



(* concatena contesto dei tipi *) 
fun concatenaContesto ( tipiList l1 , tipiList l2) = tipiList ( l1 @ l2);

(* %%%%%%%%%%%%%%%%% cerca la definizione di una classe in un programmo %%%%%%%%%%%%%%%%%%%%% *)
fun cercaClasseInProgramma ( programma, Object ) = defClass(Object, Object, [], [])	
	| cercaClasseInProgramma ( codice [], nomeCl nclasse ) = raise ClassNotFound
	| cercaClasseInProgramma ( codice (  (defClass (nomeCl c, ce , lv , lm)) ::l ), nomeCl nclasse ) = 
				if (nclasse = c) then defClass (nomeCl c, ce , lv , lm) else cercaClasseInProgramma( codice l, nomeCl nclasse)
	| cercaClasseInProgramma ( codice (  (defClass (Object, ce , lv , lm)) ::l ), nomeCl nclasse ) =  
				cercaClasseInProgramma( codice l, nomeCl nclasse);

(* %%%%%%%%%%%%%%%%% data la definizione di una classe, torna la classe che estende %%%%%%%%%%%%%%%%%%%%% *)
fun getExtendedClass( defClass (nomeclasse, nomeclasseestesa , campi , metodi) ) = nomeclasseestesa;

(* %%%%%%%%%%%%%%%%% tipo di un campo (programma, nomecampo, cnomeclasse) %%%%%%%%%%%%%%%%%%%%% *)

fun tipoToType ( intero   ) = myInt
| tipoToType ( class c ) = tyC c;

fun cercaTipoCampoinClasse ( programma, defClass (Object, _ , _ , _), nomeC campo ) = raise FieldNotFound
	| cercaTipoCampoinClasse (programma,  defClass (nomeCl _, classeEstesa , [] , _), nomeC campo ) = cercaTipoCampoinClasse( programma, cercaClasseInProgramma(programma,classeEstesa ), nomeC campo)
	| cercaTipoCampoinClasse ( programma, defClass (nomeCl classe, ext , (defCampo( t, nomeC nc, r))::campi , metodi), nomeC campo ) =
			if( nc = campo ) then tipoToType ( t ) else cercaTipoCampoinClasse(programma, defClass (nomeCl classe, ext , campi , metodi), nomeC campo );

fun ftype( programma, nomec, nomecl) = cercaTipoCampoinClasse(programma, cercaClasseInProgramma ( programma, nomecl ), nomec );


(* %%%%%%%%%%%%%%%%% tipo di un metodo (programma, nomemetodo, nomeclasse, [t1,...,tn] ) %%%%%%%%%%%%%%%%%%%%% *)
(*
fun equalType ( intero, intero ) = true
	| equalType ( intero, class( c) ) = false
	| equalType ( class( c), intero ) = false
	| equalType ( class( Object), class( Object) ) = true
	| equalType ( class( Object), class( nomeCl c2) ) = false
	| equalType ( class( nomeCl c1), class( Object) ) = false
	| equalType ( class( nomeCl c1), class( nomeCl c2) ) = (c1 = c2);
*)

fun compatibleTipoTypes ( intero, myInt ) = true
	| compatibleTipoTypes ( intero, tyC( c) ) = false
	| compatibleTipoTypes ( intero, T ) = false
	| compatibleTipoTypes ( c, T ) = true
	| compatibleTipoTypes ( class( c), myInt ) = false
	| compatibleTipoTypes ( class( Object), tyC( Object) ) = true
	| compatibleTipoTypes ( class( Object), tyC( nomeCl c2) ) = false
	| compatibleTipoTypes ( class( nomeCl c1), tyC( Object) ) = false
	| compatibleTipoTypes ( class( nomeCl c1), tyC( nomeCl c2) ) = (c1 = c2);


fun parametriCompatibili( [], [] ) = true
	| parametriCompatibili( defvariabile(t,n)::l, [] ) = false
	| parametriCompatibili( [], t::l ) = false
	| parametriCompatibili( (defvariabile(t1,n))::l1, t2::l2 ) = if( not (compatibleTipoTypes(t1,t2))) then false else parametriCompatibili(l1,l2 );

fun cercaTipoMetodoInClasse(programma, defClass (Object, _ , _ , _), nomeM metodo, parametri ) = raise MethodNotFound

	| cercaTipoMetodoInClasse(programma,  defClass (nomeCl _, classeEstesa , _ , []), nomeM metodo, parametri ) = 
			cercaTipoMetodoInClasse( programma, cercaClasseInProgramma(programma,classeEstesa ), nomeM metodo, parametri)

	| cercaTipoMetodoInClasse ( programma, defClass (nomeCl classe, ext , campi , (defMetodo(t,nomeM m,args,locals,cmds))::metodi), nomeM metodo,parametri ) =
			if( (m = metodo) andalso (parametriCompatibili(args, parametri))) (* parametri deve contere tipi dal datatype types*)
				then tipoToType ( t ) 
				else cercaTipoMetodoInClasse(programma, defClass (nomeCl classe, ext , campi , metodi), nomeM metodo, parametri );


fun mtype( programma, nomem, nomecl, tipi) = cercaTipoMetodoInClasse(programma, cercaClasseInProgramma ( programma, nomecl ), nomem, tipi );


(* %%%%%%%%%%%%%%%%% regole per i tipi %%%%%%%%%%%%%%%%%%%%% *)


(*  REGOLA 1 - cerca var *)
fun equalVarpiu( varThis, varThis ) = true
| equalVarpiu( varThis, varNome n2) = false
| equalVarpiu( varNome( nomeV n), varThis) = false
| equalVarpiu( varNome( nomeV n), varNome ( nomeV n2)) = (n = n2) 



fun equalVar ( varNome( nomeV v ), nomeV s ) = ( v = s )
	| equalVar (  varThis , nomeV s ) = ( "this" = s );

fun cercaTipoVariabileInContesto ( tipiList [],nomeV s ) = raise NonTypedVar
	| cercaTipoVariabileInContesto ( tipiList ((n1,t1)::l), nomeV s ) = 
		if (equalVar(n1, nomeV s)) then t1 else cercaTipoVariabileInContesto( tipiList l, nomeV s);


fun cercaTipoThisInContesto ( tipiList [] ) = raise NonTypedThis
	| cercaTipoThisInContesto ( tipiList ((varThis,t1)::l) ) = t1
	|  cercaTipoThisInContesto ( tipiList ((_,t1)::l) ) =cercaTipoThisInContesto( tipiList l );

fun getNomeClasseDaTipo( myInt ) =  raise TypeIsNotAClass
 	| getNomeClasseDaTipo( T ) = raise TypeIsNotAClass
	| getNomeClasseDaTipo( tyC n) = n;

fun getListTipiArgs (programma, contesto, [] ) = []
	| getListTipiArgs ( programma, contesto,r::l ) = (cercaTipoRightValueInContesto(programma, contesto, r)) :: (getListTipiArgs (  programma, contesto,l ))

and	cercaTipoRightValueInContesto( programma, contesto, isvariabile(nomeV v)  ) = cercaTipoVariabileInContesto( contesto, nomeV v)
	| cercaTipoRightValueInContesto( programma, contesto, isint n) = myInt
	| cercaTipoRightValueInContesto( programma, contesto, new( c)) = tyC c
	| cercaTipoRightValueInContesto( programma, contesto, kw( null )) = T 
	| cercaTipoRightValueInContesto( programma, contesto, kw( this )) = cercaTipoThisInContesto(contesto)
	| cercaTipoRightValueInContesto( programma, contesto, kw( super )) = tyC (getExtendedClass( cercaClasseInProgramma(programma, getNomeClasseDaTipo( cercaTipoThisInContesto contesto)) ) )
	| cercaTipoRightValueInContesto( programma, contesto, accessocampo( right, c) ) = ftype( programma, c, getNomeClasseDaTipo( cercaTipoRightValueInContesto(programma,contesto, right)))
	| cercaTipoRightValueInContesto( programma, contesto, chiamatametodo( right, m, args) ) = mtype(programma, m, getNomeClasseDaTipo( cercaTipoRightValueInContesto( programma, contesto, right) ) , getListTipiArgs (programma, contesto, args )) ;


(* TEST *)
use "PrintToJava.sml";
use "ProgrammiEsempio.sml";


val chiama = chiamatametodo((new (nomeCl "Classe1")) ,
							nomeM "metodo2",
							[isint 1]
							);
print (stampaProgramma esempio);
print (stampaTypes( cercaTipoRightValueInContesto(esempio, tipiList [], chiama )) ^ "\n");

(*)
val x=tipiList [(varNome (nomeV "i"), myInt), (varNome(nomeV "e"), myInt)];

val y=tipiList [(varNome (nomeV "e"), T), (varNome(nomeV "g"), T)];


print (stampaContesto x);
print (stampaContesto y);
print (stampaContesto (concatenaContesto (x,y)));
print (stampaContesto ( concatenaContesto (x,y)));
print( "Cerca tipo variabile: " ^ (stampaTypes (cercaTipoVariabileInContesto (concatenaContesto (x,y), nomeV "g"))) ^ "\n");
*)

(* SEMANTICA DINAMICA - ESECUZIONE *)
(*
datatype Loc = id of int
and Obj = classeObj of nomeClasse * (( nomeClasse * nomeCampo * Loc ) list)
and Valori =  valInt of int | valObj of Obj | valNull | *
and Env = envList of (Varpiu * Valori) list
and Heap = heapList of (Loc * Valori) list;
	
*)
(*
print (stampaTipo(ftype( esempio, nomeC "a", nomeCl "Classe1")) ^ "\n");
print (stampaProgramma esempio);
*)

