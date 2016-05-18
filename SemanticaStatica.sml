exception NonTypedVar
exception NonTypedThis
exception FieldNotFound
exception MethodNotFound
exception ClassNotFound
exception TypeIsNotAClass

exception NoReturnType


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

fun esisteClasseInProgramma ( programma, Object ) = true	
	| esisteClasseInProgramma ( codice [], nomeCl nclasse ) = false
	| esisteClasseInProgramma ( codice (  (defClass (nomeCl c, ce , lv , lm)) ::l ), nomeCl nclasse ) = 
				if (nclasse = c) then true else esisteClasseInProgramma( codice l, nomeCl nclasse)
	| esisteClasseInProgramma ( codice (  (defClass (Object, ce , lv , lm)) ::l ), nomeCl nclasse ) =  
				esisteClasseInProgramma( codice l, nomeCl nclasse);

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

(* c1 è più in alto nella gerarchia di c2 *)
fun isSottoclasse( programma, nomeCl c1, Object) = false
 | isSottoclasse( programma, nomeCl c1, nomeCl c2) = if (c1 = c2) then true else  (isSottoclasse(programma, nomeCl c1,  getExtendedClass(cercaClasseInProgramma(programma,nomeCl c2))));

(* il secondo è compatibile con il primo*)
fun compatibleTipoTypes (programma, intero, myInt ) = true
	| compatibleTipoTypes (programma, intero, tyC( c) ) = false
	| compatibleTipoTypes (programma, intero, T ) = false
	| compatibleTipoTypes (programma, c, T ) = true
	| compatibleTipoTypes (programma, class( c), myInt ) = false
	| compatibleTipoTypes (programma, class( Object), tyC( Object) ) = true
	| compatibleTipoTypes (programma, class( Object), tyC( nomeCl c2) ) = false
	| compatibleTipoTypes (programma, class( nomeCl c1), tyC( Object) ) = false
	| compatibleTipoTypes (programma, class( nomeCl c1), tyC( nomeCl c2) ) = isSottoclasse(programma, nomeCl c1, nomeCl c2);

fun compatibleTypesTypes (programma, tyC( nomeCl c1), tyC( nomeCl c2) ) = isSottoclasse(programma, nomeCl c1, nomeCl c2)
	| compatibleTypesTypes (programma, tyC c, T) = true
	| compatibleTypesTypes (programma, t1, t2) = (t1 = t2);


fun parametriCompatibili(programma, [], [] ) = true
	| parametriCompatibili(programma, defvariabile(t,n)::l, [] ) = false
	| parametriCompatibili(programma, [], t::l ) = false
	| parametriCompatibili(programma, (defvariabile(t1,n))::l1, t2::l2 ) = if( not (compatibleTipoTypes(programma,t1,t2))) then false else parametriCompatibili(programma,l1,l2 );

fun cercaTipoMetodoInClasse(programma, defClass (Object, _ , _ , _), nomeM metodo, parametri ) = raise MethodNotFound

	| cercaTipoMetodoInClasse(programma,  defClass (nomeCl _, classeEstesa , _ , []), nomeM metodo, parametri ) = 
			cercaTipoMetodoInClasse( programma, cercaClasseInProgramma(programma,classeEstesa ), nomeM metodo, parametri)

	| cercaTipoMetodoInClasse ( programma, defClass (nomeCl classe, ext , campi , (defMetodo(t,nomeM m,args,locals,cmds))::metodi), nomeM metodo,parametri ) =
			if( (m = metodo) andalso (parametriCompatibili(programma, args, parametri))) (* parametri deve contere tipi dal datatype types*)
				then tipoToType ( t ) 
				else cercaTipoMetodoInClasse(programma, defClass (nomeCl classe, ext , campi , metodi), nomeM metodo, parametri );


fun mtype( programma, nomem, nomecl, tipi) = cercaTipoMetodoInClasse(programma, cercaClasseInProgramma ( programma, nomecl ), nomem, tipi );


(* %%%%%%%%%%%%%%%%% regole per i tipi %%%%%%%%%%%%%%%%%%%%% *)


(*  REGOLA 1 - cerca var *)
fun equalVarpiuAndVar ( varNome( nomeV v ), nomeV s ) = ( v = s )
	| equalVarpiuAndVar (  varThis , nomeV s ) = ( "this" = s );

fun cercaTipoVariabileInContesto ( tipiList [],nomeV s ) = raise NonTypedVar
	| cercaTipoVariabileInContesto ( tipiList ((n1,t1)::l), nomeV s ) = 
		if (equalVarpiuAndVar(n1, nomeV s)) then t1 else cercaTipoVariabileInContesto( tipiList l, nomeV s);


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


(* aggiungi al contesto una lista di variabili*)



fun getContestExpanded( tipiList lc, [] ) = tipiList lc 
	|getContestExpanded( tipiList lc, defvariabile( tipo, nomeV v)::l ) =getContestExpanded( tipiList ((varNome (nomeV v),  tipoToType ( tipo ))::lc), l);



(*  il secondo è compatibile con il primo
fun compatibleTipoTypes (programma, intero, myInt ) = 
*)

fun controllaTipoMetodoApp( programma, contesto, defMetodo(tipoSintattico, nomemetodo, args, locals, [] ) , ret) = ret

| controllaTipoMetodoApp( programma, contesto, defMetodo(tipoSintattico, nomemetodo, args, locals, (assegnamentoVar( nomeV v, right))::comandi ) , ret) = 
	if( not ( compatibleTypesTypes( programma , 
		cercaTipoRightValueInContesto( programma, getContestExpanded( getContestExpanded(contesto, args), locals),  isvariabile(nomeV v)  ) , 
		cercaTipoRightValueInContesto( programma, getContestExpanded( getContestExpanded(contesto, args), locals),  right  ))))
	then false
	else controllaTipoMetodoApp( programma, contesto, defMetodo(tipoSintattico, nomemetodo, args, locals, comandi ) , ret)

| controllaTipoMetodoApp( programma, contesto, defMetodo(tipoSintattico, nomemetodo, args, locals, (assegnamentoCampo( right1, nomeC c, right2))::comandi ) , ret) = 
	if( not ( compatibleTypesTypes( programma , 
		cercaTipoRightValueInContesto( programma, getContestExpanded( getContestExpanded(contesto, args), locals),  accessocampo( right1, nomeC c)  ) , 
		cercaTipoRightValueInContesto( programma, getContestExpanded( getContestExpanded(contesto, args), locals),  right2  ))))
	then false
	else controllaTipoMetodoApp( programma, contesto, defMetodo(tipoSintattico, nomemetodo, args, locals, comandi ) , ret)

| controllaTipoMetodoApp( programma, contesto, defMetodo(tipoSintattico, nomemetodo, args, locals, (return d)::comandi ), ret) = 
	if( not ( compatibleTipoTypes( programma ,  tipoSintattico , cercaTipoRightValueInContesto( programma, getContestExpanded( getContestExpanded(contesto, args), locals),  d  )  )))
	then false 
	else controllaTipoMetodoApp( programma, contesto, defMetodo(tipoSintattico, nomemetodo, args, locals, comandi ), true);

fun controllaTipoMetodo( programma, contesto, metodo ) = controllaTipoMetodoApp( programma, contesto, metodo , false);


(* classe = defClass of nomeClasse * nomeClasse * campo list * metodo list 
and campo = defCampo of tipo * nomeCampo * rigthvalue
*)

(* Controlla tipo classe *)
fun controllaListaCampi( programma, contesto, [] ) = true
| controllaListaCampi( programma, contesto, ( defCampo( t, n, r))::l ) = if ( not (compatibleTipoTypes(programma, t, cercaTipoRightValueInContesto(programma, contesto, r)))) 
																		then false 
																		else controllaListaCampi( programma, contesto, l );

fun controllaListaMetodi( programma, contesto, []) = true
	| controllaListaMetodi( programma, contesto, m::l) = if ( not (controllaTipoMetodo( programma, contesto, m))) then false
														else controllaListaMetodi(programma, contesto, l);


fun  controllaTipoClasse( programma, defClass(nomeClasseCorrente, nomeClasseEstesa, campi, metodi)) = controllaListaCampi( programma, tipiList[(varThis,tyC(nomeClasseCorrente))], campi )
																										andalso
																										controllaListaMetodi( programma, tipiList[(varThis,tyC(nomeClasseCorrente))], metodi )
																										andalso 
																										esisteClasseInProgramma( programma, nomeClasseEstesa)
																										;

fun controllaTipoProgrammaApp(programma, codice [] ) = true
	| controllaTipoProgrammaApp(programma, codice (c::l) ) = if (not(controllaTipoClasse(programma, c))) then false
										else controllaTipoProgrammaApp(programma,codice l);

fun controllaTipoProgramma( programma ) = controllaTipoProgrammaApp(programma, programma);

(*and metodo = defMetodo of tipo * nomeMetodo *  variabile list * variabile list * comando list*)

(* TEST
use "PrintToJava.sml";
use "ProgrammiEsempio.sml";

print (stampaProgramma esempio);
 ( controllaTipoProgramma( esempio));
 *)
(*
val chiama = chiamatametodo((new (nomeCl "Classe2")) ,
							nomeM "metodo3",
							[isvariabile ( nomeV "v")]
							);
print (stampaProgramma esempio);
print (stampaTypes( cercaTipoRightValueInContesto(esempio, tipiList [(varNome(nomeV "v"), tyC(nomeCl "Classe2"))], chiama )) ^ "\n");

val metodo = defMetodo ( intero, nomeM "metodo2", [defvariabile (intero, nomeV "input")], [], [assegnamentoVar(nomeV "input", isint 5), return (isvariabile (nomeV "input"))]);

controllaTipoMetodo(esempio, tipiList [] , metodo );

print (stampaMetodo metodo);

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

