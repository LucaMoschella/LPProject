(******************** FUNZIONI DI UTILITA ********************)
use "Sintassi.sml";

(* concatena buildContesto dei tipi *) 
fun concatenaContesto ( buildContesto l1 , buildContesto l2) = buildContesto ( l1 @ l2);

(* %%%%%%%%%%%%%%%%% cerca la definizione di una classeSintattica in un programmo %%%%%%%%%%%%%%%%%%%%% *)
fun cercaClasseInProgramma ( programmaSintattico, Object ) = defClasseS(Object, Object, [], [])	
	| cercaClasseInProgramma ( codiceS [], nomeCl nclasse ) = raise ClassNotFound
	| cercaClasseInProgramma ( codiceS (  (defClasseS (nomeCl c, ce , lv , lm)) ::l ), nomeCl nclasse ) = 
				if (nclasse = c) then defClasseS (nomeCl c, ce , lv , lm) else cercaClasseInProgramma( codiceS l, nomeCl nclasse)
	| cercaClasseInProgramma ( codiceS (  (defClasseS (Object, ce , lv , lm)) ::l ), nomeCl nclasse ) =  
				cercaClasseInProgramma( codiceS l, nomeCl nclasse);

fun esisteClasseInProgramma ( programmaSintattico, Object ) = true	
	| esisteClasseInProgramma ( codiceS [], nomeCl nclasse ) = false
	| esisteClasseInProgramma ( codiceS (  (defClasseS (nomeCl c, ce , lv , lm)) ::l ), nomeCl nclasse ) = 
				if (nclasse = c) then true else esisteClasseInProgramma( codiceS l, nomeCl nclasse)
	| esisteClasseInProgramma ( codiceS (  (defClasseS (Object, ce , lv , lm)) ::l ), nomeCl nclasse ) =  
				esisteClasseInProgramma( codiceS l, nomeCl nclasse);

(* %%%%%%%%%%%%%%%%% data la definizione di una classeSintattica, torna la classeSintattica che estende %%%%%%%%%%%%%%%%%%%%% *)
fun getExtendedClass( defClasseS (nomeclasse, nomeclasseestesa , campi , metodi) ) = nomeclasseestesa;

(* %%%%%%%%%%%%%%%%% tipoSintattico di un campoSintattico (programmaSintattico, nomecampo, cnomeclasse) %%%%%%%%%%%%%%%%%%%%% *)

fun tipoToType ( intS   ) = intT
| tipoToType ( classeS c ) = classeT c;

fun cercaTipoCampoinClasse ( programmaSintattico, defClasseS (Object, _ , _ , _), nomeC campoSintattico ) = raise FieldNotFound
	| cercaTipoCampoinClasse (programmaSintattico,  defClasseS (nomeCl _, classeEstesa , [] , _), nomeC campoSintattico ) = cercaTipoCampoinClasse( programmaSintattico, cercaClasseInProgramma(programmaSintattico,classeEstesa ), nomeC campoSintattico)
	| cercaTipoCampoinClasse ( programmaSintattico, defClasseS (nomeCl classeSintattica, ext , (defCampoS( t, nomeC nc, r))::campi , metodi), nomeC campoSintattico ) =
			if( nc = campoSintattico ) then tipoToType ( t ) else cercaTipoCampoinClasse(programmaSintattico, defClasseS (nomeCl classeSintattica, ext , campi , metodi), nomeC campoSintattico );

fun ftype( programmaSintattico, nomec, nomecl) = cercaTipoCampoinClasse(programmaSintattico, cercaClasseInProgramma ( programmaSintattico, nomecl ), nomec );


(* %%%%%%%%%%%%%%%%% tipoSintattico di un metodoSintattico (programmaSintattico, nomemetodo, nomeclasse, [t1,...,tn] ) %%%%%%%%%%%%%%%%%%%%% *)
(*
fun equalType ( intS, intS ) = true
	| equalType ( intS, classeS( c) ) = false
	| equalType ( classeS( c), intS ) = false
	| equalType ( classeS( Object), classeS( Object) ) = true
	| equalType ( classeS( Object), classeS( nomeCl c2) ) = false
	| equalType ( classeS( nomeCl c1), classeS( Object) ) = false
	| equalType ( classeS( nomeCl c1), classeS( nomeCl c2) ) = (c1 = c2);
*)

(* c1 è più in alto nella gerarchia di c2 *)
fun isSottoclasse( programmaSintattico, nomeCl c1, Object) = false
 | isSottoclasse( programmaSintattico, nomeCl c1, nomeCl c2) = if (c1 = c2) then true else  (isSottoclasse(programmaSintattico, nomeCl c1,  getExtendedClass(cercaClasseInProgramma(programmaSintattico,nomeCl c2))));

(* il secondo è compatibile con il primo*)
fun compatibleTipoTypes (programmaSintattico, intS, intT ) = true
	| compatibleTipoTypes (programmaSintattico, intS, classeT( c) ) = false
	| compatibleTipoTypes (programmaSintattico, intS, T ) = false
	| compatibleTipoTypes (programmaSintattico, c, T ) = true
	| compatibleTipoTypes (programmaSintattico, classeS( c), intT ) = false
	| compatibleTipoTypes (programmaSintattico, classeS( Object), classeT( Object) ) = true
	| compatibleTipoTypes (programmaSintattico, classeS( Object), classeT( nomeCl c2) ) = false
	| compatibleTipoTypes (programmaSintattico, classeS( nomeCl c1), classeT( Object) ) = false
	| compatibleTipoTypes (programmaSintattico, classeS( nomeCl c1), classeT( nomeCl c2) ) = isSottoclasse(programmaSintattico, nomeCl c1, nomeCl c2);

fun compatibleTypesTypes (programmaSintattico, classeT( nomeCl c1), classeT( nomeCl c2) ) = isSottoclasse(programmaSintattico, nomeCl c1, nomeCl c2)
	| compatibleTypesTypes (programmaSintattico, classeT c, T) = true
	| compatibleTypesTypes (programmaSintattico, t1, t2) = (t1 = t2);


fun parametriCompatibili(programmaSintattico, [], [] ) = true
	| parametriCompatibili(programmaSintattico, defVarS(t,n)::l, [] ) = false
	| parametriCompatibili(programmaSintattico, [], t::l ) = false
	| parametriCompatibili(programmaSintattico, (defVarS(t1,n))::l1, t2::l2 ) = if( not (compatibleTipoTypes(programmaSintattico,t1,t2))) then false else parametriCompatibili(programmaSintattico,l1,l2 );

fun cercaTipoMetodoInClasse(programmaSintattico, defClasseS (Object, _ , _ , _), nomeM metodoSintattico, parametri ) = raise MethodNotFound

	| cercaTipoMetodoInClasse(programmaSintattico,  defClasseS (nomeCl _, classeEstesa , _ , []), nomeM metodoSintattico, parametri ) = 
			cercaTipoMetodoInClasse( programmaSintattico, cercaClasseInProgramma(programmaSintattico,classeEstesa ), nomeM metodoSintattico, parametri)

	| cercaTipoMetodoInClasse ( programmaSintattico, defClasseS (nomeCl classeSintattica, ext , campi , (defMetodoS(t,nomeM m,args,locals,cmds))::metodi), nomeM metodoSintattico,parametri ) =
			if( (m = metodoSintattico) andalso (parametriCompatibili(programmaSintattico, args, parametri))) (* parametri deve contere tipi dal datatype types*)
				then tipoToType ( t ) 
				else cercaTipoMetodoInClasse(programmaSintattico, defClasseS (nomeCl classeSintattica, ext , campi , metodi), nomeM metodoSintattico, parametri );


fun mtype( programmaSintattico, nomem, nomecl, tipi) = cercaTipoMetodoInClasse(programmaSintattico, cercaClasseInProgramma ( programmaSintattico, nomecl ), nomem, tipi );


(* %%%%%%%%%%%%%%%%% regole per i tipi %%%%%%%%%%%%%%%%%%%%% *)


(*  REGOLA 1 - cerca var *)
fun equalVarpiuAndVar ( varNome( nomeV v ), nomeV s ) = ( v = s )
	| equalVarpiuAndVar (  this, _ ) = raise ThisIsNotAVariable;

fun cercaTipoVariabileInContesto ( buildContesto [],nomeV s ) = raise NonTypedVar
	| cercaTipoVariabileInContesto ( buildContesto ((n1,t1)::l), nomeV s ) = 
		if (equalVarpiuAndVar(n1, nomeV s)) then t1 else cercaTipoVariabileInContesto( buildContesto l, nomeV s);


fun cercaTipoThisInContesto ( buildContesto [] ) = raise NonTypedThis
	| cercaTipoThisInContesto ( buildContesto ((this,t1)::l) ) = t1
	|  cercaTipoThisInContesto ( buildContesto ((_,t1)::l) ) =cercaTipoThisInContesto( buildContesto l );

fun getNomeClasseDaTipo( intT ) =  raise TypeIsNotAClass
 	| getNomeClasseDaTipo( T ) = raise TypeIsNotAClass
	| getNomeClasseDaTipo( classeT n) = n;

fun getListTipiArgs (programmaSintattico, cont, [] ) = []
	| getListTipiArgs ( programmaSintattico, cont,r::l ) = (cercaTipoRightValueInContesto(programmaSintattico, cont, r)) :: (getListTipiArgs (  programmaSintattico, cont,l ))

and	cercaTipoRightValueInContesto( programmaSintattico, cont, varExprS(nomeV v)  ) = cercaTipoVariabileInContesto( cont, nomeV v)
	| cercaTipoRightValueInContesto( programmaSintattico, cont, intExprS n) = intT
	| cercaTipoRightValueInContesto( programmaSintattico, cont, newS( c)) = classeT c
	| cercaTipoRightValueInContesto( programmaSintattico, cont, ( nullS )) = T 
	| cercaTipoRightValueInContesto( programmaSintattico, cont, ( thisS )) = cercaTipoThisInContesto(cont)
	| cercaTipoRightValueInContesto( programmaSintattico, cont, ( superS )) = classeT (getExtendedClass( cercaClasseInProgramma(programmaSintattico, getNomeClasseDaTipo( cercaTipoThisInContesto cont)) ) )
	| cercaTipoRightValueInContesto( programmaSintattico, cont, accessoCampoS( right, c) ) = ftype( programmaSintattico, c, getNomeClasseDaTipo( cercaTipoRightValueInContesto(programmaSintattico,cont, right)))
	| cercaTipoRightValueInContesto( programmaSintattico, cont, chiamataMetodoS( right, m, args) ) = mtype(programmaSintattico, m, getNomeClasseDaTipo( cercaTipoRightValueInContesto( programmaSintattico, cont, right) ) , getListTipiArgs (programmaSintattico, cont, args )) ;


(* aggiungi al buildContesto una lista di variabili*)



fun getContestExpanded( buildContesto lc, [] ) = buildContesto lc 
	|getContestExpanded( buildContesto lc, defVarS( tipoSintattico, nomeV v)::l ) =getContestExpanded( buildContesto ((varNome (nomeV v),  tipoToType ( tipoSintattico ))::lc), l);



(*  il secondo è compatibile con il primo
fun compatibleTipoTypes (programmaSintattico, intS, intT ) = 
*)

fun controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, [] ) , ret) = ret

| controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, (assegnamentoVarS( nomeV v, right))::comandi ) , ret) = 
	if( not ( compatibleTypesTypes( programmaSintattico , 
		cercaTipoRightValueInContesto( programmaSintattico, getContestExpanded( getContestExpanded(cont, args), locals),  varExprS(nomeV v)  ) , 
		cercaTipoRightValueInContesto( programmaSintattico, getContestExpanded( getContestExpanded(cont, args), locals),  right  ))))
	then false
	else controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, comandi ) , ret)

| controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, (assegnamentoCampoS( right1, nomeC c, right2))::comandi ) , ret) = 
	if( not ( compatibleTypesTypes( programmaSintattico , 
		cercaTipoRightValueInContesto( programmaSintattico, getContestExpanded( getContestExpanded(cont, args), locals),  accessoCampoS( right1, nomeC c)  ) , 
		cercaTipoRightValueInContesto( programmaSintattico, getContestExpanded( getContestExpanded(cont, args), locals),  right2  ))))
	then false
	else controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, comandi ) , ret)

| controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, (returnS d)::comandi ), ret) = 
	if( not ( compatibleTipoTypes( programmaSintattico ,  tipoSintattico , cercaTipoRightValueInContesto( programmaSintattico, getContestExpanded( getContestExpanded(cont, args), locals),  d  )  )))
	then false 
	else controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, comandi ), true);

fun controllaTipoMetodo( programmaSintattico, cont, metodoSintattico ) = controllaTipoMetodoApp( programmaSintattico, cont, metodoSintattico , false);


(* classeSintattica = defClasseS of nomeClasse * nomeClasse * campoSintattico list * metodoSintattico list 
and campoSintattico = defCampoS of tipoSintattico * nomeCampo * espressioneSintattica
*)

(* Controlla tipoSintattico classeSintattica *)
fun controllaListaCampi( programmaSintattico, cont, [] ) = true
| controllaListaCampi( programmaSintattico, cont, ( defCampoS( t, n, r))::l ) = if ( not (compatibleTipoTypes(programmaSintattico, t, cercaTipoRightValueInContesto(programmaSintattico, cont, r)))) 
																		then false 
																		else controllaListaCampi( programmaSintattico, cont, l );

fun controllaListaMetodi( programmaSintattico, cont, []) = true
	| controllaListaMetodi( programmaSintattico, cont, m::l) = if ( not (controllaTipoMetodo( programmaSintattico, cont, m))) then false
														else controllaListaMetodi(programmaSintattico, cont, l);


fun  controllaTipoClasse( programmaSintattico, defClasseS(nomeClasseCorrente, nomeClasseEstesa, campi, metodi)) = controllaListaCampi( programmaSintattico, buildContesto[(this,classeT(nomeClasseCorrente))], campi )
																										andalso
																										controllaListaMetodi( programmaSintattico, buildContesto[(this,classeT(nomeClasseCorrente))], metodi )
																										andalso 
																										esisteClasseInProgramma( programmaSintattico, nomeClasseEstesa)
																										;

fun controllaTipoProgrammaApp(programmaSintattico, codiceS [] ) = true
	| controllaTipoProgrammaApp(programmaSintattico, codiceS (c::l) ) = if (not(controllaTipoClasse(programmaSintattico, c))) then false
										else controllaTipoProgrammaApp(programmaSintattico,codiceS l);

fun controllaTipoProgramma( programmaSintattico ) = controllaTipoProgrammaApp(programmaSintattico, programmaSintattico);

(*and metodoSintattico = defMetodoS of tipoSintattico * nomeMetodo *  variabileSintattica list * variabileSintattica list * comandoSintattico list*)

(* TEST
use "PrintToJava.sml";
use "ProgrammiEsempio.sml";

print (stampaProgramma esempio);
 ( controllaTipoProgramma( esempio));
 *)
(*
val chiama = chiamataMetodoS((newS (nomeCl "Classe2")) ,
							nomeM "metodo3",
							[varExprS ( nomeV "v")]
							);
print (stampaProgramma esempio);
print (stampaTypes( cercaTipoRightValueInContesto(esempio, buildContesto [(varNome(nomeV "v"), classeT(nomeCl "Classe2"))], chiama )) ^ "\n");

val metodoSintattico = defMetodoS ( intS, nomeM "metodo2", [defVarS (intS, nomeV "input")], [], [assegnamentoVarS(nomeV "input", intExprS 5), returnS (varExprS (nomeV "input"))]);

controllaTipoMetodo(esempio, buildContesto [] , metodoSintattico );

print (stampaMetodo metodoSintattico);

val x=buildContesto [(varNome (nomeV "i"), intT), (varNome(nomeV "e"), intT)];

val y=buildContesto [(varNome (nomeV "e"), T), (varNome(nomeV "g"), T)];


print (stampaContesto x);
print (stampaContesto y);
print (stampaContesto (concatenaContesto (x,y)));
print (stampaContesto ( concatenaContesto (x,y)));
print( "Cerca tipoSintattico variabileSintattica: " ^ (stampaTypes (cercaTipoVariabileInContesto (concatenaContesto (x,y), nomeV "g"))) ^ "\n");
*)

(* SEMANTICA DINAMICA - ESECUZIONE *)
(*
datatype locazione = id of int
and obj = classeObj of nomeClasse * (( nomeClasse * nomeCampo * locazione ) list)
and Valori =  intV of int | objV of obj | nullV | *
and env = envList of (varPiu * Valori) list
and heap = heapList of (locazione * Valori) list;
	
*)
(*
print (stampaTipo(ftype( esempio, nomeC "a", nomeCl "Classe1")) ^ "\n");
print (stampaProgramma esempio);
*)

