(******************** FUNZIONI DI UTILITA ********************)
use "Sintassi.sml";
exception ex
(* concatena buildContesto dei tipi *) 
(* OK *)		fun concatenaContesto ( buildContesto l1 , buildContesto l2) = buildContesto ( l1 @ l2);

(* %%%%%%%%%%%%%%%%% cerca la definizione di una classeSintattica in un programmo %%%%%%%%%%%%%%%%%%%%% *)
(*  ok  *)		fun cercaClasseInProgramma ( programmaSintattico, Object ) = defClasseS(Object, Object, [], [])	
			| cercaClasseInProgramma ( codiceS [], nomeCl nclasse ) = raise ex
			| cercaClasseInProgramma ( codiceS (  (defClasseS (nomeCl c, ce , lv , lm)) ::l ), nomeCl nclasse ) = 
						if (nclasse = c) then defClasseS (nomeCl c, ce , lv , lm) else cercaClasseInProgramma( codiceS l, nomeCl nclasse)
			| cercaClasseInProgramma ( codiceS (  (defClasseS (Object, ce , lv , lm)) ::l ), nomeCl nclasse ) =  
						cercaClasseInProgramma( codiceS l, nomeCl nclasse);

(*  ok  *)	fun esisteClasseInProgramma ( programmaSintattico, Object ) = true	
				| esisteClasseInProgramma ( codiceS [], nomeCl nclasse ) = false
				| esisteClasseInProgramma ( codiceS (  (defClasseS (nomeCl c, ce , lv , lm)) ::l ), nomeCl nclasse ) = 
							if (nclasse = c) then true else esisteClasseInProgramma( codiceS l, nomeCl nclasse)
				| esisteClasseInProgramma ( codiceS (  (defClasseS (Object, ce , lv , lm)) ::l ), nomeCl nclasse ) =  
							esisteClasseInProgramma( codiceS l, nomeCl nclasse);

(* %%%%%%%%%%%%%%%%% data la definizione di una classeSintattica, torna la classeSintattica che estende %%%%%%%%%%%%%%%%%%%%% *)
(*  ok  *) fun getExtendedClass( defClasseS (nomeclasse, nomeclasseestesa , campi , metodi) ) = nomeclasseestesa;

(* %%%%%%%%%%%%%%%%% tipoSintattico di un campoSintattico (programmaSintattico, nomecampo, cnomeclasse) %%%%%%%%%%%%%%%%%%%%% *)

(*  ok  *) fun tipoSintatticoToSemantico ( intS   ) = intT
				| tipoSintatticoToSemantico ( classeS c ) = classeT c;

(*  ok  *) fun cercaTipoCampoinClasse ( programmaSintattico, defClasseS (Object, _ , _ , _), nomeC campoSintattico ) = raise ex
				| cercaTipoCampoinClasse (programmaSintattico,  defClasseS (nomeCl _, classeEstesa , [] , _), nomeC campoSintattico ) = cercaTipoCampoinClasse( programmaSintattico, cercaClasseInProgramma(programmaSintattico,classeEstesa ), nomeC campoSintattico)
				| cercaTipoCampoinClasse ( programmaSintattico, defClasseS (nomeCl classeSintattica, ext , (defCampoS( t, nomeC nc, r))::campi , metodi), nomeC campoSintattico ) =
						if( nc = campoSintattico ) then tipoSintatticoToSemantico ( t ) else cercaTipoCampoinClasse(programmaSintattico, defClasseS (nomeCl classeSintattica, ext , campi , metodi), nomeC campoSintattico );

(*  ok  *) fun ftype( programmaSintattico, nomec, nomecl) = cercaTipoCampoinClasse(programmaSintattico, cercaClasseInProgramma ( programmaSintattico, nomecl ), nomec );


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
(*  ok  *) fun isSottoClasse( programmaSintattico, nomeCl c1, Object) = false
 				| isSottoClasse( programmaSintattico, nomeCl c1, nomeCl c2) = if (c1 = c2) then true else  (isSottoClasse(programmaSintattico, nomeCl c1,  getExtendedClass(cercaClasseInProgramma(programmaSintattico,nomeCl c2))));

(* il secondo è compatibile con il primo*)
(*  ok  *) fun compatibleTipoSintSem (programmaSintattico, intS, intT ) = true
				| compatibleTipoSintSem (programmaSintattico, intS, classeT( c) ) = false
				| compatibleTipoSintSem (programmaSintattico, intS, T ) = false
				| compatibleTipoSintSem (programmaSintattico, c, T ) = true
				| compatibleTipoSintSem (programmaSintattico, classeS( c), intT ) = false
				| compatibleTipoSintSem (programmaSintattico, classeS( Object), classeT( Object) ) = true
				| compatibleTipoSintSem (programmaSintattico, classeS( Object), classeT( nomeCl c2) ) = false
				| compatibleTipoSintSem (programmaSintattico, classeS( nomeCl c1), classeT( Object) ) = false
				| compatibleTipoSintSem (programmaSintattico, classeS( nomeCl c1), classeT( nomeCl c2) ) = isSottoClasse(programmaSintattico, nomeCl c1, nomeCl c2);

(*  ok  *) fun compatibleTipoSemSem (programmaSintattico, classeT( nomeCl c1), classeT( nomeCl c2) ) = isSottoClasse(programmaSintattico, nomeCl c1, nomeCl c2)
				| compatibleTipoSemSem (programmaSintattico, classeT c, T) = true
				| compatibleTipoSemSem (programmaSintattico, t1, t2) = (t1 = t2);


(*  ok  *) fun parametriCompatibili(programmaSintattico, [], [] ) = true
				| parametriCompatibili(programmaSintattico, defVarS(t,n)::l, [] ) = false
				| parametriCompatibili(programmaSintattico, [], t::l ) = false
				| parametriCompatibili(programmaSintattico, (defVarS(t1,n))::l1, t2::l2 ) = if( not (compatibleTipoSintSem(programmaSintattico,t1,t2))) then false else parametriCompatibili(programmaSintattico,l1,l2 );

(*  ok  *) fun cercaTipoMetodoInClasse(programmaSintattico, defClasseS (Object, _ , _ , _), nomeM metodoSintattico, parametri ) = raise ex

				| cercaTipoMetodoInClasse(programmaSintattico,  defClasseS (nomeCl _, classeEstesa , _ , []), nomeM metodoSintattico, parametri ) = 
						cercaTipoMetodoInClasse( programmaSintattico, cercaClasseInProgramma(programmaSintattico,classeEstesa ), nomeM metodoSintattico, parametri)

				| cercaTipoMetodoInClasse ( programmaSintattico, defClasseS (nomeCl classeSintattica, ext , campi , (defMetodoS(t,nomeM m,args,locals,cmds))::metodi), nomeM metodoSintattico,parametri ) =
						if( (m = metodoSintattico) andalso (parametriCompatibili(programmaSintattico, args, parametri))) (* parametri deve contere tipi dal datatype types*)
							then tipoSintatticoToSemantico ( t ) 
							else cercaTipoMetodoInClasse(programmaSintattico, defClasseS (nomeCl classeSintattica, ext , campi , metodi), nomeM metodoSintattico, parametri );


(*  ok  *) fun mtype( programmaSintattico, nomem, nomecl, tipi) = cercaTipoMetodoInClasse(programmaSintattico, cercaClasseInProgramma ( programmaSintattico, nomecl ), nomem, tipi );


(* %%%%%%%%%%%%%%%%% regole per i tipi %%%%%%%%%%%%%%%%%%%%% *)


(*  REGOLA 1 - cerca var *) 
(* OK *)	fun equalVarPiuVar ( varNome( nomeV v ), nomeV s ) = ( v = s )
			| equalVarPiuVar (  this, _ ) = raise ex;

(* OK *)	fun cercaVarPiuInContesto ( buildContesto [],v ) = raise ex
				| cercaVarPiuInContesto ( buildContesto ((n1,t1)::l), v ) = 
					if ((n1 = v)) then t1 else cercaVarPiuInContesto( buildContesto l, v);

(* OK 	fun cercaVarInContesto ( buildContesto [],nomeV s ) = raise NonTypedVar
				| cercaVarInContesto ( buildContesto ((n1,t1)::l), nomeV s ) = 
					if (equalVarPiuVar(n1, nomeV s)) then t1 else cercaVarInContesto( buildContesto l, nomeV s);
*)
(* OK 	fun cercaThisInContesto ( buildContesto [] ) = raise NonTypedThis
				| cercaThisInContesto ( buildContesto ((this,t1)::l) ) = t1
				|  cercaThisInContesto ( buildContesto ((_,t1)::l) ) =cercaThisInContesto( buildContesto l );
*)
(* OK *)fun getNomeClasseDaTipoT( intT ) =  raise ex
			 	| getNomeClasseDaTipoT( T ) = raise ex
				| getNomeClasseDaTipoT( classeT n) = n;

(* OK *)fun getListaTipiExpr (programmaSintattico, cont, [] ) = []
				| getListaTipiExpr ( programmaSintattico, cont,r::l ) = (cercaTipoRightValueInContesto(programmaSintattico, cont, r)) :: (getListaTipiExpr (  programmaSintattico, cont,l ))

(* OK *)and	cercaTipoRightValueInContesto( programmaSintattico, cont, varExprS(nomeV v)  ) = cercaVarPiuInContesto( cont, varNome (nomeV v))
				| cercaTipoRightValueInContesto( programmaSintattico, cont, intExprS n) = intT
				| cercaTipoRightValueInContesto( programmaSintattico, cont, newS( c)) = classeT c
				| cercaTipoRightValueInContesto( programmaSintattico, cont, ( nullS )) = T 
				| cercaTipoRightValueInContesto( programmaSintattico, cont, ( thisS )) = cercaVarPiuInContesto(cont, this)
				| cercaTipoRightValueInContesto( programmaSintattico, cont, ( superS )) = classeT (getExtendedClass( cercaClasseInProgramma(programmaSintattico, getNomeClasseDaTipoT( cercaVarPiuInContesto(cont, this))) ) )
				| cercaTipoRightValueInContesto( programmaSintattico, cont, accessoCampoS( right, c) ) = ftype( programmaSintattico, c, getNomeClasseDaTipoT( cercaTipoRightValueInContesto(programmaSintattico,cont, right)))
				| cercaTipoRightValueInContesto( programmaSintattico, cont, chiamataMetodoS( right, m, args) ) = mtype(programmaSintattico, m, getNomeClasseDaTipoT( cercaTipoRightValueInContesto( programmaSintattico, cont, right) ) , getListaTipiExpr (programmaSintattico, cont, args )) ;


(* aggiungi al buildContesto una lista di variabili*)
(* OK *)	fun getContestExpanded( buildContesto lc, [] ) = buildContesto lc 
	 			|getContestExpanded( buildContesto lc, defVarS( tipoSintattico, nomeV v)::l ) =getContestExpanded( buildContesto ((varNome (nomeV v),  tipoSintatticoToSemantico ( tipoSintattico ))::lc), l);



(*  il secondo è compatibile con il primo
fun compatibleTipoSintSem (programmaSintattico, intS, intT ) = 
*)

(* OK *) fun controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, [] ) , ret) = ret

		| controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, (assegnamentoVarS( nomeV v, right))::comandi ) , ret) = 
			if( not ( compatibleTipoSemSem( programmaSintattico , 
				cercaTipoRightValueInContesto( programmaSintattico, getContestExpanded( getContestExpanded(cont, args), locals),  varExprS(nomeV v)  ) , 
				cercaTipoRightValueInContesto( programmaSintattico, getContestExpanded( getContestExpanded(cont, args), locals),  right  ))))
			then false
			else controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, comandi ) , ret)

		| controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, (assegnamentoCampoS( right1, nomeC c, right2))::comandi ) , ret) = 
			if( not ( compatibleTipoSemSem( programmaSintattico , 
				cercaTipoRightValueInContesto( programmaSintattico, getContestExpanded( getContestExpanded(cont, args), locals),  accessoCampoS( right1, nomeC c)  ) , 
				cercaTipoRightValueInContesto( programmaSintattico, getContestExpanded( getContestExpanded(cont, args), locals),  right2  ))))
			then false
			else controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, comandi ) , ret)

		| controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, (returnS d)::comandi ), ret) = 
			if( not ( compatibleTipoSintSem( programmaSintattico ,  tipoSintattico , cercaTipoRightValueInContesto( programmaSintattico, getContestExpanded( getContestExpanded(cont, args), locals),  d  )  )))
			then false 
			else controllaTipoMetodoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomemetodo, args, locals, comandi ), true);

(* OK *) fun controllaTipoMetodo( programmaSintattico, cont, metodoSintattico ) = controllaTipoMetodoApp( programmaSintattico, cont, metodoSintattico , false);


(* classeSintattica = defClasseS of nomeClasse * nomeClasse * campoSintattico list * metodoSintattico list 
and campoSintattico = defCampoS of tipoSintattico * nomeCampo * espressioneSintattica
*)

(* Controlla tipoSintattico classeSintattica *)
(* OK *)  fun controllaListaCampi( programmaSintattico, cont, [] ) = true
			| controllaListaCampi( programmaSintattico, cont, ( defCampoS( t, n, r))::l ) = if ( not (compatibleTipoSintSem(programmaSintattico, t, cercaTipoRightValueInContesto(programmaSintattico, cont, r)))) 
																		then false 
																		else controllaListaCampi( programmaSintattico, cont, l );

(* OK *) fun controllaListaMetodi( programmaSintattico, cont, []) = true
			| controllaListaMetodi( programmaSintattico, cont, m::l) = if ( not (controllaTipoMetodo( programmaSintattico, cont, m))) then false
														else controllaListaMetodi(programmaSintattico, cont, l);


(* OK *)  fun  controllaTipoClasse( programmaSintattico, defClasseS(nomeClasseCorrente, nomeClasseEstesa, campi, metodi)) = controllaListaCampi( programmaSintattico, buildContesto[(this,classeT(nomeClasseCorrente))], campi )
																										andalso
																										controllaListaMetodi( programmaSintattico, buildContesto[(this,classeT(nomeClasseCorrente))], metodi )
																										andalso 
																										esisteClasseInProgramma( programmaSintattico, nomeClasseEstesa)
																										;

(* OK *) fun controllaTipoProgrammaApp(programmaSintattico, codiceS [] ) = true
	| controllaTipoProgrammaApp(programmaSintattico, codiceS (c::l) ) = if (not(controllaTipoClasse(programmaSintattico, c))) then false
										else controllaTipoProgrammaApp(programmaSintattico,codiceS l);

(* OK *) fun controllaTipoProgramma( programmaSintattico ) = controllaTipoProgrammaApp(programmaSintattico, programmaSintattico);

(*and metodoSintattico = defMetodoS of tipoSintattico * nomeMetodo *  variabileSintattica list * variabileSintattica list * comandoSintattico list*)

use "PrintToJava.sml";
use "ProgrammiEsempio.sml";

print (stampaProgrammaS esempioDispensa);

val x=buildContesto [(varNome (nomeV "i"), intT), (varNome(nomeV "e"), intT)];

val y=buildContesto [(varNome (nomeV "e"), T), (varNome(nomeV "g"), T)];


print (stampaContesto x);
print (stampaContesto y);
((concatenaContesto (x,y)));

(*
val chiama = chiamataMetodoS((newS (nomeCl "Classe2")) ,
							nomeM "metodo3",
							[varExprS ( nomeV "v")]
							);
print (stampaProgrammaS esempio);
print (stampaNomeTipoT( cercaTipoRightValueInContesto(esempio, buildContesto [(varNome(nomeV "v"), classeT(nomeCl "Classe2"))], chiama )) ^ "\n");

val metodoSintattico = defMetodoS ( intS, nomeM "metodo2", [defVarS (intS, nomeV "input")], [], [assegnamentoVarS(nomeV "input", intExprS 5), returnS (varExprS (nomeV "input"))]);

controllaTipoMetodo(esempio, buildContesto [] , metodoSintattico );

print (stampaMetodoS metodoSintattico);

val x=buildContesto [(varNome (nomeV "i"), intT), (varNome(nomeV "e"), intT)];

val y=buildContesto [(varNome (nomeV "e"), T), (varNome(nomeV "g"), T)];


print (stampaContesto x);
print (stampaContesto y);
print (stampaContesto (concatenaContesto (x,y)));
print (stampaContesto ( concatenaContesto (x,y)));
print( "Cerca tipoSintattico variabileSintattica: " ^ (stampaNomeTipoT (cercaVarInContesto (concatenaContesto (x,y), nomeV "g"))) ^ "\n");
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
print (stampaProgrammaS esempio);
*)

