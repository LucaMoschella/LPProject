use "Sintassi.sml";

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FUNZIONI DI UTLITA INDIPENDENTI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

fun tipoSintatticoToSemantico ( intS   ) = intT
	| tipoSintatticoToSemantico ( classeS c ) = classeT c;

fun concatenaContesto ( (buildContesto l1), buildContesto l2) = buildContesto ( l1 @ l2);

(* aggiungi al buildContesto una lista di variabili*)
fun addVarsToContesto( buildContesto lc, [] ) = buildContesto lc 
	| addVarsToContesto( buildContesto lc, defVarS( tipoSintattico, nomeV v)::l ) =
		addVarsToContesto( buildContesto ((varNome (nomeV v),  tipoSintatticoToSemantico ( tipoSintattico ))::lc), l);

fun equalVarPiuVar ( varNome( nomeV v ), nomeV s ) = ( v = s )
	| equalVarPiuVar (  this, nomeV s ) = raise VarNameNotValid(nomeV s);

fun cercaVarPiuInContesto ( buildContesto [],v ) =  raise UnknownVar( v )
	| cercaVarPiuInContesto ( buildContesto ((n1,t1)::l), v ) = 
		if ((n1 = v)) then t1 else cercaVarPiuInContesto( buildContesto l, v);

fun getNomeClasseDaTipoT( classeT n) = n
 	| getNomeClasseDaTipoT( _ ) = raise TypeIsNotAClass;

fun getExtendedClass( defClasseS (nomeclasse, nomeclasseestesa, campi, metodi) ) = nomeclasseestesa;

fun cercaClasseInProgramma ( programmaSintattico, Object ) = defClasseS(Object, Object, [], [])	
	| cercaClasseInProgramma ( codiceS [], nomeCl nclasse ) = raise ClassNotFound(nomeCl nclasse)
	| cercaClasseInProgramma ( codiceS (  (defClasseS (nomeCl c, ce, lv, lm)) ::l ), nomeCl nclasse ) = 
				if (nclasse = c) then defClasseS (nomeCl c, ce, lv, lm) else cercaClasseInProgramma( codiceS l, nomeCl nclasse)
	| cercaClasseInProgramma ( codiceS (  (defClasseS (Object, ce, lv, lm)) ::l ), nomeCl nclasse ) =  
				cercaClasseInProgramma( codiceS l, nomeCl nclasse);

fun esisteClasseInProgramma ( programmaSintattico, Object ) = true	
	| esisteClasseInProgramma ( codiceS [], nomeCl nclasse ) = false
	| esisteClasseInProgramma ( codiceS (  (defClasseS (nomeCl c, ce, lv, lm)) ::l ), nomeCl nclasse ) = 
				if (nclasse = c) then true else esisteClasseInProgramma( codiceS l, nomeCl nclasse)
	| esisteClasseInProgramma ( codiceS (  (defClasseS (Object, ce, lv, lm)) ::l ), nomeCl nclasse ) =  
				esisteClasseInProgramma( codiceS l, nomeCl nclasse);

(* c1 è più in alto nella gerarchia di c2 *)
fun isSottoClasse( programmaSintattico, Object, Object) = true
 |	isSottoClasse( programmaSintattico, Object, nomeCl c2) = true
 |	isSottoClasse( programmaSintattico, nomeCl c1, Object) = false
 | isSottoClasse( programmaSintattico, nomeCl c1, nomeCl c2) = 
 	if (c1 = c2) 
 	then true 
 	else  (isSottoClasse(programmaSintattico, nomeCl c1,  getExtendedClass(cercaClasseInProgramma(programmaSintattico,nomeCl c2))));

(* il secondo è compatibile con il primo*)
fun compatibleTipoSintSem (programmaSintattico, intS, intT ) = true
	| compatibleTipoSintSem (programmaSintattico, intS, classeT( c) ) = false
	| compatibleTipoSintSem (programmaSintattico, intS, T ) = false
	| compatibleTipoSintSem (programmaSintattico, c, T ) = true
	| compatibleTipoSintSem (programmaSintattico, classeS( c), intT ) = false
	| compatibleTipoSintSem (programmaSintattico, classeS( Object), classeT( Object) ) = true
	| compatibleTipoSintSem (programmaSintattico, classeS( Object), classeT( nomeCl c2) ) = false
	| compatibleTipoSintSem (programmaSintattico, classeS( nomeCl c1), classeT( Object) ) = false
	| compatibleTipoSintSem (programmaSintattico, classeS( nomeCl c1), classeT( nomeCl c2) ) = isSottoClasse(programmaSintattico, nomeCl c1, nomeCl c2);

fun compatibleTipoSemSem (programmaSintattico, classeT( nomeCl c1), classeT( nomeCl c2) ) = isSottoClasse(programmaSintattico, nomeCl c1, nomeCl c2)
	| compatibleTipoSemSem (programmaSintattico, classeT c, T) = true
	| compatibleTipoSemSem (programmaSintattico, t1, t2) = (t1 = t2);

fun equalList ([],[])=true
	| equalList (_,[])=false
	| equalList ([],_)=false
	| equalList (n::l, n1::l1)=if(n=n1)then equalList(l,l1)else false;

fun parametriCompatibili(programmaSintattico, [], [] ) = true
	| parametriCompatibili(programmaSintattico, defVarS(t,n)::l, [] ) = false
	| parametriCompatibili(programmaSintattico, [], t::l ) = false
	| parametriCompatibili(programmaSintattico, (defVarS(t1,n))::l1, t2::l2 ) = if( not (compatibleTipoSintSem(programmaSintattico,t1,t2))) then false else parametriCompatibili(programmaSintattico,l1,l2 );


(********** tipo semantico da un nome di campo sintattico **********)
fun cercaTipoCampoinClasse ( programmaSintattico, defClasseS (Object, _, _, _), nomeC campoSintattico ) = raise FieldNotFound(nomeC campoSintattico)
	| cercaTipoCampoinClasse (programmaSintattico,  defClasseS (nomeCl _, classeEstesa, [], _), nomeC campoSintattico ) = cercaTipoCampoinClasse( programmaSintattico, cercaClasseInProgramma(programmaSintattico,classeEstesa ), nomeC campoSintattico)
	| cercaTipoCampoinClasse ( programmaSintattico, defClasseS (nomeCl classeSintattica, ext, (defCampoS( t, nomeC nc, r))::campi, metodi), nomeC campoSintattico ) =
			if( nc = campoSintattico ) then tipoSintatticoToSemantico ( t ) else cercaTipoCampoinClasse(programmaSintattico, defClasseS (nomeCl classeSintattica, ext, campi, metodi), nomeC campoSintattico );

fun ftype( programmaSintattico, nomec, nomecl) = cercaTipoCampoinClasse(programmaSintattico, cercaClasseInProgramma ( programmaSintattico, nomecl ), nomec );

(********** tipo semantico da un nome di metodo sintattico **********)

fun cercaTipoMetodoInGerarchiaClasse(programmaSintattico, defClasseS (Object, _, _, _), nomeM metodoSintattico, parametri ) = raise MethodNotFound(nomeM metodoSintattico)

	| cercaTipoMetodoInGerarchiaClasse(programmaSintattico,  defClasseS (nomeCl _, classeEstesa, _, []), nomeM metodoSintattico, parametri ) = 
			cercaTipoMetodoInGerarchiaClasse( programmaSintattico, cercaClasseInProgramma(programmaSintattico, classeEstesa ), nomeM metodoSintattico, parametri)

	| cercaTipoMetodoInGerarchiaClasse ( programmaSintattico, defClasseS (nomeCl classeSintattica, ext, campi, (defMetodoS(t,nomeM m,args,locals,cmds))::metodi), nomeM metodoSintattico,parametri ) =
			if( (m = metodoSintattico) andalso (parametriCompatibili(programmaSintattico, args, parametri))) (* parametri deve contere tipi dal datatype types*)
				then tipoSintatticoToSemantico ( t ) 
				else cercaTipoMetodoInGerarchiaClasse(programmaSintattico, defClasseS (nomeCl classeSintattica, ext, campi, metodi), nomeM metodoSintattico, parametri );


fun mtype( programmaSintattico, nomem, nomecl, tipi) = 
	cercaTipoMetodoInGerarchiaClasse(programmaSintattico, cercaClasseInProgramma ( programmaSintattico, nomecl ), nomem, tipi );


fun estraiTipoSemantico( varExprT ( _, x)) = x
	|estraiTipoSemantico( intExprT ( _, x )) = x
	|estraiTipoSemantico( thisT ( x)) = x
	|estraiTipoSemantico( superT ( x )) = x
	|estraiTipoSemantico( nullT ( x )) = x
	|estraiTipoSemantico( newT ( _, x )) = x
	|estraiTipoSemantico( accessoCampoT ( _, _, x )) = x
	|estraiTipoSemantico( chiamataMetodoT ( _, _, _, x)) = x;

fun getNomeVarDaExpT( varExprT( nomeV v, _)) = v
	| getNomeVarDaExpT( _ ) = raise ExpIsNotAVar;
	



(* %%%%%%%%%%%%%%%%% REGOLE PER LA TRADUZIONE IN PROGRAMMA TIPATO %%%%%%%%%%%%%%%%%%%%% *)

fun getListaTipiArgs ( [] ) = []
	| getListaTipiArgs ( (defVarS(t,n))::l ) = 
		(tipoSintatticoToSemantico t) :: (getListaTipiArgs ( l ))

fun getListaTipiExpr (programmaSintattico, cont, [] ) = []
	| getListaTipiExpr ( programmaSintattico, cont,r::l ) = 
		(estraiTipoSemantico(espressioneStoT(programmaSintattico, cont, r))) :: (getListaTipiExpr (  programmaSintattico, cont,l ))

and espressioneListStoT( programmaSintattico, cont, [] ) = []
	| espressioneListStoT( programmaSintattico, cont, a::l ) = 
		espressioneStoT(programmaSintattico, cont, a)::(espressioneListStoT( programmaSintattico, cont, l ))

and	espressioneStoT( programmaSintattico, cont, varExprS(nomeV v)  ) = 
		varExprT(nomeV v, cercaVarPiuInContesto( cont, varNome (nomeV v)))

	| espressioneStoT( programmaSintattico, cont, intExprS n) = 
		intExprT( n, intT)

	| espressioneStoT( programmaSintattico, cont, newS( c)) = 
		newT( c, classeT c)

	| espressioneStoT( programmaSintattico, cont, ( nullS )) = 
		nullT( T ) 

	| espressioneStoT( programmaSintattico, cont, ( thisS )) = 
		 thisT( cercaVarPiuInContesto(cont, this))

	| espressioneStoT( programmaSintattico, cont, ( superS )) = 
		superT(classeT (getExtendedClass( cercaClasseInProgramma(programmaSintattico, getNomeClasseDaTipoT( cercaVarPiuInContesto(cont, this))))))

	| espressioneStoT( programmaSintattico, cont, accessoCampoS( v, c) ) = 
		let val expTyped = espressioneStoT( programmaSintattico, cont, v)
		in
			accessoCampoT( expTyped, c, ftype(programmaSintattico, c, getNomeClasseDaTipoT( estraiTipoSemantico expTyped)))
		end
		
	| espressioneStoT( programmaSintattico, cont, chiamataMetodoS( v, m, args) ) = 
		let val expTyped = espressioneStoT( programmaSintattico, cont, v)
		in
			chiamataMetodoT(expTyped, 
							m ,
							espressioneListStoT (programmaSintattico, cont, args),
							mtype(	programmaSintattico, 
									m, 
									getNomeClasseDaTipoT( estraiTipoSemantico(expTyped) ),
									getListaTipiExpr (programmaSintattico, cont, args )
								)
				)
		end;


fun variabileListStoT( [] ) = []
	| variabileListStoT( (defVarS( t, n))::l) = 
	(defVarT(t,n,tipoSintatticoToSemantico t))::(variabileListStoT(l));

(********** controllo override **********)

fun cercaMetodoInClasse(programmaSintattico, defClasseS (Object, _, _, _), nomeM metodoSintattico, parametri ) = raise MethodNotFound(nomeM metodoSintattico)

	| cercaMetodoInClasse(programmaSintattico,  defClasseS ( _, _, _, []), nomeM metodoSintattico, parametri ) = raise MethodNotFound(nomeM metodoSintattico)

	| cercaMetodoInClasse ( programmaSintattico, defClasseS (nomeCl classeSintattica, ext, campi, (defMetodoS(t, nomeM m, args, locals, cmds))::metodi),  
																									nomeM metodoSintattico, parametri ) =
			if( (m = metodoSintattico) andalso (equalList(getListaTipiArgs(args), parametri) ) )  (* parametri deve contere tipi dal datatype types*)
				then 
					defMetodoS(t, nomeM m, args, locals, cmds)
				else 
					cercaMetodoInClasse(programmaSintattico, defClasseS (nomeCl classeSintattica, ext, campi, metodi), nomeM metodoSintattico, parametri );


fun esisteMetodoInClasse(programmaSintattico, defClasseS (Object, _, _, _), nomeM metodoSintattico, parametri ) = false

	| esisteMetodoInClasse(programmaSintattico,  defClasseS ( _, _, _, []), nomeM metodoSintattico, parametri ) = false

	| esisteMetodoInClasse ( programmaSintattico, defClasseS (nomeCl classeSintattica, ext, campi, (defMetodoS(t, nomeM m, args, locals, cmds))::metodi),  
																									nomeM metodoSintattico, parametri ) =
			( (m = metodoSintattico) andalso (equalList(getListaTipiArgs(args), parametri))); (* parametri deve contere tipi dal datatype types*)
				

fun controlloOverride (programmaSintattico, defMetodoS (ts,  nomeM n, args, locals, commands), Object ) = true

	| controlloOverride (programmaSintattico, defMetodoS (ts,  nomeM n, args, locals, commands), nomeclasse ) = 
		let
			val nomeclasseestesa = getExtendedClass(cercaClasseInProgramma(programmaSintattico, nomeclasse))
			val defclasseestesa = cercaClasseInProgramma(programmaSintattico, nomeclasseestesa)
			val listatipi = getListaTipiArgs(args)
		in
			if (esisteMetodoInClasse(programmaSintattico, defclasseestesa,  nomeM n, listatipi) )
			then
				let 
					val (defMetodoS(ts2, _, _, _,_)) = cercaMetodoInClasse(programmaSintattico, defclasseestesa,  nomeM n, listatipi)
				in
					if (ts2=ts)
					then 
						controlloOverride( programmaSintattico,defMetodoS(ts, nomeM n,args,locals,commands), nomeclasseestesa) 
					else 
						false
				end
			else
				controlloOverride(programmaSintattico,defMetodoS(ts, nomeM n,args,locals,commands), nomeclasseestesa) 
		end;

(********************************************************************)

(********** controllo doppione: true non ci osno doppioni **********)

fun controlloMetodiDoppiApp( defClasseS (Object, _, _, _), _, _, found ) = true

	|controlloMetodiDoppiApp( defClasseS (nomeCl classeSintattica, ext, campi, []), nomemetodo, tipiargs, found ) = true

	| controlloMetodiDoppiApp( defClasseS (nomeCl classeSintattica, ext, campi, (defMetodoS(t, m, args, locals, cmds))::metodi), nomemetodo, tipiargs, found ) =
		if( (m = nomemetodo) andalso (equalList(getListaTipiArgs(args), tipiargs) ))
		then
			if( found )
			then
				false
			else
				controlloMetodiDoppiApp( defClasseS (nomeCl classeSintattica, ext, campi, metodi), nomemetodo, tipiargs, true )

		else
			controlloMetodiDoppiApp( defClasseS (nomeCl classeSintattica, ext, campi, metodi), nomemetodo, tipiargs, found );


fun controlloMetodiDoppi( definizioneclasse, nomemetodo, args) = controlloMetodiDoppiApp( definizioneclasse, nomemetodo, getListaTipiArgs(args), false );

(********************************************************************)
fun metodoStoTApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomeM nomemetodo, args, locals, [] ), metodoT, ret) = 
		if( ret ) then metodoT else raise ReturnNotFound(nomeM nomemetodo)

	| metodoStoTApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, (assegnamentoVarS( nomevar, v))::comandi ), 
													defMetodoT(ti, no, ar, lo, cmds), ret) = 
		let 
			val left = espressioneStoT( programmaSintattico, cont, varExprS nomevar  )
			val right = espressioneStoT( programmaSintattico, cont,  v  )
		in
			if( compatibleTipoSemSem( programmaSintattico, estraiTipoSemantico left, estraiTipoSemantico right) )
			then  
				metodoStoTApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, comandi ), 
															defMetodoT(ti, no, ar, lo, cmds @ [assegnamentoVarT( nomevar, right )]), ret)
			else 
				raise TypeErrorAssignVar(nomeM nomemetodo, left, right)
		end

	| metodoStoTApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, (assegnamentoCampoS( right1, nomecampo, right2))::comandi ), 
													defMetodoT(ti, no, ar, lo, cmds), ret) = 
		let 
			val left = espressioneStoT( programmaSintattico, cont, right1 )
			val field = espressioneStoT( programmaSintattico, cont,  accessoCampoS( right1, nomecampo)  )
			val right = espressioneStoT( programmaSintattico, cont,  right2  )
		in
			if( compatibleTipoSemSem( programmaSintattico, estraiTipoSemantico field, estraiTipoSemantico right ))
			then 
				metodoStoTApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, comandi ), 
															defMetodoT(ti, no, ar, lo, cmds @ [assegnamentoCampoT(left, nomecampo, right)]), ret)
			else
				raise TypeErrorAssignField(nomeM nomemetodo, left, field, right )
		end

	| metodoStoTApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, (returnS d)::comandi ), 
													defMetodoT(ti, no, ar, lo, cmds), ret) = 
		let 
			val right = espressioneStoT(  programmaSintattico, cont,  d  )
		in
			if( compatibleTipoSintSem( programmaSintattico,  tipoSintattico, estraiTipoSemantico right ))
			then metodoStoTApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, comandi ), 
															defMetodoT(ti, no, ar, lo, cmds @ [returnT(right)]), true)
			else
				raise TypeErrorReturn(nomeM nomemetodo, tipoSintattico, right)
		end

and metodoStoT( programmaSintattico, cont, nomeclasse, defMetodoS( t, n, args, locals, comandi ) ) = 
			if ( controlloOverride( programmaSintattico, defMetodoS( t, n, args, locals, comandi ), nomeclasse ) )
			then
				if  ( (controlloMetodiDoppi( cercaClasseInProgramma(programmaSintattico, nomeclasse) , n, args ))) 
				then
					metodoStoTApp( programmaSintattico, 
										addVarsToContesto( addVarsToContesto(cont, args), locals), 
										defMetodoS( t, n, args, locals, comandi ),
										defMetodoT( t, n, variabileListStoT( args ), variabileListStoT(locals), [] ),
										false
									)
				else
					raise MultipleMothodDef( n, nomeclasse )
			else raise OverrideMismatch( n, t, nomeclasse );


fun campoListStoT( programmaSintattico, cont, [] ) = []
	| campoListStoT( programmaSintattico, cont, ( defCampoS( t, nomeC n, r))::l ) = 
		let 
			val right = espressioneStoT(programmaSintattico, cont, r)
		in
			if ( compatibleTipoSintSem(programmaSintattico, t, estraiTipoSemantico right )) 
			then 
				defCampoT(t, nomeC n, right, tipoSintatticoToSemantico t) :: (campoListStoT( programmaSintattico, cont, l ))
			else
				raise TypeErrorDefField(t, nomeC n, right)
		end; 

fun metodoListStoT( programmaSintattico, cont, nomeclasse, []) = []
	| metodoListStoT( programmaSintattico, cont,nomeclasse, m::l) = 
		metodoStoT( programmaSintattico, cont, nomeclasse, m) :: (metodoListStoT(programmaSintattico, cont, nomeclasse, l));

fun classeStoT(programmaSintattico, defClasseS(nomeClasseCorrente, nomeClasseEstesa, campi, metodi)) =
	if( esisteClasseInProgramma( programmaSintattico, nomeClasseEstesa))
	then 
		defClasseT( nomeClasseCorrente, 
					nomeClasseEstesa, 
					campoListStoT( programmaSintattico, 
									buildContesto[(this,classeT(nomeClasseCorrente))], campi),
					metodoListStoT( programmaSintattico, 
									buildContesto[(this,classeT(nomeClasseCorrente))], nomeClasseCorrente, metodi))		
	else
		raise ClassNotFound(nomeClasseCorrente);

	
fun programmaStoTApp(programmaSintattico, codiceS [] ) = codiceT []
	| programmaStoTApp(programmaSintattico, codiceS (c::l) ) = 
		let 
			val codiceT(x) = programmaStoTApp(programmaSintattico,codiceS l)
		in
			codiceT( classeStoT(programmaSintattico,c) :: x)
		end;

fun programmaStoT( programmaSintattico ) = 
	programmaStoTApp(programmaSintattico, programmaSintattico)
	handle  VarNameNotValid x => ( print ("ERRORE: Il nome <" ^ (stampaNomeVar x) ^ "> non è un nome di variabile è valido.\n\n"); codiceT [] )
			| UnknownVar x => ( print ("ERRORE: La variabile <" ^ (stampaNomeVarPiu x) ^ "> non è stata definita.\n\n"); codiceT [] )

			| FieldNotFound x => ( print ("ERRORE: Il campo <" ^ (stampaNomeCampo x) ^ "> non è stato trovato.\n\n"); codiceT [] )
			| MethodNotFound x => ( print ("ERRORE: Il metodo <" ^ (stampaNomeMetodo x) ^ "> non è stato trovato.\n\n"); codiceT [] )
			| ClassNotFound x => ( print ("ERRORE: La classe <" ^ (stampaNomeClasse x) ^ "> non è stata trovata.\n\n"); codiceT [] )
			| ReturnNotFound x => ( print ("ERRORE: Il metodo <" ^ (stampaNomeMetodo x) ^ "> non contiene un comando di return.\n\n"); codiceT [] )

			| TypeIsNotAClass => ( print ("ERRORE: Impossibile convertire l'espressione in una classe.\n\n"); codiceT [] )

			| TypeErrorDefField ( ts, n, e ) => 
				( print ("ERRORE: Impossibile inizializzare il campo <" ^ (stampaNomeCampo n) ^ ">, di tipo <" 
							^ (stampaNomeTipoS ts) ^ ">, con un espressione di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e )) ^ ">.\n\n"); codiceT [] )
			
			| TypeErrorReturn ( n, ts, e) => 
				( print ("ERRORE: Impossibile tornare un espressione di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e )) ^ 
							"> nel metodo <" ^ (stampaNomeMetodo n) ^ ">, che ha un tipo di ritorno <" ^ (stampaNomeTipoS( ts )) ^ ">.\n\n"); codiceT [] )
			
			| TypeErrorAssignVar (n, e1, e2) => 
				( print ("ERRORE: Impossibile assegnare alla variabile <" ^ (getNomeVarDaExpT e1) ^ "> di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e1 ))^ ">, nel metodo <" 
					^ (stampaNomeMetodo n) ^ ">, un espressione di tipo <"^ (stampaNomeTipoT( estraiTipoSemantico e2 )) ^"> .\n\n"); codiceT [] )
			
			
			| TypeErrorAssignField (n, e1, e2, e3) => 
				( print ("ERRORE: Il tipo del valore nell'assegnamento non è compatibile con il tipo del campo, nel metodo <" ^ (stampaNomeMetodo n) ^ ">.\n\n"); codiceT [] )

			| OverrideMismatch ( n, ts, cla ) =>
				( print ("ERRORE: Il metodo <" ^ (stampaNomeMetodo n) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ 
					"> effettua un Override cambiando il tipo di ritorno, in <" ^ (stampaNomeTipoS ts) ^ ">.\n\n"); codiceT [] )
			| MultipleMothodDef ( n, cla ) =>
				( print ("ERRORE: Il metodo <" ^ (stampaNomeMetodo n) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ 
					"> è definito più volte.\n\n"); codiceT [] );


use "ProgrammiEsempio.sml";

print (stampaProgrammaS programmaOverload);
val p = programmaStoT programmaOverload;
print (stampaProgrammaT p);
