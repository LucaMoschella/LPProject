use "Sintassi.sml";

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FUNZIONI DI UTLITA INDIPENDENTI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

fun tipoSintatticoToSemantico ( intS   ) = intT
	| tipoSintatticoToSemantico ( classeS c ) = classeT c;

fun concatenaContesto ( buildContesto l1 , buildContesto l2) = buildContesto ( l1 @ l2);

(* aggiungi al buildContesto una lista di variabili*)
fun addVarsToContesto( buildContesto lc, [] ) = buildContesto lc 
	| addVarsToContesto( buildContesto lc, defVarS( tipoSintattico, nomeV v)::l ) =
		addVarsToContesto( buildContesto ((varNome (nomeV v),  tipoSintatticoToSemantico ( tipoSintattico ))::lc), l);

fun equalVarPiuVar ( varNome( nomeV v ), nomeV s ) = ( v = s )
	| equalVarPiuVar (  this, nomeV s ) = raise VarNameNotValid(s);

fun cercaVarPiuInContesto ( buildContesto [],v ) = 
		(case v of
			varNome (nomeV s) => raise UnknownVar(s)
			| this => raise UnknownVar("this"))
	| cercaVarPiuInContesto ( buildContesto ((n1,t1)::l), v ) = 
		if ((n1 = v)) then t1 else cercaVarPiuInContesto( buildContesto l, v);

fun getNomeClasseDaTipo( intT ) =  raise TypeIsNotAClass
 	| getNomeClasseDaTipo( T ) = raise TypeIsNotAClass
	| getNomeClasseDaTipo( classeT n) = n;

fun getExtendedClass( defClasseS (nomeclasse, nomeclasseestesa , campi , metodi) ) = nomeclasseestesa;

fun cercaClasseInProgramma ( programmaSintattico, Object ) = defClasseS(Object, Object, [], [])	
	| cercaClasseInProgramma ( codiceS [], nomeCl nclasse ) = raise ClassNotFound(nclasse)
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



(* -> unificate con cerca var piu in contesto
fun cercaVarInContesto ( buildContesto [],nomeV s ) = raise NonTypedVar
	| cercaVarInContesto ( buildContesto ((n1,t1)::l), nomeV s ) = 
		if (equalVarPiuVar(n1, nomeV s)) then t1 else cercaVarInContesto( buildContesto l, nomeV s);

fun cercaThisInContesto ( buildContesto [] ) = raise NonTypedThis
	| cercaThisInContesto ( buildContesto ((this,t1)::l) ) = t1
	|  cercaThisInContesto ( buildContesto ((_,t1)::l) ) =cercaThisInContesto( buildContesto l );
*)

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

fun parametriCompatibili(programmaSintattico, [], [] ) = true
	| parametriCompatibili(programmaSintattico, defVarS(t,n)::l, [] ) = false
	| parametriCompatibili(programmaSintattico, [], t::l ) = false
	| parametriCompatibili(programmaSintattico, (defVarS(t1,n))::l1, t2::l2 ) = if( not (compatibleTipoSintSem(programmaSintattico,t1,t2))) then false else parametriCompatibili(programmaSintattico,l1,l2 );


(********** tipo semantico da un nome di campo sintattico **********)
fun cercaTipoCampoinClasse ( programmaSintattico, defClasseS (Object, _ , _ , _), nomeC campoSintattico ) = raise FieldNotFound(campoSintattico)
	| cercaTipoCampoinClasse (programmaSintattico,  defClasseS (nomeCl _, classeEstesa , [] , _), nomeC campoSintattico ) = cercaTipoCampoinClasse( programmaSintattico, cercaClasseInProgramma(programmaSintattico,classeEstesa ), nomeC campoSintattico)
	| cercaTipoCampoinClasse ( programmaSintattico, defClasseS (nomeCl classeSintattica, ext , (defCampoS( t, nomeC nc, r))::campi , metodi), nomeC campoSintattico ) =
			if( nc = campoSintattico ) then tipoSintatticoToSemantico ( t ) else cercaTipoCampoinClasse(programmaSintattico, defClasseS (nomeCl classeSintattica, ext , campi , metodi), nomeC campoSintattico );

fun ftype( programmaSintattico, nomec, nomecl) = cercaTipoCampoinClasse(programmaSintattico, cercaClasseInProgramma ( programmaSintattico, nomecl ), nomec );

(********** tipo semantico da un nome di metodo sintattico **********)

fun cercaTipoMetodoInClasse(programmaSintattico, defClasseS (Object, _ , _ , _), nomeM metodoSintattico, parametri ) = raise MethodNotFound(metodoSintattico)

	| cercaTipoMetodoInClasse(programmaSintattico,  defClasseS (nomeCl _, classeEstesa , _ , []), nomeM metodoSintattico, parametri ) = 
			cercaTipoMetodoInClasse( programmaSintattico, cercaClasseInProgramma(programmaSintattico,classeEstesa ), nomeM metodoSintattico, parametri)

	| cercaTipoMetodoInClasse ( programmaSintattico, defClasseS (nomeCl classeSintattica, ext , campi , (defMetodoS(t,nomeM m,args,locals,cmds))::metodi), nomeM metodoSintattico,parametri ) =
			if( (m = metodoSintattico) andalso (parametriCompatibili(programmaSintattico, args, parametri))) (* parametri deve contere tipi dal datatype types*)
				then tipoSintatticoToSemantico ( t ) 
				else cercaTipoMetodoInClasse(programmaSintattico, defClasseS (nomeCl classeSintattica, ext , campi , metodi), nomeM metodoSintattico, parametri );


fun mtype( programmaSintattico, nomem, nomecl, tipi) = cercaTipoMetodoInClasse(programmaSintattico, cercaClasseInProgramma ( programmaSintattico, nomecl ), nomem, tipi );


(* %%%%%%%%%%%%%%%%% REGOLE PER LA TRADUZIONE IN PROGRAMMA TIPATO %%%%%%%%%%%%%%%%%%%%% *)

fun estraiTipoSemantico( varExprT ( _, x)) = x
	|estraiTipoSemantico( intExprT ( _, x )) = x
	|estraiTipoSemantico( thisT ( x)) = x
	|estraiTipoSemantico( superT ( x )) = x
	|estraiTipoSemantico( nullT ( x )) = x
	|estraiTipoSemantico( newT ( _, x )) = x
	|estraiTipoSemantico( accessoCampoT ( _, _, x )) = x
	|estraiTipoSemantico( chiamataMetodoT ( _, _ , _, x)) = x;

fun getListaTipiArgs (programmaSintattico, cont, [] ) = []
	| getListaTipiArgs ( programmaSintattico, cont,r::l ) = 
		(estraiTipoSemantico(espressioneToTipata(programmaSintattico, cont, r))) :: (getListaTipiArgs (  programmaSintattico, cont,l ))

and espressioneListToTipata( programmaSintattico, cont, [] ) = []
	| espressioneListToTipata( programmaSintattico, cont, a::l ) = 
		espressioneToTipata(programmaSintattico, cont, a)::(espressioneListToTipata( programmaSintattico, cont, l ))

and	espressioneToTipata( programmaSintattico, cont, varExprS(nomeV v)  ) = 
		varExprT(nomeV v, cercaVarPiuInContesto( cont, varNome (nomeV v)))

	| espressioneToTipata( programmaSintattico, cont, intExprS n) = 
		intExprT( n, intT)

	| espressioneToTipata( programmaSintattico, cont, newS( c)) = 
		newT( c, classeT c)

	| espressioneToTipata( programmaSintattico, cont, ( nullS )) = 
		nullT( T ) 

	| espressioneToTipata( programmaSintattico, cont, ( thisS )) = 
		 thisT( cercaVarPiuInContesto(cont, this))

	| espressioneToTipata( programmaSintattico, cont, ( superS )) = 
		superT(classeT (getExtendedClass( cercaClasseInProgramma(programmaSintattico, getNomeClasseDaTipo( cercaVarPiuInContesto(cont, this))))))

	| espressioneToTipata( programmaSintattico, cont, accessoCampoS( v, c) ) = 
		let val expTyped = espressioneToTipata( programmaSintattico, cont, v)
		in
			accessoCampoT( expTyped, c, ftype(programmaSintattico, c, getNomeClasseDaTipo( estraiTipoSemantico expTyped)))
		end
	| espressioneToTipata( programmaSintattico, cont, chiamataMetodoS( v, m, args) ) = 
		let val expTyped = espressioneToTipata( programmaSintattico, cont, v)
		in
			chiamataMetodoT(expTyped, 
							m ,
							espressioneListToTipata (programmaSintattico, cont, args),
							mtype(	programmaSintattico, 
									m, 
									getNomeClasseDaTipo( estraiTipoSemantico(expTyped) ),
									getListaTipiArgs (programmaSintattico, cont, args )
								)
				)
		end;


fun variabileListToTipata( [] ) = []
	| variabileListToTipata( (defVarS( t, n))::l) = 
	(defVarT(t,n,tipoSintatticoToSemantico t))::(variabileListToTipata(l));


fun metodoToTipatoApp( programmaSintattico, cont, defMetodoS(tipoSintattico, nomeM nomemetodo, args, locals, [] ), metodoT, ret) = 
	if( ret ) then metodoT else raise ReturnNotFound(nomemetodo)

| metodoToTipatoApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, (assegnamentoVarS( nomevar, v))::comandi ), 
												defMetodoT(ti, no, ar, lo, cmds), ret) = 
	let 
		val left = espressioneToTipata( programmaSintattico, addVarsToContesto( addVarsToContesto(cont, args), locals),  varExprS(nomevar)  )
		val right = espressioneToTipata( programmaSintattico, addVarsToContesto( addVarsToContesto(cont, args), locals),  v  )
	in
		if( compatibleTipoSemSem( programmaSintattico , estraiTipoSemantico left, estraiTipoSemantico right) )
		then  
			metodoToTipatoApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, comandi ), 
														defMetodoT(ti, no, ar, lo, cmds @ [assegnamentoVarT(nomevar, right )]) , ret)
		else 
			raise TypeErrorAssignVar(nomemetodo)
	end

| metodoToTipatoApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, (assegnamentoCampoS( right1, nomecampo, right2))::comandi ), 
												defMetodoT(ti, no, ar, lo, cmds), ret) = 
	let 
		val left = espressioneToTipata( programmaSintattico, addVarsToContesto( addVarsToContesto(cont, args), locals), right1 )
		val field = espressioneToTipata( programmaSintattico, addVarsToContesto( addVarsToContesto(cont, args), locals),  accessoCampoS( right1, nomecampo)  )
		val right = espressioneToTipata( programmaSintattico, addVarsToContesto( addVarsToContesto(cont, args), locals),  right2  )
	in
		if( compatibleTipoSemSem( programmaSintattico, estraiTipoSemantico field, estraiTipoSemantico right ))
		then 
			metodoToTipatoApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, comandi ), 
														defMetodoT(ti, no, ar, lo, cmds @ [assegnamentoCampoT(left, nomecampo, right)]) , ret)
		else
			raise TypeErrorAssignField(nomemetodo)
	end

| metodoToTipatoApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, (returnS d)::comandi ), 
												defMetodoT(ti, no, ar, lo, cmds), ret) = 
	let 
		val right = espressioneToTipata(  programmaSintattico, addVarsToContesto( addVarsToContesto(cont, args), locals),  d  )
	in
		if( compatibleTipoSintSem( programmaSintattico ,  tipoSintattico , estraiTipoSemantico right ))
		then metodoToTipatoApp( programmaSintattico, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, comandi ), 
														defMetodoT(ti, no, ar, lo, cmds @ [returnT(right)]) , true)
		else
			raise TypeErrorReturn(nomemetodo)
	end

and metodoToTipato( programmaSintattico, cont, defMetodoS( t, n, args, locals, comandi ) ) = 
			metodoToTipatoApp( programmaSintattico, 
								cont, 
								defMetodoS( t, n, args, locals, comandi ),
								defMetodoT( t, n, variabileListToTipata( args ), variabileListToTipata(locals), [] ),
								false
							);


fun campoListToTipato( programmaSintattico, cont, [] ) = []
| campoListToTipato( programmaSintattico, cont, ( defCampoS( t, nomeC n, r))::l ) = 
	let 
		val right = espressioneToTipata(programmaSintattico, cont, r)
	in
		if ( compatibleTipoSintSem(programmaSintattico, t, estraiTipoSemantico right )) 
		then 
			defCampoT(t, nomeC n, right, tipoSintatticoToSemantico t) :: (campoListToTipato( programmaSintattico, cont, l ))
		else
			raise TypeErrorField(n)
	end 

fun metodoListToTipato( programmaSintattico, cont, []) = []
	| metodoListToTipato( programmaSintattico, cont, m::l) = 
		metodoToTipato( programmaSintattico, cont, m) :: (metodoListToTipato(programmaSintattico, cont, l));

fun classeToTipata(programmaSintattico, defClasseS(nomeClasseCorrente, nomeClasseEstesa, campi, metodi)) =
	if( esisteClasseInProgramma( programmaSintattico, nomeClasseEstesa))
	then 
		defClasseT( nomeClasseCorrente, 
					nomeClasseEstesa, 
					campoListToTipato( programmaSintattico, 
									buildContesto[(this,classeT(nomeClasseCorrente))], campi),
					metodoListToTipato( programmaSintattico, 
									buildContesto[(this,classeT(nomeClasseCorrente))], metodi))		
	else
		(case nomeClasseCorrente of
			Object =>  raise ClassNotFound("Object")
			| nomeCl c => raise ClassNotFound(c));

	
fun programmaToTipatoApp(programmaSintattico, codiceS [] ) = codiceT []
	| programmaToTipatoApp(programmaSintattico, codiceS (c::l) ) = 
		let 
			val codiceT(x) = programmaToTipatoApp(programmaSintattico,codiceS l)
		in
			codiceT( classeToTipata(programmaSintattico,c) :: x)
		end;

fun programmaToTipato( programmaSintattico ) = 
	programmaToTipatoApp(programmaSintattico, programmaSintattico)
	handle  VarNameNotValid x => ( print ("\nERRORE: Il nome: " ^ x ^ ", non è un nome di variabile è valido.\n"); codiceT [] )
			| UnknownVar x => ( print ("\nERRORE: La variabile: " ^ x ^ ", non è stata definita.\n"); codiceT [] )

			| FieldNotFound x => ( print ("\nERRORE: Il campo: " ^ x ^ ", non è stato trovato.\n"); codiceT [] )
			| MethodNotFound x => ( print ("\nERRORE: Il metodo: " ^ x ^ ", non è stato trovato.\n"); codiceT [] )
			| ClassNotFound x => ( print ("\nERRORE: La classe: " ^ x ^ ", non è stata trovata.\n"); codiceT [] )
			| ReturnNotFound x => ( print ("\nERRORE: Il metodo: " ^ x ^ ", non contiene un comando di return.\n"); codiceT [] )

			| TypeIsNotAClass => ( print ("\nERRORE: Impossibile convertire l'espressione in una classe.\n"); codiceT [] )

			| TypeErrorField x => ( print ("\nERRORE: I tipi non sono compatibili durante l'inizializzazione del campo: " ^ x ^ ".\n"); codiceT [] )
			| TypeErrorReturn x => ( print ("\nERRORE: Il tipo di return non è compatibile con il tipo di ritorno dichiarato nel metodo: " ^ x ^ ".\n"); codiceT [] )
			| TypeErrorAssignVar x => ( print ("\nERRORE: Il tipo del valore nell'assegnamento non è compatibile con il tipo della variabile, nel metodo: " ^ x ^ ".\n"); codiceT [] )
			| TypeErrorAssignField x => ( print ("\nERRORE: Il tipo del valore nell'assegnamento non è compatibile con il tipo del campo, nel metodo: " ^ x ^ ".\n"); codiceT [] );


use "ProgrammiEsempio.sml";

programmaToTipato esempio;
programmaToTipato esempioDispensa;
programmaToTipato esempio;