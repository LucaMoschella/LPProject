use "Sintassi.sml";
use "StruttureDati.sml";
use "Exception.sml";
use "PrintToJava.sml";
use "ProgrammiEsempio.sml";

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONVERSIONE E COMPATIBILITA FRA TIPI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun tipoValido ( programMap, intS ) = true
	| tipoValido ( programMap, classeS c ) = containsKey(programMap, c);

fun tipoSintToSem ( intS ) = intT
	| tipoSintToSem ( classeS c ) = classeT c;

fun tipoSemToSint ( intT ) = intS
	| tipoSemToSint ( classeT c ) = classeS c
	| tipoSemToSint( T ) = raise WrongSemToSint;

fun getExtendedClassS( defClasseS (_, nomeclasseestesa, _, _) ) = nomeclasseestesa;

(* c1 è più in alto nella gerarchia di c2 *)
fun isSottoClasse( programMap, Object, Object) = true
 |	isSottoClasse( programMap, Object, nomeCl c2) = true
 |	isSottoClasse( programMap, nomeCl c1, Object) = false
 |  isSottoClasse( programMap, nomeCl c1, nomeCl c2) = if (c1 = c2) then true 
 	else  isSottoClasse(programMap, nomeCl c1,  getExtendedClassS(get(programMap,nomeCl c2)));

(* il secondo è compatibile con il primo*)
fun   compatibleTipoSemSem (programMap, T, T) = false
	| compatibleTipoSemSem (programMap, intT, T) = false
	| compatibleTipoSemSem (programMap, classeT( nome ), T) = true
  	| compatibleTipoSemSem (programMap, T, intT) = false
	| compatibleTipoSemSem (programMap, intT, intT) = true
	| compatibleTipoSemSem (programMap, classeT( nome ), intT) = false
  	| compatibleTipoSemSem (programMap, T, classeT( nome )) = false
	| compatibleTipoSemSem (programMap, intT, classeT( nome )) = false
	| compatibleTipoSemSem (programMap, classeT( nome1 ), classeT( nome2 )) = isSottoClasse(programMap, nome1, nome2);

fun compatibleTipoSintSem (programMap, ts, tt ) = compatibleTipoSemSem(programMap, tipoSintToSem ts ,tt);

fun compatibleTipoSintSint( programMap, ts1, ts2) = compatibleTipoSemSem(programMap, tipoSintToSem ts1, tipoSintToSem ts2);

fun compatibleTipiSemSem(programMap, [], [] ) = true
	| compatibleTipiSemSem(programMap, v::l, [] ) = false
	| compatibleTipiSemSem(programMap, [], t::l ) = false
	| compatibleTipiSemSem(programMap, t1::l1, t2::l2 ) = if( not (compatibleTipoSemSem(programMap,t1,t2))) then false else compatibleTipiSemSem(programMap,l1,l2 );
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ESTRAZIONE INFORMAZIONI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun estraiTipoSemantico( varExprT ( _, x)) = x
	| estraiTipoSemantico( intExprT ( _, x )) = x
	| estraiTipoSemantico( thisT ( x)) = x
	| estraiTipoSemantico( superT ( x )) = x
	| estraiTipoSemantico( nullT ( x )) = x
	| estraiTipoSemantico( newT ( _, x )) = x
	| estraiTipoSemantico( accessoCampoT ( _, _, x )) = x
	| estraiTipoSemantico( chiamataMetodoT ( _, _, _, x)) = x;

fun getNomeClasseDaTipoT( classeT n) = n
 	| getNomeClasseDaTipoT( _ ) = raise TypeIsNotAClass;
 
fun listVarSToTipoT( l ) = fList(l, fn defVarS(t, n) => tipoSintToSem t )
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* utili per sfruttare le funzioni già definite su dataList! *)
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COSTRUZIONE MAPPE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun buildVarSMap( l ) = headPutAllFun( buildData [], l, fn defVarS(t, n) => (n, defVarS(t, n)) );
fun buildCampiSMap( l ) = headPutAllFun( buildData [], l, fn defCampoS(t,n,s) => (n, defCampoS(t,n,s)));
fun buildMetodiSMap( l ) = headPutAllFun( buildData [], l, fn defMetodoS(t, m,args,locals,cmds) => ( (m, listVarSToTipoT args), defMetodoS(t, m,args,locals,cmds)) );
fun buildClassiSMap( codiceS l ) = headPutAllFun( buildData [(Object, defClasseS ( Object, Object, [], []))], l, fn defClasseS( c, ce, lv, lm) => (c, defClasseS( c, ce, lv, lm)) );

fun buildComandiSList( l ) = headPutAllFun( buildData [], l, fn assegnamentoVarS(n, e) => ("assegnamentoVar", assegnamentoVarS(n, e) )
														| assegnamentoCampoS(e1, n, e2) => ("assegnamentoCampo", assegnamentoCampoS(e1, n, e2))
														| returnS(e) => ("return", returnS(e)));
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TIPO SEMANTICO DI UN NOME CAMPO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun cSbody( programMap,  nomeclasse ) =  (fn defClasseS( nome , _ , campi, _)  => campi ) (get( programMap, nomeclasse ));

fun buildAllCampiSMap(programMap, Object) = buildData []
	| buildAllCampiSMap(programMap, nomeclasse) = concat( buildCampiSMap( cSbody( programMap,  nomeclasse ) ), buildAllCampiSMap(programMap, getExtendedClassS( get(programMap, nomeclasse) )) );

fun ftype( programMap, nomeclasse, nomecampo) = (fn defCampoS(t, _, _) => tipoSintToSem t) ( get( buildAllCampiSMap(programMap, nomeclasse), nomecampo))
	handle KeyNotFound => raise FieldNotFound(nomecampo, nomeclasse);
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TIPO SEMANTICO DI UN NOME METODO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun mSbody( programMap,  nomeclasse ) =  (fn defClasseS( nome , _ , _ , metodi)  => metodi ) (get( programMap, nomeclasse ));

fun buildAllMetodiSMap(programMap, Object) = buildData []
	| buildAllMetodiSMap(programMap, nomeclasse) = concat( buildMetodiSMap( mSbody( programMap,  nomeclasse ) ), buildAllMetodiSMap(programMap, getExtendedClassS( get(programMap, nomeclasse) )) );

fun mtype( programMap, nomeclasse, nomemetodo, tipi) = (fn defMetodoS(t, _, _, _, _) => tipoSintToSem t) 
	( find( buildAllMetodiSMap(programMap, nomeclasse), (nomemetodo, tipi), fn ((m1,t1), (m2,t2)) => (m1 = m2) andalso (compatibleTipiSemSem(programMap, t1, t2))) )
	handle KeyNotFound => raise MethodNotFound(nomemetodo, nomeclasse);
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% pronde solo quelli della classe attuale *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONTROLLO OVERRIDE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun controlloOverride (programMap, classebase, _, Object ) = true
	| controlloOverride (programMap, classebase, defMetodoS (baseype, n, args, l, c), nomeclasse ) = 
		let 
			val defmetodobase = defMetodoS (baseype, n, args, l, c)
			val defclasse = get(programMap, nomeclasse)
			val metodiMap = buildMetodiSMap( (fn defClasseS(_, _, _, metodi) => metodi) defclasse )
			val metodoKey = ( n, listVarSToTipoT args)
		in
			if not (containsKey( metodiMap, metodoKey ) ) then controlloOverride(programMap, classebase, defmetodobase, getExtendedClassS defclasse)
			else
				let 
					val supertype = (fn defMetodoS(t, _, _, _,_) => t) (get( metodiMap , metodoKey ))
				in
					if compatibleTipoSintSint( programMap, supertype, baseype)
					then controlloOverride( programMap, classebase, defmetodobase, getExtendedClassS defclasse ) 
					else raise TypeErrorOverrideMismatch( n, supertype, nomeclasse, baseype, classebase )
				end
		end;
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONTROLLO INIZIALIZZAZIONE VARIABILI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun headPutAss( l, v ) = headPutFun( l, v, fn assegnamentoVarS(nomeV n, e) => (("assegnamentoVar", n), assegnamentoVarS(nomeV n, e) )
									| assegnamentoCampoS(e1, nomeC n, e2) => (("assegnamentoCampo", n), assegnamentoCampoS(e1, nomeC n, e2))
									| returnS(e) => (("return", "notValid"), returnS(e)));

fun isInitialized(cmds,  nomeV v ) = containsKey(cmds, ("assegnamentoVar", v));
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%% REGOLE PER LA TRADUZIONE IN PROGRAMMA TIPATO %%%%%%%%%%%%%%%%%%%%% *)
fun	espressioneStoT( programMap, cont, varExprS( v ), executedCmds) = 
		if not (containsKey( cont, varPiuNome ( v ) )) then raise UnknownVar(  v  )
		else if not (isInitialized( executedCmds,  v  )) then raise VarNotInitialized(  v )
		else varExprT( v , get( cont, varPiuNome ( v )))

	| espressioneStoT( programMap, cont, intExprS n, executedCmds) = 
		intExprT( n, intT)

	| espressioneStoT( programMap, cont, newS( c), executedCmds) = 
		if not (tipoValido(programMap, classeS(c))) then raise TypeIsNotAClassNew(c)
		else newT( c, classeT c)

	| espressioneStoT( programMap, cont, ( nullS ), executedCmds) = 
		nullT( T ) 

	| espressioneStoT( programMap, cont, ( thisS ), executedCmds) = 
		thisT( get(cont, this) )

	| espressioneStoT( programMap, cont, ( superS ), executedCmds) = 
		superT( classeT( getExtendedClassS( get( programMap, getNomeClasseDaTipoT( get(cont, this) )) )))

	| espressioneStoT( programMap, cont, accessoCampoS( v, c) , executedCmds) = 
		let val expTyped = espressioneStoT( programMap, cont, v, executedCmds)
		in
			accessoCampoT( expTyped, c, ftype(programMap, getNomeClasseDaTipoT( estraiTipoSemantico expTyped), c))
		end
		
	| espressioneStoT( programMap, cont, chiamataMetodoS( v, m, args) , executedCmds) = 
		let val expTyped = espressioneStoT( programMap, cont, v, executedCmds)
		in
			chiamataMetodoT(expTyped, 
							m ,
							fList(args, fn x => espressioneStoT(programMap, cont, x, executedCmds)),
							mtype(	programMap, 
									getNomeClasseDaTipoT( estraiTipoSemantico(expTyped) ),
									m,
									fList(args, fn x => estraiTipoSemantico(espressioneStoT(programMap, cont, x, executedCmds)))
								)
				)
		end

and comandoStoT( programMap, cont, assegnamentoVarS( nomevar, v), defMetodoS( _, nomemetodo, _, _, _ ), nomeclasse, executedCmds) = 
		let 
			val left = espressioneStoT( programMap, cont, varExprS nomevar, executedCmds  )
			val right = espressioneStoT( programMap, cont,  v, executedCmds  )
		in
			if( compatibleTipoSemSem( programMap, estraiTipoSemantico left, estraiTipoSemantico right) )
			then assegnamentoVarT( nomevar, right )
			else raise TypeErrorAssignVar(nomemetodo, left, right, nomeclasse, nomevar)
		end

	| comandoStoT( programMap, cont, assegnamentoCampoS( right1, nomecampo, right2), defMetodoS( _, nomemetodo, _, _, _ ), nomeclasse, executedCmds) = 
		let 
			val left = espressioneStoT( programMap, cont, right1, executedCmds )
			val field = espressioneStoT( programMap, cont,  accessoCampoS( right1, nomecampo), executedCmds  )
			val right = espressioneStoT( programMap, cont,  right2, executedCmds  )
		in
			if( compatibleTipoSemSem( programMap, estraiTipoSemantico field, estraiTipoSemantico right ))
			then assegnamentoCampoT(left, nomecampo, right)
			else raise TypeErrorAssignField(nomemetodo, left, field, right, nomeclasse, nomecampo)
		end

	| comandoStoT( programMap, cont, returnS d, defMetodoS( t, nomemetodo, _, _, _ ), nomeclasse, executedCmds) = 
		let 
			val right = espressioneStoT(  programMap, cont,  d, executedCmds  )
		in
			if( compatibleTipoSintSem( programMap,  t, estraiTipoSemantico right ))
			then returnT(right)
			else raise TypeErrorReturn(nomemetodo, t, right, nomeclasse)
		end


and metodoStoT( programMap, cont, nomeclasse, defMetodoS( t, n, args, locals, comandi ) ) = 
				let 
					val argsMap = buildVarSMap args
					val localsMap = buildVarSMap locals
					val allVar = concat( argsMap, localsMap)
					val cmdsMap = buildComandiSList comandi
					val contestExpanded = headPutAllFun(cont, args @ locals, fn defVarS(t, v) => (varPiuNome v, tipoSintToSem t))
					val defmetodo = defMetodoS( t, n, args, locals, comandi )
				in				 
					(if containsDuplicatedKey (argsMap) then raise MultipleArgsDef( getDuplicatedKey argsMap, nomeclasse, n)
					else if containsDuplicatedKey (localsMap) then raise MultipleLocalsDef( getDuplicatedKey localsMap, nomeclasse, n)
					else if containsDuplicatedKey (allVar) then raise MultipleLocalsArgsDef( getDuplicatedKey allVar, nomeclasse, n)
					else if not (containsKey (cmdsMap, "return")) then raise ReturnNotFound(n, nomeclasse)
					else if not (tipoValido(programMap, t)) then raise TypeIsNotAClassMetodo(t, n, nomeclasse)
					else
						defMetodoT	( t, n, fList(args, fn defVarS( t, var) => 
													if not (tipoValido(programMap, t)) then raise TypeIsNotAClassArgs(t, var, n, nomeclasse)
													else defVarT(t,var, tipoSintToSem t)), 

											fList(locals, fn defVarS( t, var) =>
													if not (tipoValido(programMap, t)) then raise TypeIsNotAClassLocals(t, var, n, nomeclasse)
													else defVarT(t,var, tipoSintToSem t)),

											f2List(comandi, 
												fn (cmd, exec) => comandoStoT(programMap, contestExpanded, cmd, defmetodo, nomeclasse, exec), (* questo è il comando per convertire la lista *)
												fn (cmd, exec) => headPutAss(exec, cmd),	(* questo serve per dirgli cosa si deve portare dietro mentre scorre la lista: i comandi usati + quello attuale *)
												buildData []) (* inzialmente non sono stati eseguiti comandi*)
									))			
					handle VarNotInitialized v => raise VarNotInitializedInMetodo( v, nomeclasse, n )
						  | UnknownVar v => raise UnknownVarInMetodo( v, nomeclasse, n )
						  | TypeIsNotAClassNew c => raise TypeIsNotAClassNewInMetodo(c, nomeclasse, n)
						  | TypeIsNotAClass => raise TypeIsNotAClassInMetodo(n, nomeclasse)
				end

and classeStoT(programMap, defClasseS(nomeClasseCorrente, nomeClasseEstesa, campi, metodi)) =
	if(nomeClasseCorrente = nomeClasseEstesa) then raise ClassExtNotValid( nomeClasseCorrente )
	else 
		let 
			val campiMap = buildCampiSMap campi
			val metodiMap = buildMetodiSMap metodi
			val contex = buildContesto[(this,classeT(nomeClasseCorrente))]
		in
			(if containsDuplicatedKey (campiMap) then raise MultipleCampoDef( getDuplicatedKey campiMap, nomeClasseCorrente)
			else if containsDuplicatedKey (metodiMap) then raise MultipleMetodoDef( (fn (x,y) => x)(getDuplicatedKey metodiMap), nomeClasseCorrente)
			else if not (containsKey( programMap, nomeClasseEstesa)) then raise ClassNotFound(nomeClasseCorrente, nomeClasseEstesa)
			else 
				defClasseT( nomeClasseCorrente, 
							nomeClasseEstesa, 
							fList( campi, fn defCampoS( t, n, r) => 
										let 
											val right = espressioneStoT(programMap, contex, r, buildData [])
										in
											if not (tipoValido(programMap, t)) then raise TypeIsNotAClassCampo(t, n, nomeClasseCorrente)
											else if not (compatibleTipoSintSem(programMap, t, estraiTipoSemantico right )) then raise TypeErrorDefField(t, n, right, nomeClasseCorrente)
											else defCampoT(t, n, right, tipoSintToSem t) 
										end
									),
							fList(metodi, fn m => 	(	controlloOverride( programMap, nomeClasseCorrente, m, nomeClasseCorrente );
														metodoStoT( programMap, contex, nomeClasseCorrente, m))
													)
						))	
			handle VarNotInitialized v => raise VarNotInitializedInClasse( v, nomeClasseCorrente )
				| UnknownVar v => raise UnknownVarInClasse( v, nomeClasseCorrente )
				| TypeIsNotAClassNew c => raise TypeIsNotAClassNewInClasse(c, nomeClasseCorrente )
				| TypeIsNotAClass => raise TypeIsNotAClassInClasse( nomeClasseCorrente)				
		end

and programmaStoT( programma ) = 
	let 
		val programMap = buildClassiSMap programma
	in
		(if containsDuplicatedKey programMap then raise MultipleClasseDef( getDuplicatedKey programMap )
		else codiceT ( fList( (fn codiceS x => x) programma, fn c => classeStoT(programMap, c))) )
		
		handle ClassExtNotValid x => ( print ("ERROR: La classe <" ^ (stampaNomeClasse x) ^ "> non può estendere sé stessa.\n\n"); codiceT [] )

			| UnknownVarInMetodo( n, cla, m ) => ( print ("ERROR: La variabile <" ^ (stampaNomeVar n) ^ "> utilizzata nel metodo <" ^ (stampaNomeMetodo m) ^ "> della classe <"^ (stampaNomeClasse cla) ^ "> non è stata definita.\n\n"); codiceT [] )			
			| UnknownVarInClasse( n, cla ) => ( print ("ERROR: La variabile <" ^ (stampaNomeVar n) ^ "> utilizzata durante l'inizializzazione dei campi nella classe <" ^ (stampaNomeClasse cla) ^ "> non è stata definita.\n\n"); codiceT [] )

			| VarNotInitializedInMetodo( n, cla, m ) => ( print ("ERROR: La variabile <" ^ (stampaNomeVar n) ^ "> utilizzata nel metodo <" ^ (stampaNomeMetodo m) ^ "> della classe <" ^ (stampaNomeClasse cla) ^ "> non è stata inizializzata.\n\n"); codiceT [] )			
			| VarNotInitializedInClasse( n, cla ) => ( print ("ERROR: La variabile <" ^ (stampaNomeVar n) ^ "> utilizzata durante l'inizializzazione dei campi nella classe <" ^ (stampaNomeClasse cla) ^ "> non è stata inizializzata.\n\n"); codiceT [] )

			| FieldNotFound (x, cla) => ( print ("ERROR: Il campo <" ^ (stampaNomeCampo x) ^ "> non è stato trovato nella gerarchia della classe <" ^ (stampaNomeClasse cla) ^ ">.\n\n"); codiceT [] )
			| MethodNotFound (x, cla) => ( print ("ERROR: Il metodo <" ^ (stampaNomeMetodo x) ^ "> compatibile con gli argomenti passati non è stato trovato nella gerarchia della classe <" ^ (stampaNomeClasse cla) ^ ">.\n\n"); codiceT [] )			
			| ClassNotFound (x, x2) => ( print ("ERROR: La classe <" ^ (stampaNomeClasse x) ^ ">  estende la classe non definita <" ^ (stampaNomeClasse x) ^ ">.\n\n"); codiceT [] )			
			| ReturnNotFound (x, cla) => ( print ("ERROR: Il metodo <" ^ (stampaNomeMetodo x) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> non contiene un comando di return.\n\n"); codiceT [] )

			| TypeIsNotAClassInMetodo(m, cla) => ( print ("ERROR: L'espressione utilizzata nel metodo <" ^ (stampaNomeMetodo m) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  "> non risolve in un oggetto valido.\n\n"); codiceT [] ) 
			| TypeIsNotAClassInClasse( cla) => ( print ("ERROR: L'espressione utilizzata durante l'inizializzazione dei campi nella classe <" ^ (stampaNomeClasse cla) ^  "> non risolve in un oggetto valido.\n\n"); codiceT [] ) 
			
			| TypeIsNotAClassCampo ( ts, c, cla ) => ( print ("ERROR: Il tipo <" ^ (stampaNomeTipoS ts) ^ "> del campo <" ^ (stampaNomeCampo c) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^  "> non rappresenta un tipo valido.\n\n"); codiceT [] )
			| TypeIsNotAClassMetodo ( ts, m, cla ) => ( print ("ERROR: Il tipo di ritorno <" ^ (stampaNomeTipoS ts) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^  "> non rappresenta un tipo valido.\n\n"); codiceT [] )
			| TypeIsNotAClassArgs ( ts, v, m, cla ) => ( print ("ERROR: Il tipo <" ^ (stampaNomeTipoS ts) ^ "> del parametro <" ^ (stampaNomeVar v) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^  "> non rappresenta un tipo valido.\n\n"); codiceT [] )
			| TypeIsNotAClassLocals ( ts, v, m, cla ) => ( print ("ERROR: Il tipo <" ^ (stampaNomeTipoS ts) ^ "> della variabile <" ^ (stampaNomeVar v) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^  "> non rappresenta un tipo valido.\n\n"); codiceT [] )
			
			| TypeIsNotAClassNewInMetodo ( x, cla, m ) => ( print ("ERROR: Nel metodo <" ^ (stampaNomeMetodo m) ^ "> della classe <" ^ (stampaNomeClasse cla) ^ "> viene effettuata un operazione di new(<" ^ (stampaNomeClasse x) ^  ">), ma <" ^ (stampaNomeClasse x) ^  "> non è un nome di classe definito.\n\n"); codiceT [] )
			| TypeIsNotAClassNewInClasse ( x, cla ) => ( print ("ERROR: Durante l'inizializzazione dei campi nella classe <" ^ (stampaNomeClasse cla) ^ "> viene effettuata un operazione di new(<" ^ (stampaNomeClasse x) ^  ">), ma <" ^ (stampaNomeClasse x) ^  "> non è un nome di classe definito.\n\n"); codiceT [] )

			| TypeErrorDefField ( ts, n, e , cla) => ( print ("ERROR TYPE MISMATCH: Impossibile inizializzare il campo <" ^ (stampaNomeCampo n) ^ "> di tipo <" ^ (stampaNomeTipoS ts) ^ "> con un espressione di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e )) ^"> nella classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); codiceT [] )
			| TypeErrorReturn ( n, ts, e, cla) => ( print ("ERROR TYPE MISMATCH: Impossibile tornare un espressione di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e )) ^ "> se il tipo di ritorno definito è <" ^ (stampaNomeTipoS( ts )) ^"> nel metodo <" ^ (stampaNomeMetodo n) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); codiceT [] )
			| TypeErrorAssignVar (n, e1, e2, cla, v) =>	( print ("ERROR TYPE MISMATCH: Impossibile assegnare alla variabile <" ^ (stampaNomeVar v) ^ "> di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e1 ))^ "> un espressione di tipo <"^ (stampaNomeTipoT( estraiTipoSemantico e2 )) ^"> nel metodo <" ^ (stampaNomeMetodo n) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); codiceT [] )			
			| TypeErrorAssignField (n, e1, e2, e3, cla, c) => ( print ("ERROR TYPE MISMATCH: Impossibile assegnare al campo <" ^ (stampaNomeCampo c) ^ "> di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e2 ))^ "> un espressione di tipo <"^ (stampaNomeTipoT( estraiTipoSemantico e3 )) ^"> nel metodo <" ^ (stampaNomeMetodo n) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); codiceT [] )
			| TypeErrorOverrideMismatch ( n, supert, superc, baset, basec ) => ( print ("ERROR TYPE MISMATCH: Il metodo <" ^ (stampaNomeMetodo n) ^ "> nella classe <" ^ (stampaNomeClasse basec) ^ "> effettua un override del metodo definito nella classe <" ^ (stampaNomeClasse superc) ^  "> cambiando il tipo di ritorno da <" ^ (stampaNomeTipoS supert) ^ "> al tipo incompatibile <" ^ (stampaNomeTipoS baset) ^ ">.\n\n"); codiceT [] )

			| MultipleClasseDef ( cla ) => ( print ("ERROR: La classe <" ^ (stampaNomeClasse cla) ^ "> è definita più volte.\n\n"); codiceT [] )
			| MultipleCampoDef ( n, cla ) => ( print ("ERROR: Il campo <" ^ (stampaNomeCampo n) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> è definito più volte.\n\n"); codiceT [] )
			| MultipleMetodoDef ( n, cla ) => ( print ("ERROR: Il metodo <" ^ (stampaNomeMetodo n) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> è definito più volte.\n\n"); codiceT [] )
			| MultipleArgsDef ( n, cla, m ) => ( print ("ERROR: Il parametro <" ^ (stampaNomeVar n) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> è definito più volte.\n\n"); codiceT [] )
			| MultipleLocalsDef ( n, cla, m ) => ( print ("ERROR: La variabile <" ^ (stampaNomeVar n) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> è definita più volte.\n\n"); codiceT [] )
			| MultipleLocalsArgsDef ( n, cla, m ) => ( print ("ERROR: La variabile <" ^ (stampaNomeVar n) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ "> è già definita come parametro.\n\n"); codiceT [] )
	end;	
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(*
programmaStatDin 
programmaWeird 
programmaOverride0 
programmaOverride1 
programmaOverride2 
programmaOverride3 
programmaOverride4 
programmaOverride5 
programmaOverride6 
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
programmaTEST 
*)

print( stampaProgrammaS( programmaTEST));
print( stampaProgrammaT( programmaStoT( programmaTEST)));

