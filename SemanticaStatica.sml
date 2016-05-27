use "Sintassi.sml";
use "Datatype.sml";
use "Exception.sml";
use "PrintToJava.sml";

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONVERSIONE E COMPATIBILITA FRA TIPI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun tipoSintToSem ( intS ) = intT
	| tipoSintToSem ( classeS c ) = classeT c;

fun tipoSemToSint ( intT ) = intS
	| tipoSemToSint ( classeT c ) = classeS c
	| tipoSemToSint( T ) = raise WrongSemToSint;

fun getExtendedClass( defClasseS (nomeclasse, nomeclasseestesa, campi, metodi) ) = nomeclasseestesa;

(* c1 è più in alto nella gerarchia di c2 *)
fun isSottoClasse( programMap, Object, Object) = true
 |	isSottoClasse( programMap, Object, nomeCl c2) = true
 |	isSottoClasse( programMap, nomeCl c1, Object) = false
 |  isSottoClasse( programMap, nomeCl c1, nomeCl c2) = if (c1 = c2) then true 
 	else  (isSottoClasse(programMap, nomeCl c1,  getExtendedClass(get(programMap,nomeCl c2))))
 	handle KeyNotFound => raise ClassNotFound(nomeCl c2);

(* il secondo è compatibile con il primo*)
fun   compatibleTipoSemSem (programMap, T, T) = false
	| compatibleTipoSemSem (programMap, intT, T) = false
	| compatibleTipoSemSem (programMap, classeT( nome ), T) = true
  	| compatibleTipoSemSem (programMap, T, intT) = false
	| compatibleTipoSemSem (programMap, intT, intT) = true
	| compatibleTipoSemSem (programMap, classeT( nome ), intT) = true
  	| compatibleTipoSemSem (programMap, T, classeT( nome )) = false
	| compatibleTipoSemSem (programMap, intT, classeT( nome )) = false
	| compatibleTipoSemSem (programMap, classeT( nome1 ), classeT( nome2 )) = isSottoClasse(programMap, nome1, nome2);

fun compatibleTipoSintSem (programMap, ts, tt ) = compatibleTipoSemSem(programMap, tipoSintToSem ts ,tt);

fun compatibleTipoSintSint( programMap, ts1, ts2) = compatibleTipoSemSem(programMap, tipoSintToSem ts1, tipoSintToSem ts2);

fun parametriCompatibili(programMap, [], [] ) = true
	| parametriCompatibili(programMap, v::l, [] ) = false
	| parametriCompatibili(programMap, [], t::l ) = false
	| parametriCompatibili(programMap, (defVarS(t1,n))::l1, t2::l2 ) = if( not (compatibleTipoSintSem(programMap,t1,t2))) then false else parametriCompatibili(programMap,l1,l2 );

(*
fun equalList ([],[])=true
	| equalList (_,[])=false
	| equalList ([],_)=false
	| equalList (n::l, n1::l1)=if(n=n1)then equalList(l,l1)else false;
*)
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
 	
fun getNomeVarDaExpT( varExprT( nomeV v, _)) = v
	| getNomeVarDaExpT( _ ) = raise ExpIsNotAVar;

fun listVarToTipoT ( [] ) = []
	| listVarToTipoT ( (defVarS(t,n))::l ) = 
		(tipoSintToSem t) :: (listVarToTipoT ( l ));
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COSTRUZIONE MAPPE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun buildVarsMap( l ) = putAllFun( buildData [], l, fn defVarS(t, n) => (n, defVarS(t, n)) );
fun buildCampiMap( l ) = putAllFun( buildData [], l, fn defCampoS(t,n,s) => (n, defCampoS(t,n,s)));
fun buildMetodiMap( l ) = putAllFun( buildData [], l, fn defMetodoS(t, m,args,locals,cmds) => ( (m, listVarToTipoT args), defMetodoS(t, m,args,locals,cmds)) );
fun buildClassiMap( codiceS l ) = putAllFun( buildData [(Object, defClasseS ( Object, Object, [], []))], l, fn defClasseS( c, ce, lv, lm) => (c, defClasseS( c, ce, lv, lm)) );
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

(* aggiungi al buildContesto una lista di variabili *)
(*
fun addVarsToContesto( cont, [] ) = cont 
	| addVarsToContesto( cont, defVarS( tipoSintattico, nomeV v)::l ) =
		addVarsToContesto( put(cont, varNome (nomeV v),  tipoSintToSem ( tipoSintattico )), l);
*)

(*
fun cercaClasseInProgramma ( programMap, Object ) = defClasseS(Object, Object, [], [])	
	| cercaClasseInProgramma ( codiceS [], nomeCl nclasse ) = raise ClassNotFound(nomeCl nclasse)
	| cercaClasseInProgramma ( codiceS (  (defClasseS (nomeCl c, ce, lv, lm)) ::l ), nomeCl nclasse ) = 
				if (nclasse = c) then defClasseS (nomeCl c, ce, lv, lm) else cercaClasseInProgramma( codiceS l, nomeCl nclasse)
	| cercaClasseInProgramma ( codiceS (  (defClasseS (Object, ce, lv, lm)) ::l ), nomeCl nclasse ) =  
				cercaClasseInProgramma( codiceS l, nomeCl nclasse);

fun esisteClasseInProgramma ( programMap, Object ) = true	
	| esisteClasseInProgramma ( codiceS [], nomeCl nclasse ) = false
	| esisteClasseInProgramma ( codiceS (  (defClasseS (nomeCl c, ce, lv, lm)) ::l ), nomeCl nclasse ) = 
				if (nclasse = c) then true else esisteClasseInProgramma( codiceS l, nomeCl nclasse)
	| esisteClasseInProgramma ( codiceS (  (defClasseS (Object, ce, lv, lm)) ::l ), nomeCl nclasse ) =  
				esisteClasseInProgramma( codiceS l, nomeCl nclasse);
*)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TIPO SEMANTICO DI UN NOME CAMPO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun cercaTipoCampoinGerarchiaClasse ( programMap, defClasseS (Object, _, _, _), nomeC campoSintattico ) = raise FieldNotFound(nomeC campoSintattico)
	| cercaTipoCampoinGerarchiaClasse (programMap,  defClasseS (nomeCl _, classeEstesa, [], _), nomeC campoSintattico ) = (cercaTipoCampoinGerarchiaClasse( programMap, get(programMap,classeEstesa ), nomeC campoSintattico)
																													handle KeyNotFound => raise ClassNotFound(classeEstesa))
	| cercaTipoCampoinGerarchiaClasse ( programMap, defClasseS (nomeCl classeSintattica, ext, (defCampoS( t, nomeC nc, r))::campi, metodi), nomeC campoSintattico ) =
			if( nc = campoSintattico ) then tipoSintToSem ( t ) else cercaTipoCampoinGerarchiaClasse(programMap, defClasseS (nomeCl classeSintattica, ext, campi, metodi), nomeC campoSintattico );

fun ftype( programMap, nomec, nomecl) = cercaTipoCampoinGerarchiaClasse(programMap, get ( programMap, nomecl ), nomec )
										handle KeyNotFound => raise ClassNotFound(nomecl);
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TIPO SEMANTICO DI UN NOME METODO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun cercaTipoMetodoInGerarchiaClasse(programMap, defClasseS (Object, _, _, _), nomeM metodoSintattico, parametri ) = raise MethodNotFound(nomeM metodoSintattico)

	| cercaTipoMetodoInGerarchiaClasse(programMap,  defClasseS (nomeCl _, classeEstesa, _, []), nomeM metodoSintattico, parametri ) = 
			(cercaTipoMetodoInGerarchiaClasse( programMap, get(programMap, classeEstesa ), nomeM metodoSintattico, parametri)
				handle KeyNotFound => raise ClassNotFound(classeEstesa))
	| cercaTipoMetodoInGerarchiaClasse ( programMap, defClasseS (nomeCl classeSintattica, ext, campi, (defMetodoS(t,nomeM m,args,locals,cmds))::metodi), nomeM metodoSintattico,parametri ) =
			if( (m = metodoSintattico) andalso (parametriCompatibili(programMap, args, parametri))) (* parametri deve contere tipi dal datatype types*)
				then tipoSintToSem ( t ) 
				else cercaTipoMetodoInGerarchiaClasse(programMap, defClasseS (nomeCl classeSintattica, ext, campi, metodi), nomeM metodoSintattico, parametri );


fun mtype( programMap, nomem, nomecl, tipi) = 
	cercaTipoMetodoInGerarchiaClasse(programMap, get ( programMap, nomecl ), nomem, tipi )
	handle KeyNotFound => raise ClassNotFound(nomecl);
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)



	



(* %%%%%%%%%%%%%%%%% REGOLE PER LA TRADUZIONE IN PROGRAMMA TIPATO %%%%%%%%%%%%%%%%%%%%% *)


(*
fun variabileListStoT( [] ) = []
	| variabileListStoT( (defVarS( t, n))::l) = 
	(defVarT(t,n,tipoSintToSem t))::(variabileListStoT(l))
*)

(*
fun campoListStoT( programMap, cont, l ) =
			funList( l, fn defCampoS( t, n, r) => 
							let 
								val right = espressioneStoT(programMap, cont, r)
							in
								if ( compatibleTipoSintSem(programMap, t, estraiTipoSemantico right )) 
								then 
									defCampoT(t, n, right, tipoSintToSem t) 
								else
									raise TypeErrorDefField(t, n, right)
							end
					)
*)
(*

and campoListStoT( programMap, cont, [] ) = []
	| campoListStoT( programMap, cont, ( defCampoS( t, nomeC n, r))::l ) = 
		let 
			val right = espressioneStoT(programMap, cont, r)
		in
			if ( compatibleTipoSintSem(programMap, t, estraiTipoSemantico right )) 
			then 
				defCampoT(t, nomeC n, right, tipoSintToSem t) :: (campoListStoT( programMap, cont, l ))
			else
				raise TypeErrorDefField(t, nomeC n, right)
		end 

and listExprToTipoT (programMap, cont, [] ) = []
	| listExprToTipoT ( programMap, cont,r::l ) = 
		(estraiTipoSemantico(espressioneStoT(programMap, cont, r))) :: (listExprToTipoT (  programMap, cont,l ))
*)
(*
and espressioneListStoT( programMap, cont, [] ) = []
	| espressioneListStoT( programMap, cont, a::l ) = 
		espressioneStoT(programMap, cont, a)::(espressioneListStoT( programMap, cont, l ))
*)
fun	espressioneStoT( programMap, cont, varExprS(nomeV v)  ) = 
		(varExprT(nomeV v, get( cont, varNome (nomeV v)))
			handle KeyNotFound => raise UnknownVar(varNome(nomeV v)))

	| espressioneStoT( programMap, cont, intExprS n) = 
		intExprT( n, intT)

	| espressioneStoT( programMap, cont, newS( c)) = 
		newT( c, classeT c)

	| espressioneStoT( programMap, cont, ( nullS )) = 
		nullT( T ) 

	| espressioneStoT( programMap, cont, ( thisS )) = 
		(thisT( get(cont, this))
			handle KeyNotFound => raise UnknownVar(this))

	| espressioneStoT( programMap, cont, ( superS )) = 
			let 
				val x = get(cont, this) handle KeyNotFound => raise UnknownVar(this)
				val y = getNomeClasseDaTipoT( x )
			in
				superT(classeT (getExtendedClass( get(programMap, y))))
				handle KeyNotFound => raise ClassNotFound(y)
			end 

	| espressioneStoT( programMap, cont, accessoCampoS( v, c) ) = 
		let val expTyped = espressioneStoT( programMap, cont, v)
		in
			accessoCampoT( expTyped, c, ftype(programMap, c, getNomeClasseDaTipoT( estraiTipoSemantico expTyped)))
		end
		
	| espressioneStoT( programMap, cont, chiamataMetodoS( v, m, args) ) = 
		let val expTyped = espressioneStoT( programMap, cont, v)
		in
			chiamataMetodoT(expTyped, 
							m ,
							funList(args, fn x => espressioneStoT(programMap, cont, x)),
							mtype(	programMap, 
									m, 
									getNomeClasseDaTipoT( estraiTipoSemantico(expTyped) ),
								(*	listExprToTipoT (programMap, cont, args ) *)
									funList(args, fn x => estraiTipoSemantico(espressioneStoT(programMap, cont, x)))
								)
				)
		end




(********** controllo override OK**********) 

and cercaMetodoInClasse(programMap, defClasseS (Object, _, _, _), nomeM metodoSintattico, parametri ) = raise MethodNotFound(nomeM metodoSintattico)

	| cercaMetodoInClasse(programMap,  defClasseS ( _, _, _, []), nomeM metodoSintattico, parametri ) = raise MethodNotFound(nomeM metodoSintattico)

	| cercaMetodoInClasse ( programMap, defClasseS (nomeCl classeSintattica, ext, campi, (defMetodoS(t, nomeM m, args, locals, cmds))::metodi),  
																									nomeM metodoSintattico, parametri ) =
			if( (m = metodoSintattico) andalso (listVarToTipoT(args) = parametri) ) 
				then 
					defMetodoS(t, nomeM m, args, locals, cmds)
				else 
					cercaMetodoInClasse(programMap, defClasseS (nomeCl classeSintattica, ext, campi, metodi), nomeM metodoSintattico, parametri )


and esisteMetodoInClasse(programMap, defClasseS (Object, _, _, _), nomeM metodoSintattico, parametri ) = false

	| esisteMetodoInClasse(programMap,  defClasseS ( _, _, _, []), nomeM metodoSintattico, parametri ) = false

	| esisteMetodoInClasse ( programMap, defClasseS (nomeCl classeSintattica, ext, campi, (defMetodoS(t, nomeM m, args, locals, cmds))::metodi),  
																									nomeM metodoSintattico, parametri ) =
			( (m = metodoSintattico) andalso (listVarToTipoT(args) = parametri)) 


and controlloOverride (programMap, _, Object ) = true

	| controlloOverride (programMap, defMetodoS (ts,  nomeM n, args, locals, commands), nomeclasse ) = 
		let
			val nomeclasseestesa = getExtendedClass(get(programMap, nomeclasse))
			val defclasseestesa = get(programMap, nomeclasseestesa)
			val listatipi = listVarToTipoT(args)
		in
			if (esisteMetodoInClasse(programMap, defclasseestesa,  nomeM n, listatipi) )
			then
				let 
					val (defMetodoS(ts2, _, _, _,_)) = cercaMetodoInClasse(programMap, defclasseestesa,  nomeM n, listatipi)
				in
					if ( compatibleTipoSintSint(programMap, ts2,ts) )
					then 
						controlloOverride( programMap,defMetodoS(ts, nomeM n,args,locals,commands), nomeclasseestesa) 
					else 
						false
				end
			else
				controlloOverride(programMap,defMetodoS(ts, nomeM n,args,locals,commands), nomeclasseestesa) 
		end

(********************************************************************)

(********** controllo doppione: true non ci osno doppioni **********)
(*
and controlloMetodiDoppiApp( defClasseS (Object, _, _, _), _, _, found ) = true

	|controlloMetodiDoppiApp( defClasseS (nomeCl classeSintattica, ext, campi, []), nomemetodo, tipiargs, found ) = true

	| controlloMetodiDoppiApp( defClasseS (nomeCl classeSintattica, ext, campi, (defMetodoS(t, m, args, locals, cmds))::metodi), nomemetodo, tipiargs, found ) =
		if( (m = nomemetodo) andalso (listVarToTipoT(args) = tipiargs))
		then
			if( found )
			then
				false
			else
				controlloMetodiDoppiApp( defClasseS (nomeCl classeSintattica, ext, campi, metodi), nomemetodo, tipiargs, true )

		else
			controlloMetodiDoppiApp( defClasseS (nomeCl classeSintattica, ext, campi, metodi), nomemetodo, tipiargs, found )


and controlloMetodiDoppi( definizioneclasse, nomemetodo, args) = controlloMetodiDoppiApp( definizioneclasse, nomemetodo, listVarToTipoT(args), false )
*)
(********************************************************************)
and metodoStoTApp( programMap, cont, defMetodoS(tipoSintattico, nomeM nomemetodo, args, locals, [] ), metodoT, ret) = 
		if( ret ) then metodoT else raise ReturnNotFound(nomeM nomemetodo)

	| metodoStoTApp( programMap, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, (assegnamentoVarS( nomevar, v))::comandi ), 
													defMetodoT(ti, no, ar, lo, cmds), ret) = 
		let 
			val left = espressioneStoT( programMap, cont, varExprS nomevar  )
			val right = espressioneStoT( programMap, cont,  v  )
		in
			if( compatibleTipoSemSem( programMap, estraiTipoSemantico left, estraiTipoSemantico right) )
			then  
				metodoStoTApp( programMap, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, comandi ), 
															defMetodoT(ti, no, ar, lo, cmds @ [assegnamentoVarT( nomevar, right )]), ret)
			else 
				raise TypeErrorAssignVar(nomeM nomemetodo, left, right)
		end

	| metodoStoTApp( programMap, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, (assegnamentoCampoS( right1, nomecampo, right2))::comandi ), 
													defMetodoT(ti, no, ar, lo, cmds), ret) = 
		let 
			val left = espressioneStoT( programMap, cont, right1 )
			val field = espressioneStoT( programMap, cont,  accessoCampoS( right1, nomecampo)  )
			val right = espressioneStoT( programMap, cont,  right2  )
		in
			if( compatibleTipoSemSem( programMap, estraiTipoSemantico field, estraiTipoSemantico right ))
			then 
				metodoStoTApp( programMap, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, comandi ), 
															defMetodoT(ti, no, ar, lo, cmds @ [assegnamentoCampoT(left, nomecampo, right)]), ret)
			else
				raise TypeErrorAssignField(nomeM nomemetodo, left, field, right )
		end

	| metodoStoTApp( programMap, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, (returnS d)::comandi ), 
													defMetodoT(ti, no, ar, lo, cmds), ret) = 
		let 
			val right = espressioneStoT(  programMap, cont,  d  )
		in
			if( compatibleTipoSintSem( programMap,  tipoSintattico, estraiTipoSemantico right ))
			then metodoStoTApp( programMap, cont, defMetodoS(tipoSintattico,nomeM nomemetodo, args, locals, comandi ), 
															defMetodoT(ti, no, ar, lo, cmds @ [returnT(right)]), true)
			else
				raise TypeErrorReturn(nomeM nomemetodo, tipoSintattico, right)
		end

and metodoStoT( programMap, cont, nomeclasse, defMetodoS( t, n, args, locals, comandi ) ) = 
				let 
					val argsMap = buildVarsMap args
					val localsMap = buildVarsMap locals
				in
					if not ( controlloOverride( programMap, defMetodoS( t, n, args, locals, comandi ), nomeclasse ) ) then raise OverrideMismatch( n, t, nomeclasse )
					else if containsDuplicate (argsMap) then raise MultipleArgsDef( getKeyDuplicated argsMap, nomeclasse, n)
					else if containsDuplicate (localsMap) then raise MultipleLocalsDef( getKeyDuplicated localsMap, nomeclasse, n)
					else
					(*if  ( (controlloMetodiDoppi( get(programMap, nomeclasse) , n, args ))) 
					then*)
						metodoStoTApp( programMap, 
											putAllFun(cont, args @ locals, fn defVarS(t, v) => (varNome v, tipoSintToSem t)),
											defMetodoS( t, n, args, locals, comandi ),
											defMetodoT( t, n, 	funList(args, fn defVarS( t, n) => defVarT(t,n,tipoSintToSem t)), 
																funList(locals, fn defVarS( t, n) => defVarT(t,n,tipoSintToSem t)), []),
											false
										)
				end
					(*else
						raise MultipleMetodoDef( n, nomeclasse )*)
			
(*
and metodoListStoT( programMap, cont, nomeclasse, l) = funList(l, fn m => metodoStoT( programMap, cont, nomeclasse, m));
*)


fun classeStoT(programMap, defClasseS(nomeClasseCorrente, nomeClasseEstesa, campi, metodi)) =
	if(nomeClasseCorrente = nomeClasseEstesa) then raise ClassExtNotValid( nomeClasseCorrente )
	else 
		let 
			val campiMap = buildCampiMap campi
			val metodiMap = buildMetodiMap metodi
			val contex = buildContesto[(this,classeT(nomeClasseCorrente))]
		in
			if containsDuplicate (campiMap) then raise MultipleCampoDef( getKeyDuplicated campiMap, nomeClasseCorrente)
			else if containsDuplicate (metodiMap) then raise MultipleMetodoDef( (fn (x,y) => x)(getKeyDuplicated metodiMap), nomeClasseCorrente)
			else if not (containsKey( programMap, nomeClasseEstesa)) then raise ClassNotFound(nomeClasseEstesa)
			else 
				defClasseT( nomeClasseCorrente, 
							nomeClasseEstesa, 
							funList( campi, fn defCampoS( t, n, r) => 
										let 
											val right = espressioneStoT(programMap, contex, r)
										in
											if not (compatibleTipoSintSem(programMap, t, estraiTipoSemantico right )) then raise TypeErrorDefField(t, n, right)
											else defCampoT(t, n, right, tipoSintToSem t) 
										end
									),
							funList(metodi, fn m => metodoStoT( programMap, contex, nomeClasseCorrente, m))
						)						
		end;
	

fun programmaStoTApp(programMap, codiceS [] ) = codiceT []
	| programmaStoTApp(programMap, codiceS (c::l) ) = 
		let 
			val codiceT(x) = programmaStoTApp(programMap, codiceS l)
		in
			codiceT( classeStoT(programMap,c) :: x)
		end;


fun programmaStoT( programma ) = 
	programmaStoTApp(buildClassiMap programma, programma)
	handle    VarNameNotValid x => ( print ("ERRORE: Il nome <" ^ (stampaNomeVar x) ^ "> non è un nome di variabile è valido.\n\n"); codiceT [] )
			| ClassExtNotValid x => ( print ("ERRORE: La classe <" ^ (stampaNomeClasse x) ^ "> non non può estendere sé stessa.\n\n"); codiceT [] )

			| UnknownVar x => ( print ("ERRORE: La variabile <" ^ (stampaNomeVarPiu x) ^ "> non è stata definita.\n\n"); codiceT [] )

			| FieldNotFound x => ( print ("ERRORE: Il campo <" ^ (stampaNomeCampo x) ^ "> non è stato trovato.\n\n"); codiceT [] )
			| MethodNotFound x => ( print ("ERRORE: Il metodo <" ^ (stampaNomeMetodo x) ^ ">, compatibile con gli argomenti, non è stato trovato.\n\n"); codiceT [] )
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
					"> effettua un Override cambiando il tipo di ritorno in <" ^ (stampaNomeTipoS ts) ^ ">, non compatibile con quello definito.\n\n"); codiceT [] )
			
			| MultipleMetodoDef ( n, cla ) =>
				( print ("ERRORE: Il metodo <" ^ (stampaNomeMetodo n) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ 
					"> è definito più volte.\n\n"); codiceT [] )

			| MultipleCampoDef ( n, cla ) =>
				( print ("ERRORE: Il campo <" ^ (stampaNomeCampo n) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ 
					"> è definito più volte.\n\n"); codiceT [] )

			| MultipleArgsDef ( n, cla, m ) =>
				( print ("ERRORE: Il parametro <" ^ (stampaNomeVar n) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" 
					^ (stampaNomeClasse cla) ^ "> è definito più volte.\n\n"); codiceT [] )

			| MultipleLocalsDef ( n, cla, m ) =>
				( print ("ERRORE: La variabile <" ^ (stampaNomeVar n) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" 
					^ (stampaNomeClasse cla) ^ "> è definita più volte.\n\n"); codiceT [] );

use "ProgrammiEsempio.sml";

print( stampaProgrammaS( programmaWeird));
print( stampaProgrammaT( programmaStoT( programmaWeird)));
