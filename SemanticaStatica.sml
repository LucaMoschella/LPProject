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

fun listVarToTipoT ( l ) = funList(l, fn defVarS(t,n) => tipoSintToSem t )

(*
fun listVarToTipoT ( [] ) = []
	| listVarToTipoT ( (defVarS(t,n))::l ) = 
		(tipoSintToSem t) :: (listVarToTipoT ( l ));
*)
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COSTRUZIONE MAPPE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun buildVarsMap( l ) = putAllFun( buildData [], l, fn defVarS(t, n) => (n, defVarS(t, n)) );
fun buildCampiMap( l ) = putAllFun( buildData [], l, fn defCampoS(t,n,s) => (n, defCampoS(t,n,s)));
fun buildComandiMap( l ) = putAllFun( buildData [], l, fn assegnamentoVarS(n, e) => ("assegnamentoVar", assegnamentoVarS(n, e) )
														| assegnamentoCampoS(e1, n, e2) => ("assegnamentoCampo", assegnamentoCampoS(e1, n, e2))
														| returnS(e) => ("return", returnS(e)));
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



(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONTROLLO OVERRIDE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun controlloOverride (programMap, classebase, _, Object ) = true
	| controlloOverride (programMap, classebase, defMetodoS (baseype, n, args, l, c), nomeclasse ) = 
		let 
			val defmetodobase = defMetodoS (baseype, n, args, l, c)
			val defclasse = get(programMap, nomeclasse)
			val metodiMap = buildMetodiMap( (fn defClasseS(_, _, _, metodi) => metodi) defclasse )
			val metodoKey = ( n, listVarToTipoT args)
		in
			if not (containsKey( metodiMap, metodoKey ) ) then controlloOverride(programMap, classebase, defmetodobase, getExtendedClass defclasse)
			else
				let 
					val supertype = (fn defMetodoS(t, _, _, _,_) => t) (get( metodiMap , metodoKey ))
				in
					if compatibleTipoSintSint( programMap, supertype, baseype)
					then controlloOverride( programMap, classebase, defmetodobase, getExtendedClass defclasse ) 
					else raise TypeErrorOverrideMismatch( n, supertype, nomeclasse, baseype, classebase )
				end
		end;
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)



(* %%%%%%%%%%%%%%%%% REGOLE PER LA TRADUZIONE IN PROGRAMMA TIPATO %%%%%%%%%%%%%%%%%%%%% *)
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

and comandoStoT( programMap, cont, assegnamentoVarS( nomevar, v), defMetodoS( _, nomemetodo, _, _, _ ), nomeclasse) = 
		let 
			val left = espressioneStoT( programMap, cont, varExprS nomevar  )
			val right = espressioneStoT( programMap, cont,  v  )
		in
			if( compatibleTipoSemSem( programMap, estraiTipoSemantico left, estraiTipoSemantico right) )
			then assegnamentoVarT( nomevar, right )
			else raise TypeErrorAssignVar(nomemetodo, left, right, nomeclasse, nomevar)
		end

	| comandoStoT( programMap, cont, assegnamentoCampoS( right1, nomecampo, right2), defMetodoS( _, nomemetodo, _, _, _ ), nomeclasse) = 
		let 
			val left = espressioneStoT( programMap, cont, right1 )
			val field = espressioneStoT( programMap, cont,  accessoCampoS( right1, nomecampo)  )
			val right = espressioneStoT( programMap, cont,  right2  )
		in
			if( compatibleTipoSemSem( programMap, estraiTipoSemantico field, estraiTipoSemantico right ))
			then assegnamentoCampoT(left, nomecampo, right)
			else raise TypeErrorAssignField(nomemetodo, left, field, right, nomeclasse, nomecampo)
		end

	| comandoStoT( programMap, cont, returnS d, defMetodoS( t, nomemetodo, _, _, _ ), nomeclasse) = 
		let 
			val right = espressioneStoT(  programMap, cont,  d  )
		in
			if( compatibleTipoSintSem( programMap,  t, estraiTipoSemantico right ))
			then returnT(right)
			else raise TypeErrorReturn(nomemetodo, t, right, nomeclasse)
		end


and metodoStoT( programMap, cont, nomeclasse, defMetodoS( t, n, args, locals, comandi ) ) = 
				let 
					val argsMap = buildVarsMap args
					val localsMap = buildVarsMap locals
					val cmdsMap = buildComandiMap comandi
					val contestExpanded = putAllFun(cont, args @ locals, fn defVarS(t, v) => (varNome v, tipoSintToSem t))
					val defmetodo = defMetodoS( t, n, args, locals, comandi )
				in				 
					(if containsDuplicate (argsMap) then raise MultipleArgsDef( getKeyDuplicated argsMap, nomeclasse, n)
					else if containsDuplicate (localsMap) then raise MultipleLocalsDef( getKeyDuplicated localsMap, nomeclasse, n)
					else if not (containsKey (cmdsMap, "return")) then raise ReturnNotFound(n, nomeclasse)
					else
						defMetodoT	( t, n, funList(args, fn defVarS( t, n) => defVarT(t,n,tipoSintToSem t)), 
											funList(locals, fn defVarS( t, n) => defVarT(t,n,tipoSintToSem t)),
											funList(comandi, fn cmd => comandoStoT(	programMap, contestExpanded, cmd, defmetodo, nomeclasse))
									)	
					)					
				end

and classeStoT(programMap, defClasseS(nomeClasseCorrente, nomeClasseEstesa, campi, metodi)) =
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
											if not (compatibleTipoSintSem(programMap, t, estraiTipoSemantico right )) then raise TypeErrorDefField(t, n, right, nomeClasseCorrente)
											else defCampoT(t, n, right, tipoSintToSem t) 
										end
									),
							funList(metodi, fn m => (	controlloOverride( programMap, nomeClasseCorrente, m, nomeClasseCorrente );
														metodoStoT( programMap, contex, nomeClasseCorrente, m))
													)
						)						
		end

and programmaStoT( programma ) = 
	let 
		val programMap = buildClassiMap programma
	in
		(if containsDuplicate programMap then raise MultipleClasseDef( getKeyDuplicated programMap )
		else codiceT ( funList( (fn codiceS x => x) programma, fn c => classeStoT(programMap, c))) )
		
		handle ClassExtNotValid x => ( print ("ERROR: La classe <" ^ (stampaNomeClasse x) ^ "> non non può estendere sé stessa.\n\n"); codiceT [] )

			| UnknownVar x => ( print ("ERROR: La variabile <" ^ (stampaNomeVarPiu x) ^ "> non è stata definita.\n\n"); codiceT [] )

			| FieldNotFound x => ( print ("ERROR: Il campo <" ^ (stampaNomeCampo x) ^ "> non è stato trovato.\n\n"); codiceT [] )
			
			| MethodNotFound x => ( print ("ERROR: Il metodo <" ^ (stampaNomeMetodo x) ^ "> compatibile con gli argomenti passati non è stato trovato nella gerarchia.\n\n"); codiceT [] )
			
			| ClassNotFound x => ( print ("ERROR: La classe <" ^ (stampaNomeClasse x) ^ "> non è stata trovata.\n\n"); codiceT [] )
			
			| ReturnNotFound (x, cla) => ( print ("ERROR: Il metodo <" ^ (stampaNomeMetodo x) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ 
					"> non contiene un comando di return.\n\n"); codiceT [] )

			| TypeIsNotAClass => ( print ("ERROR: Impossibile convertire l'espressione in una classe.\n\n"); codiceT [] )

			| TypeErrorDefField ( ts, n, e , cla) => 
				( print ("ERROR TYPE MISMATCH: Impossibile inizializzare il campo <" ^ (stampaNomeCampo n) ^ "> di tipo <" ^ (stampaNomeTipoS ts) ^ "> con un espressione di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e )) ^
					 ">,\n                     nella classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); codiceT [] )

			| TypeErrorReturn ( n, ts, e, cla) => 
				( print ("ERROR TYPE MISMATCH: Impossibile tornare un espressione di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e )) ^ "> se il tipo di ritorno definito è <" ^ (stampaNomeTipoS( ts )) ^
					 ">,\n                     nel metodo <" ^ (stampaNomeMetodo n) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); codiceT [] )
			
			| TypeErrorAssignVar (n, e1, e2, cla, v) => 
				( print ("ERROR TYPE MISMATCH: Impossibile assegnare alla variabile <" ^ (stampaNomeVar v) ^ "> di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e1 ))^ "> un espressione di tipo <"^ (stampaNomeTipoT( estraiTipoSemantico e2 )) ^
					 ">,\n                     nel metodo <" ^ (stampaNomeMetodo n) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); codiceT [] )
			
			| TypeErrorAssignField (n, e1, e2, e3, cla, c) => 
				( print ("ERROR TYPE MISMATCH: Impossibile assegnare al campo <" ^ (stampaNomeCampo c) ^ "> di tipo <" ^ (stampaNomeTipoT( estraiTipoSemantico e2 ))^ "> un espressione di tipo <"^ (stampaNomeTipoT( estraiTipoSemantico e3 )) ^
					 ">,\n                     nel metodo <" ^ (stampaNomeMetodo n) ^ "> della classe <" ^ (stampaNomeClasse cla) ^  ">.\n\n"); codiceT [] )

			| TypeErrorOverrideMismatch ( n, supert, superc, baset, basec ) =>
				( print ("ERROR TYPE MISMATCH: Il metodo <" ^ (stampaNomeMetodo n) ^ "> nella classe <" ^ (stampaNomeClasse basec) ^ "> effettua un override del metodo definito nella classe <" ^ (stampaNomeClasse superc) ^  
					 ">,\n                     cambiando il tipo di ritorno da <" ^ (stampaNomeTipoS supert) ^ "> a <" ^ (stampaNomeTipoS baset) ^ "> (incompatibili).\n\n"); codiceT [] )

			| MultipleMetodoDef ( n, cla ) =>
				( print ("ERROR: Il metodo <" ^ (stampaNomeMetodo n) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ 
					"> è definito più volte.\n\n"); codiceT [] )

			| MultipleCampoDef ( n, cla ) =>
				( print ("ERROR: Il campo <" ^ (stampaNomeCampo n) ^ "> nella classe <" ^ (stampaNomeClasse cla) ^ 
					"> è definito più volte.\n\n"); codiceT [] )

			| MultipleArgsDef ( n, cla, m ) =>
				( print ("ERROR: Il parametro <" ^ (stampaNomeVar n) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" 
					^ (stampaNomeClasse cla) ^ "> è definito più volte.\n\n"); codiceT [] )

			| MultipleLocalsDef ( n, cla, m ) =>
				( print ("ERROR: La variabile <" ^ (stampaNomeVar n) ^ "> del metodo <" ^ (stampaNomeMetodo m) ^ "> nella classe <" 
					^ (stampaNomeClasse cla) ^ "> è definita più volte.\n\n"); codiceT [] )
			
			| MultipleClasseDef ( cla ) =>
				( print ("ERROR: La classe <" ^ (stampaNomeClasse cla) ^ "> è definita più volte.\n\n"); codiceT [] )
	end;	

use "ProgrammiEsempio.sml";

print( stampaProgrammaS( programmaOverride6));
print( stampaProgrammaT( programmaStoT( programmaOverride6)));

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





(********** controllo override OK**********) 
(*
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
*)

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
(*
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

*)


(*
and metodoStoT( programMap, cont, nomeclasse, defMetodoS( t, n, args, locals, comandi ) ) = 
				let 
					val argsMap = buildVarsMap args
					val localsMap = buildVarsMap locals
				in
					if not ( controlloOverride( programMap, defMetodoS( t, n, args, locals, comandi ), nomeclasse ) ) then raise TypeErrorOverrideMismatch( n, t, nomeclasse )
					else if containsDuplicate (argsMap) then raise MultipleArgsDef( getKeyDuplicated argsMap, nomeclasse, n)
					else if containsDuplicate (localsMap) then raise MultipleLocalsDef( getKeyDuplicated localsMap, nomeclasse, n)
					else
					if  ( (controlloMetodiDoppi( get(programMap, nomeclasse) , n, args ))) 
					then
						defMetodoT( programMap, 
											putAllFun(cont, args @ locals, fn defVarS(t, v) => (varNome v, tipoSintToSem t)),
											defMetodoS( t, n, args, locals, comandi ),
											defMetodoT( t, n, 	funList(args, fn defVarS( t, n) => defVarT(t,n,tipoSintToSem t)), 
																funList(locals, fn defVarS( t, n) => defVarT(t,n,tipoSintToSem t)), []),
											false
										)
				end
					else
						raise MultipleMetodoDef( n, nomeclasse )
*)	
(*
and metodoListStoT( programMap, cont, nomeclasse, l) = funList(l, fn m => metodoStoT( programMap, cont, nomeclasse, m));
*)



	
(*
fun programmaStoTApp(programMap, codiceS [] ) = codiceT []
	| programmaStoTApp(programMap, codiceS (c::l) ) = 
		let 
			val codiceT(x) = programmaStoTApp(programMap, codiceS l)
		in
			codiceT( classeStoT(programMap,c) :: x)
		end;
*)


