use "SemanticaStatica.sml";


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OPERAZIONI CON TIPI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun tipoDefault( intT ) = intV 0
	| tipoDefault( classeT n ) = nullV
	| tipoDefault( _ ) = raise RuntimeError;

fun getExtendedClassT( defClasseT (_, nomeclasseestesa, _, _) ) = nomeclasseestesa;

(* c1 è più in alto nella gerarchia di c2 *)
fun isSottoClasseT( programMap, Object, Object) = true
 |	isSottoClasseT( programMap, Object, nomeCl c2) = true
 |	isSottoClasseT( programMap, nomeCl c1, Object) = false
 |  isSottoClasseT( programMap, nomeCl c1, nomeCl c2) = if (c1 = c2) then true 
 	else isSottoClasseT(programMap, nomeCl c1,  getExtendedClassT(get(programMap,nomeCl c2)));

 (* il secondo è compatibile con il primo*)
fun   compatibleTipoSemSemT (programMap, T, T) = false
	| compatibleTipoSemSemT (programMap, intT, T) = false
	| compatibleTipoSemSemT (programMap, classeT( nome ), T) = true
  	| compatibleTipoSemSemT (programMap, T, intT) = false
	| compatibleTipoSemSemT (programMap, intT, intT) = true
	| compatibleTipoSemSemT (programMap, classeT( nome ), intT) = false
  	| compatibleTipoSemSemT (programMap, T, classeT( nome )) = false
	| compatibleTipoSemSemT (programMap, intT, classeT( nome )) = false
	| compatibleTipoSemSemT (programMap, classeT( nome1 ), classeT( nome2 )) = isSottoClasseT(programMap, nome1, nome2);

fun compatibleTipiSemSemT(programMap, [], [] ) = true
	| compatibleTipiSemSemT(programMap, v::l, [] ) = false
	| compatibleTipiSemSemT(programMap, [], t::l ) = false
	| compatibleTipiSemSemT(programMap, t1::l1, t2::l2 ) = if( not (compatibleTipoSemSemT(programMap,t1,t2))) then false else compatibleTipiSemSemT(programMap,l1,l2 );
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ESTRAZIONE INFORMAZIONI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun	estraiOggetto( objV obj) = obj
	| estraiOggetto( _ ) = raise RuntimeError;

fun listVarTToTipoT( l ) = fList(l, fn defVarT(t, n, ts) => ts )
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* utili per sfruttare le funzioni già definite su dataList! *)
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COSTRUZIONE MAPPE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun buildVarTMap( l ) = tailPutAllFun( buildData [], l, fn defVarT(t, n, e) => (n, defVarT(t, n, e)) );
fun buildCampiTMap( l ) = tailPutAllFun( buildData [], l, fn defCampoT(t, n, s, e) => (n, defCampoT(t, n, s, e)));
fun buildMetodiTMap( l ) = tailPutAllFun( buildData [], l, fn defMetodoT(t, m,args,locals,cmds) => ( (m, listVarTToTipoT args), defMetodoT(t, m,args,locals,cmds)) );
fun buildClassiTMap( codiceT l ) = tailPutAllFun( buildData [(Object, defClasseT ( Object, Object, [], []))], l, fn defClasseT( c, ce, lv, lm) => (c, defClasseT( c, ce, lv, lm)) );
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OPERAZIONI CON OGGETTI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun getSuperClasseObj( programMap, istanza(c, (_)) ) = getExtendedClassT( get( programMap, c ) );
fun getSuperCampiObj( programMap, istanza ( n, l)) =  f3List(l, fn (nc, nf, lo) => if( n = nc) then [] else [(nc, nf, lo)] );
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RICERCA CAMPI ASSOCIATI ALLE CLASSI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun cTbody( programMap,  nomeclasse ) =  (fn defClasseT( nome , _ , campi, _ )  => campi ) (get( programMap, nomeclasse ));

fun buildClassCampiTMap( l, nomeclasse ) = tailPutAllFun( buildData [], l, fn defCampoT(t, n, s, e) => ((nomeclasse, n), defCampoT(t, n, s, e)));

fun buildAllClassCampiTMap(programMap, Object) = buildData []
	| buildAllClassCampiTMap(programMap, nomeclasse) = 
		concat( buildClassCampiTMap( cTbody( programMap,  nomeclasse ), nomeclasse ), buildAllClassCampiTMap(programMap, getExtendedClassT( get(programMap, nomeclasse) )) );
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CHIUSURA DI UN NOME METODO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun mTbody( programMap, nomeclasse ) =  (fn defClasseT( nome , _ , _ , metodi)  => metodi ) (get( programMap, nomeclasse ));

fun buildChiusuraMetodiTMap( l ) = tailPutAllFun( buildData [], l, fn defMetodoT(t, n, args, locals, cmds) => ((n, listVarTToTipoT args),(n, args, cmds)));

fun buildAllChiusuraMetodiTMap(programMap, Object) = buildData []
	| buildAllChiusuraMetodiTMap(programMap, nomeclasse) = concat( buildChiusuraMetodiTMap( mTbody( programMap,  nomeclasse ) ), buildAllChiusuraMetodiTMap(programMap, getExtendedClassT( get(programMap, nomeclasse) )) );

fun mbody( programMap, nomeclasse, nomemetodo, tipi) = 
	find( buildAllChiusuraMetodiTMap(programMap, nomeclasse), (nomemetodo, tipi), fn ((m1,t1), (m2,t2)) => (m1 = m2) andalso (compatibleTipiSemSemT(programMap, t1, t2))) ;
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(* %%%%%%%%%%%%%%%%% REGOLE PER L'ESECUZIONE DEL PROGRAMMA TIPATO %%%%%%%%%%%%%%%%%%%%% *)
(* Aggiunge i campi all'oggeto e all'heap *)
fun allocObjHeap([], istanza( obj, campi), heap) = ( istanza( obj,campi ), heap )
	| allocObjHeap( ( (nomeclasse, nomecampo), defCampoT( _, _, _, tipo ))::l, istanza( obj, campi), heap) = 
		let val x = nextLoc() in (allocObjHeap( l,  istanza( obj, campi @ [(nomeclasse, nomecampo, x)]), tailPut( heap, x, tipoDefault tipo))) end

and inizializzaObj( programMap, obj, campiMap, [] , heap) = heap
	| inizializzaObj( programMap, obj, campiMap, (classecampo, nomecampo, loccampo)::l, heap ) = 
		let 
			val (x, _) = valutaEspressione( programMap, buildEnv [(this, objV obj )], 
				(fn defCampoT( _, _, r, _ ) => r) ( get(campiMap, (classecampo, nomecampo) ) ), heap)
		in 
			inizializzaObj( programMap, obj, campiMap, l , set(heap, loccampo, x) )
		end

and valutaEspressione (programMap, env, varExprT(v, t), heap) = (get(env, varPiuNome v), heap)

	| valutaEspressione (programMap, env, intExprT (n, t), heap) = (intV(n), heap)

 	| valutaEspressione (programMap, env,  thisT (t) , heap) = (get(env, this), heap)

	| valutaEspressione (programMap, env,  superT (t) , heap) = 
		let 
			val x = estraiOggetto( get(env, this) ) 
		in 
			(objV ( istanza ( getSuperClasseObj(programMap, x), getSuperCampiObj(programMap, x))), heap)
		end

	| valutaEspressione (programMap, env,  nullT (t) , heap) = (nullV, heap)

	| valutaEspressione (programMap, env, newT (nomeclasse, t), heap) =  
		let 
			val campiMap = buildAllClassCampiTMap(programMap, nomeclasse)
			val campi = getList( campiMap )
			val (oggetto, newheap) = allocObjHeap(campi, istanza( nomeclasse, [] ), heap)
		in
			(objV oggetto, inizializzaObj( programMap, oggetto, campiMap, (fn istanza(x, y) => y) oggetto, newheap))
		end

	| valutaEspressione (programMap, env, accessoCampoT (e , c , t), heap) = 
		let 
			val classecampo = getNomeClasseDaTipoT(estraiTipoSemantico e)
			val (objV (istanza (n, campi)), newheap) = valutaEspressione(programMap, env, e, heap)

			val campiObjMap = tailPutAllFun(buildData[], campi, fn (x, y, z) => ((x, y), z))
			val loc = find(campiObjMap, (classecampo, c), fn  ((x,y),(classecampo,c)) => ((isSottoClasseT(programMap, x, classecampo) ) andalso ( c = y)))
		in
			(get(newheap, loc), newheap)
		end

	| valutaEspressione (programMap, env, chiamataMetodoT ( e, m, args, ts), heap)=
		let 
			val eclasse = getNomeClasseDaTipoT(estraiTipoSemantico e)
			val (objV (istanza (classeoggetto, campi)), newheap) = valutaEspressione(programMap, env, e, heap)


			val (valoriargs, heapAfterArgs) = f4List( args, valutaEspressione, programMap, env, newheap)

			(* ricerca a partire dalla variabile *)
			val ( meto, param, cmds ) = mbody( programMap, eclasse, m, fList( args, fn x => estraiTipoSemantico x ))

			(* ricerca un metodo in overrdide a partire dall'oggetto *)
			val ( finalmeto, finalparam, finalcmds ) =  get(buildAllChiusuraMetodiTMap(programMap, classeoggetto), (meto, listVarTToTipoT param))

			val newenv = headPutAll( env, f5List( finalparam, fn defVarT( t, p, ts) => varPiuNome p, valoriargs, fn x => x  ) ) 
		in
			valutaComandi(programMap, headPut(newenv, this, objV (istanza (classeoggetto, campi))), finalcmds, heapAfterArgs)
		end

and valutaComandi( programMap, env, assegnamentoVarT(v, e)::l ,heap) =
		let
			val (valore, newheap) = valutaEspressione(programMap, env, e, heap)
		in
			valutaComandi(programMap, headPut(env, varPiuNome v, valore), l , newheap)
		end


	| valutaComandi( programMap, env, assegnamentoCampoT(e1, c, e2)::l ,heap) =
		let
			val eclasse = getNomeClasseDaTipoT(estraiTipoSemantico e1)

			val (objV (istanza (n, campi)), h) = valutaEspressione(programMap, env, e1, heap)

			val (vr, finalheap) = valutaEspressione(programMap, env, e2, h)

			val campiObjMap = tailPutAllFun(buildData[], campi, fn (x, y, z) => ((x, y), z))

			val loc = find(campiObjMap, (eclasse, c), fn  ((x,y),(eclasse,c)) => (isSottoClasseT(programMap, x, eclasse) andalso ( c = y )))
		in
			valutaComandi(programMap, env, l, set( finalheap, loc, vr))
		end


	| valutaComandi( programMap, env, returnT e::l ,heap) = valutaEspressione(programMap, env, e, heap)

and valutaProgramma( codiceT classi ) =
	let
		val programMap = buildClassiTMap(codiceT classi)
		val (mainTipo, mainClasse) = cercaMain( classi )
		val (startObj, newheap) = valutaEspressione( programMap, buildEnv [], newT(mainClasse, classeT mainClasse), buildHeap [] )
	in
		( (valutaEspressione(
			programMap, 
			buildEnv [(this, startObj)], 
			chiamataMetodoT( thisT ( classeT mainClasse), nomeM "main" , [] , mainTipo),
			newheap)))
  
	end
	handle MissingMain=> (print "Non è stato trovato il metodo main(), il programma non verrà eseguito!\n"; (intV ~1, buildHeap []))

and cercaMain( [] )= raise MissingMain
	| cercaMain( defClasseT( n, ne, campi, metodi)::l )=
	let
		val metodiMap = buildMetodiTMap( metodi )
	in
		if( containsKey( metodiMap, ( nomeM "main", [] ))) 
		then (fn defMetodoT( t, _, _, _, _) => ( tipoSintToSem t, n) )(get(metodiMap, (nomeM "main", [])))
		else cercaMain( l ) handle KeyNotFound => raise MissingMain
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

print (stampaProgrammaS programmaStatDin2  );
val x = programmaStoT( programmaStatDin2 );
print (stampaProgrammaT x);
(*
val (x2, y) = valutaEspressione ( buildClassiTMap x, buildEnv [], newT( nomeCl "B", classeT (Object) ), buildHeap []);
print (stampaVal(x2) ^"\n" ^stampaHeap(y) ^ "\n\n");

mbody(buildClassiTMap x, nomeCl "esempio", nomeM "main", [] )
*)

val (i,u ) = valutaProgramma x;

print("\nIL PROGRAMMA HA VALUTATO: " ^ (stampaVal( i )) ^ " ! :)\n\n " ^ (stampaHeap u));