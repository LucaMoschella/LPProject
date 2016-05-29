use "SemanticaStatica.sml";


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OPERAZIONI CON TIPI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun tipoDefault( intT ) = intV 0
	| tipoDefault( classeT n ) = nullV
	| tipoDefault( _ ) = raise RuntimeError;

fun getExtendedClassT( defClasseT (_, nomeclasseestesa, _, _) ) = nomeclasseestesa;
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ESTRAZIONE INFORMAZIONI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun	estraiOggetto( objV obj) = obj
	| estraiOggetto( _ ) = raise RuntimeErrorValIsNotObj;

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



(* %%%%%%%%%%%%%%%%% REGOLE PER L'ESECUZIONE DEL PROGRAMMA TIPATO %%%%%%%%%%%%%%%%%%%%% *)
(* Aggiunge i campi all'oggeto e all'heap *)
fun allocObjHeap([], istanza( obj, campi), heap) = ( istanza( obj,campi ), heap )
	| allocObjHeap( ( (nomeclasse, nomecampo), defCampoT( _, _, _, tipo ))::l, istanza( obj, campi), heap) = 
		let val x = nextLoc() in (allocObjHeap( l,  istanza( obj, (nomeclasse, nomecampo, x)::campi), tailPut( heap, x, tipoDefault tipo))) end

and inizializzaObj( programMap, obj, campiMap, [] , heap) = heap
	| inizializzaObj( programMap, obj, campiMap, (classecampo, nomecampo, loccampo)::l, heap ) = 
		let 
			val (x, _) = valutaEspressione( programMap, buildEnv [(this, objV obj )], 
				(fn defCampoT( _, _, r, _ ) => r) ( get(campiMap, (classecampo, nomecampo) ) ), heap)
		in 
			inizializzaObj( programMap, obj, campiMap, l , set(heap, loccampo, x) )
		end

and valutaEspressione (programMap, env, varExprT(v, t), heap) = (get(env, varPiuNome v),heap)

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
	| valutaEspressione (programMap, env, _ , heap) =( intV 999, heap);  

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


print (stampaProgrammaS programmaTEST);
val x = programmaStoT( programmaTEST );

val (x, y) = valutaEspressione ( buildClassiTMap x, buildEnv [], newT( nomeCl "B", classeT (Object) ), buildHeap []);
print (stampaVal(x) ^"\n" ^stampaHeap(y));