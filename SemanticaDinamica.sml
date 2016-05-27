use "SemanticaStatica.sml";
(* Usiamo alcune delle funzione definite in SemanticaStatica.sml *)

(* utili per sfruttare le funzioni già definite su dataList! *)
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COSTRUZIONE MAPPE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun buildVarTMap( l ) = putAllFun( buildData [], l, fn defVarT(t, n, e) => (n, defVarT(t, n, e)) );
fun buildCampiTMap( l ) = putAllFun( buildData [], l, fn defCampoT(t, n, s, e) => (n, defCampoT(t, n, s, e)));
fun buildMetodiTMap( l ) = putAllFun( buildData [], l, fn defMetodoT(t, m,args,locals,cmds) => ( (m, listVarTToTipoT args), defMetodoT(t, m,args,locals,cmds)) );
fun buildClassiTMap( codiceT l ) = putAllFun( buildData [(Object, defClasseT ( Object, Object, [], []))], l, fn defClasseT( c, ce, lv, lm) => (c, defClasseT( c, ce, lv, lm)) );
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OPERAZIONI CON TIPI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun tipoDefault( intT ) = intV 0
	| tipoDefault( classeT _ ) = nullV;
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ESTRAZIONE INFORMAZIONI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun	estraiOggetto( objV obj) = obj
	| estraiOggetto( _ ) = raise RuntimeErrorValIsNotObj;
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OPERAZIONI CON OGGETTI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
fun getSuperClasseObj( programMap, istanza(c, (_)) ) = getExtendedClass( get( programMap, c ) );
fun getSuperCampiObj( programMap, istanza ( n, l)) =  istanza ( n, f3List(l, fn (nc, nf, lo) => if( n = nc) then [] else [(nc, nf, lo)] ));
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

(*
fun getSuperCampi(programMap, istanza ( n, l))= getSuperCampiAppoggio(programMap, istanza ( n, l), istanza( n, []))

and getSuperCampiAppoggio(programMap, istanza (n1, []), istanza (n2, l))=l
	 | getSuperCampiAppoggio(programMap, istanza(n1, (nomec,nomef,lo)::l), istanza(n2,l2))= 
		 if (nomec=n1)
		 then getSuperCampiAppoggio(programMap,istanza(n1,l),istanza(n2,l2))
		 else getSuperCampiAppoggio(programMap, istanza(n1,l), istanza(n2,((nomec,nomef,lo)::l2)) );
*)




fun cbody( programMap,  nomec ) = let val ( defClasseS( _ , _ , campi, _ ) ) = cercaClasseInProgramma ( programMap, nomec ) 
								in campi end;

(* Prende un ( oggetto, classeSintattica ) => ( obj, buildHeap) dove obj = oggetto + campi in classeSintattica, con i valori nell'buildHeap *)
fun aggiornaObjAndHeap([], ncl, istanza(n, l2), heap) = (istanza(ncl,l2), heap)
	| aggiornaObjAndHeap( defCampoS( t, nc, r)::l1, ncl, istanza(n, l2), heap)= (	let val x = nextLoc() 
																						in aggiornaObjAndHeap( l1, ncl, istanza(n, (ncl,nc,x)::l2), buildHeap( (x, tipoDefault(t))::h)) 
																						end)
	
and alloc (programMap, obj, ncl) = aggiornaObjAndHeap( cbody(programMap, ncl), ncl, obj, buildHeap [])

and allocaOggetto( programMap, newT (Object,t), heap ) = ( istanza( Object, []) , heap)
	| allocaOggetto( programMap, newT ( c, t ), heap ) =
		(* Sale verso Object*)
			let val (x, y) = allocaOggetto (programMap, newT( getExtendedClass( cercaClasseInProgramma( programMap, c)), t), heap)
			in (* Riscende, e ad ogni passo in discesa: *)
				let  
					(* 1: espande l'oggetto con la classse corrente e inizializza i campi nell'buildHeap con i valori di default*)
					val (x1, y1) = alloc( programMap,  x, c) 
				in
					(* 2: Torna l'oggetto creato, espandendo l'buildHeap con i campi aggiunti *)
					( x1,  concatHeap(y, y1) )
				end
			end;

(* Inizializza la buildLoc nell'buildHeap conil corretto right value.  
QUI I CAMPI DEVONO ESSERE INZIIZALIZZATI RICHIAMANDO valutaEspressione, CON UN AMBIENTE IN CUI è PRESENTE (THIS, OBJ) *)
fun cercaInitCampoApp ( programMap, [], nomeC campoSintattico ) = raise RuntimeErrorInitCampoNonTrovato
	| cercaInitCampoApp ( programMap, (defCampoS( tipoca, nomeC nomeca, rightca))::campi , nomeC campoSintattico ) =
			if( nomeca = campoSintattico ) then rightca else cercaInitCampoApp(programMap, campi, nomeC campoSintattico )
and cercaInitCampo( programMap, nomec, nomecl) = cercaInitCampoApp(programMap, cbody ( programMap, nomecl ), nomec );

fun initCampiApp( programMap, obj, istanza( nomec, []),  heap  ) =  heap  

	| initCampiApp( programMap, obj, istanza( nomec, (classecampo, nomecampo, loccampo)::l), heap ) = 
		let val (x, _) = valutaEspressione( programMap, 
										buildEnv [(this, objV obj )],
										cercaInitCampo(programMap, nomecampo, classecampo), 
										heap)
		in
			initCampiApp( programMap, obj, istanza( nomec, l), changeHeap( heap , loccampo , x ))
		end
and initCampi( programMap, obj, mem ) = initCampiApp( programMap, obj, obj, mem)
(* fine emtodi per inziializzare i campi! *)


(* %%%%%%%%%%%%%%%%% REGOLE PER L'ESECUZIONE DEL PROGRAMMA TIPATO %%%%%%%%%%%%%%%%%%%%% *)
and valutaEspressione (programMap, env, varExprT(v, t), heap) = (get(env, varPiuNome v),heap)

	| valutaEspressione (programMap, env, intExprT (n, t), heap) = (intV(n), heap)

 	| valutaEspressione (programMap, env,  thisT (t) , heap) = (get(env, this), heap)

	| valutaEspressione (programMap, env,  superT (t) , heap) = 
		let 
			val x = estraiOggetto( get(env, this) ) 
		in 
			objV ( istanza ( getSuperClasseObj(programMap, x), getSuperCampiObj(programMap, x)), heap)
		end

	| valutaEspressione (programMap, env,  nullT (t) , heap) = (nullV, heap)

	| valutaEspressione (programMap, env, newT (nomeclasse, t), heap) =  
		(* Alloca l'oggetto*)
		let val (x, y) = allocaOggetto (programMap, newT nomeclasse, heap)
		in 
			(* 2: calcola il rightvalue di tutti i campi dell'oggetto e li assegna.  *)
			( objV x, initCampi( programMap, x, y ))
		end
	| valutaEspressione (programMap, env, _ , heap) =( intV 999, heap);  

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

use "ProgrammiEsempio.sml";

print (stampaProgrammaS esempioDispensa);
val x = programmaStoT( esempioDispensa );

print (let val (x, y) = valutaEspressione ( x, buildEnv [], newT( nomeCl "A"), buildHeap []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);
p

