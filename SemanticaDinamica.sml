	use "SemanticaStatica.sml";


(* GESTIONE AMBIENTE *)
fun getValEnv( buildEnv [], var:varPiu ) = raise RuntimeErrorVarNotFoundInEnv
	| getValEnv( buildEnv ((k,v)::l), var:varPiu) = if (k = var)then (v) else (getValEnv(buildEnv l,var)); 

(* GESTIONE HEAP *)
fun concatHeap( buildHeap h1, buildHeap h2) = buildHeap ( h1 @ h2) ;

fun changeHeapApp( buildHeap [], buildHeap newHeap, key, value  ) = buildHeap newHeap
	| changeHeapApp( buildHeap ((keyh,v)::l), buildHeap newHeap, key, value  ) = 
		if( keyh = key) 
		then changeHeapApp(  buildHeap l, buildHeap ((keyh, value)::newHeap), key, value) 
		else changeHeapApp(  buildHeap l, buildHeap ((keyh, v)::newHeap), key, value)
and changeHeap( h, key, value  ) = changeHeapApp(h, buildHeap [], key, value);

fun getValHeap( buildHeap [], lo ) = raise RuntimeErrorLocNotFoundInHeap
	| getValHeap( buildHeap ((k,v)::l), lo) = if (k = lo)then (v) else (getValHeap(buildHeap l,lo)); 

(* FUNZIONI DI COMODO *)
fun getSuperClasseOggetto(programmaTipato, istanza(c,(_)))=getExtendedClass(cercaClasseInProgramma ( programmaTipato, c ));

fun getSuperCampi(programmaTipato, istanza ( n, l))= getSuperCampiAppoggio(programmaTipato,istanza ( n, l),istanza ( n, []))
and
	 getSuperCampiAppoggio(programmaTipato, istanza (n1, []), istanza (n2, l))=l

	 | getSuperCampiAppoggio(programmaTipato, istanza(n1, (nomec,nomef,lo)::l), istanza(n2,l2))= 
	 if (nomec=n1)
	 then getSuperCampiAppoggio(programmaTipato,istanza(n1,l),istanza(n2,l2))
	 else getSuperCampiAppoggio(programmaTipato, istanza(n1,l), istanza(n2,((nomec,nomef,lo)::l2)) )
and
	getObjFromVal( objV obj) = obj
	| getObjFromVal( _ ) = raise RuntimeErrorValIsNotObj;

fun tipoDefault( intS ) = intV 0
	| tipoDefault( classeS _ ) =nullV ;

fun cbody( programmaTipato,  nomec ) = let val ( defClasseS( _ , _ , campi, _ ) ) = cercaClasseInProgramma ( programmaTipato, nomec ) 
								in campi end;

(* Prende un ( oggetto, classeSintattica ) => ( obj, buildHeap) dove obj = oggetto + campi in classeSintattica, con i valori nell'buildHeap *)
fun aggiornaObjAndHeap([], ncl, istanza(n, l2), buildHeap h) = (istanza(ncl,l2), buildHeap h)
	| aggiornaObjAndHeap( defCampoS( t, nc, r)::l1, ncl, istanza(n, l2), buildHeap h)= (	let val x = nextLoc() 
																						in aggiornaObjAndHeap( l1, ncl, istanza(n, (ncl,nc,x)::l2), buildHeap( (x, tipoDefault(t))::h)) 
																						end)
	
and alloc (programmaTipato, obj, ncl) = aggiornaObjAndHeap( cbody(programmaTipato, ncl), ncl, obj, buildHeap [])

and allocaOggetto( programmaTipato, newT (Object,t), buildHeap h ) = ( istanza( Object, []) , buildHeap h)
	| allocaOggetto( programmaTipato, newT ( c, t ), buildHeap h ) =
		(* Sale verso Object*)
			let val (x, y) = allocaOggetto (programmaTipato, newT( getExtendedClass( cercaClasseInProgramma( programmaTipato, c)), t), buildHeap h)
			in (* Riscende, e ad ogni passo in discesa: *)
				let  
					(* 1: espande l'oggetto con la classse corrente e inizializza i campi nell'buildHeap con i valori di default*)
					val (x1, y1) = alloc( programmaTipato,  x, c) 
				in
					(* 2: Torna l'oggetto creato, espandendo l'buildHeap con i campi aggiunti *)
					( x1,  concatHeap(y, y1) )
				end
			end;

(* Inizializza la buildLoc nell'buildHeap conil corretto right value.  
QUI I CAMPI DEVONO ESSERE INZIIZALIZZATI RICHIAMANDO regolaEspressione, CON UN AMBIENTE IN CUI è PRESENTE (THIS, OBJ) *)
fun cercaInitCampoApp ( programmaTipato, [], nomeC campoSintattico ) = raise RuntimeErrorInitCampoNonTrovato
	| cercaInitCampoApp ( programmaTipato, (defCampoS( tipoca, nomeC nomeca, rightca))::campi , nomeC campoSintattico ) =
			if( nomeca = campoSintattico ) then rightca else cercaInitCampoApp(programmaTipato, campi, nomeC campoSintattico )
and cercaInitCampo( programmaTipato, nomec, nomecl) = cercaInitCampoApp(programmaTipato, cbody ( programmaTipato, nomecl ), nomec );

fun initCampiApp( programmaTipato, obj, istanza( nomec, []),  buildHeap h  ) =  buildHeap h  

	| initCampiApp( programmaTipato, obj, istanza( nomec, (classecampo, nomecampo, loccampo)::l), buildHeap h ) = 
		let val (x, _) = regolaEspressione( programmaTipato, 
										buildEnv [(this, objV obj )],
										cercaInitCampo(programmaTipato, nomecampo, classecampo), 
										buildHeap h)
		in
			initCampiApp( programmaTipato, obj, istanza( nomec, l), changeHeap( buildHeap h , loccampo , x ))
		end
and initCampi( programmaTipato, obj, mem ) = initCampiApp( programmaTipato, obj, obj, mem)
(* fine emtodi per inziializzare i campi! *)

(*REGOLE PER VALUTARE RIGHT EXPRESSION *)
and regolaEspressione (programmaTipato, buildEnv a, varExprT(v, t), buildHeap h) = (getValEnv(buildEnv a, varNome v),buildHeap h)

	| regolaEspressione (programmaTipato, buildEnv a, intExprT (n, t), buildHeap h) = (intV(n), buildHeap h)

 	| regolaEspressione (programmaTipato, buildEnv a,  thisT (t) , buildHeap h) = (getValEnv(buildEnv a, this), buildHeap h)

	| regolaEspressione (programmaTipato, buildEnv a,  superT (t) , buildHeap h) = (let val x = getObjFromVal (getValEnv(buildEnv a, this)) 
									in
										(
										objV ( istanza ( getSuperClasseOggetto(programmaTipato, x), getSuperCampi(programmaTipato, x))),
										buildHeap h 
										)
									end)

	| regolaEspressione (programmaTipato, buildEnv a,  nullT (t) , buildHeap h) = (nullV, buildHeap h)

	| regolaEspressione (programmaTipato, buildEnv a, newT (nomeclasse, t), buildHeap h) =  
		(* Alloca l'intS oggetto*)
		let val (x, y) = allocaOggetto (programmaTipato, newT nomeclasse, buildHeap h)
		in 
			(* 2: calcola il rightvalue di tutti i campi dell'oggetto e li assegna.  *)
			( objV x, initCampi( programmaTipato, x, y ))
		end
	| regolaEspressione (programmaTipato, buildEnv a, _ , buildHeap h) =( intV 999, buildHeap h);  

use "ProgrammiEsempio.sml";

print (stampaProgrammaS esempioDispensa);
print (let val (x,y ) =regolaEspressione( esempioDispensa, buildEnv [], newS( nomeCl "A"), buildHeap []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);
print (let val (x,y ) =regolaEspressione( esempioDispensa, buildEnv [], newS( nomeCl "B"), buildHeap []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);
print (let val (x,y ) =regolaEspressione( esempioDispensa, buildEnv [], newS( nomeCl "weird"), buildHeap []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);

controllaTipoProgramma( esempioDispensa )