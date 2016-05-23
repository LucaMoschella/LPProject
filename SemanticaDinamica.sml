use "Sintassi.sml";
use "SemanticaStatica.sml";







(* GESTIONE AMBIENTE *)
fun getValEnv( buildEnv [], var:varPiu ) = raise VarNotFoundInEnv
	| getValEnv( buildEnv ((k,v)::l), var:varPiu) = if (k = var)then (v) else (getValEnv(buildEnv l,var)); 

(* GESTIONE HEAP *)
fun concatHeap( buildHeap h1, buildHeap h2) = buildHeap ( h1 @ h2) ;

fun changeHeapApp( buildHeap [], buildHeap newHeap, key, value  ) = buildHeap newHeap
	| changeHeapApp( buildHeap ((keyh,v)::l), buildHeap newHeap, key, value  ) = 
		if( keyh = key) then changeHeapApp(  buildHeap l, buildHeap ((keyh, value)::newHeap), key, value) else changeHeapApp(  buildHeap l, buildHeap ((keyh, v)::newHeap), key, value)
and changeHeap( h, key, value  ) = changeHeapApp(h, buildHeap [], key, value);

fun getValHeap( buildHeap [], lo ) = raise LocNotFoundInHeap
	| getValHeap( buildHeap ((k,v)::l), lo) = if (k = lo)then (v) else (getValHeap(buildHeap l,lo)); 

(* FUNZIONI DI COMODO *)
fun getSuperClasseOggetto(programmaSintattico, istanza(c,(_)))=getExtendedClass(cercaClasseInProgramma ( programmaSintattico, c ));

fun getSuperCampi(programmaSintattico, istanza ( n, l))= getSuperCampiAppoggio(programmaSintattico,istanza ( n, l),istanza ( n, []))
and
	 getSuperCampiAppoggio(programmaSintattico, istanza (n1, []), istanza (n2, l))=l

	 | getSuperCampiAppoggio(programmaSintattico, istanza(n1, (nomec,nomef,lo)::l), istanza(n2,l2))= 
	 if (nomec=n1)
	 then getSuperCampiAppoggio(programmaSintattico,istanza(n1,l),istanza(n2,l2))
	 else getSuperCampiAppoggio(programmaSintattico, istanza(n1,l), istanza(n2,((nomec,nomef,lo)::l2)) )
and
	getObjFromVal( objV obj) = obj
	| getObjFromVal( _ ) = raise ValIsNotObj;

fun tipoDefault( intS ) = intV 0
	| tipoDefault( classeS _ ) =nullV ;

fun cbody( programmaSintattico,  nomec ) = let val ( defClasseS( _ , _ , campi, _ ) ) = cercaClasseInProgramma ( programmaSintattico, nomec ) 
								in campi end;

(* Prende un ( oggetto, classeSintattica ) => ( obj, buildHeap) dove obj = oggetto + campi in classeSintattica, con i valori nell'buildHeap *)
fun aggiornaObjAndHeap([], ncl, istanza(n, l2), buildHeap h) = (istanza(ncl,l2), buildHeap h)
	| aggiornaObjAndHeap( defCampoS( t, nc, r)::l1, ncl, istanza(n, l2), buildHeap h)= (	let val x = nextLoc() 
																						in aggiornaObjAndHeap( l1, ncl, istanza(n, (ncl,nc,x)::l2), buildHeap( (x, tipoDefault(t))::h)) 
																						end)
	
and alloc (programmaSintattico, obj, ncl) = aggiornaObjAndHeap( cbody(programmaSintattico, ncl), ncl, obj, buildHeap [])

and allocaOggetto( programmaSintattico, newS (Object), buildHeap h ) = ( istanza( Object, []) , buildHeap h)
	| allocaOggetto( programmaSintattico, newS ( c ), buildHeap h ) =
		(* Sale verso Object*)
			let val (x, y) = allocaOggetto (programmaSintattico, newS( getExtendedClass( cercaClasseInProgramma( programmaSintattico, c))), buildHeap h)
			in (* Riscende, e ad ogni passo in discesa: *)
				let  
					(* 1: espande l'oggetto con la classse corrente e inizializza i campi nell'buildHeap con i valori di default*)
					val (x1, y1) = alloc( programmaSintattico,  x, c) 
				in
					(* 2: Torna l'oggetto creato, espandendo l'buildHeap con i campi aggiunti *)
					( x1,  concatHeap(y, y1) )
				end
			end;

(* Inizializza la buildLoc nell'buildHeap conil corretto right value.  
QUI I CAMPI DEVONO ESSERE INZIIZALIZZATI RICHIAMANDO regolaRightExpr, CON UN AMBIENTE IN CUI è PRESENTE (THIS, OBJ) *)
fun cercaInitCampoApp ( programmaSintattico, [], nomeC campoSintattico ) = raise InitCampoNonTrovato
	| cercaInitCampoApp ( programmaSintattico, (defCampoS( tipoca, nomeC nomeca, rightca))::campi , nomeC campoSintattico ) =
			if( nomeca = campoSintattico ) then rightca else cercaInitCampoApp(programmaSintattico, campi, nomeC campoSintattico )
and cercaInitCampo( programmaSintattico, nomec, nomecl) = cercaInitCampoApp(programmaSintattico, cbody ( programmaSintattico, nomecl ), nomec );

fun initCampiApp( programmaSintattico, obj, istanza( nomec, []),  buildHeap h  ) =  buildHeap h  

	| initCampiApp( programmaSintattico, obj, istanza( nomec, (classecampo, nomecampo, loccampo)::l), buildHeap h ) = 
		let val (x, _) = regolaRightExpr( programmaSintattico, 
										buildEnv [(thisT, objV obj )],
										cercaInitCampo(programmaSintattico, nomecampo, classecampo), 
										buildHeap h)
		in
			initCampiApp( programmaSintattico, obj, istanza( nomec, l), changeHeap( buildHeap h , loccampo , x ))
		end
and initCampi( programmaSintattico, obj, mem ) = initCampiApp( programmaSintattico, obj, obj, mem)
(* fine emtodi per inziializzare i campi! *)

(*REGOLE PER VALUTARE RIGHT EXPRESSION *)
and regolaRightExpr (programmaSintattico, buildEnv a, varExprS(v), buildHeap h) = (getValEnv(buildEnv a, varNome v),buildHeap h)

	| regolaRightExpr (programmaSintattico, buildEnv a, intExprS n, buildHeap h) = (intV(n), buildHeap h)

 	| regolaRightExpr (programmaSintattico, buildEnv a,  thisS, buildHeap h) = (getValEnv(buildEnv a, thisT), buildHeap h)

	| regolaRightExpr (programmaSintattico, buildEnv a,  superS, buildHeap h) = (let val x = getObjFromVal (getValEnv(buildEnv a, thisT)) 
									in
										(
										objV ( istanza ( getSuperClasseOggetto(programmaSintattico, x), getSuperCampi(programmaSintattico, x))),
										buildHeap h 
										)
									end)

	| regolaRightExpr (programmaSintattico, buildEnv a,  nullS, buildHeap h) = (nullV, buildHeap h)

	| regolaRightExpr (programmaSintattico, buildEnv a, newS newObj, buildHeap h) =  
		(* Alloca l'intS oggetto*)
		let val (x, y) = allocaOggetto (programmaSintattico, newS newObj, buildHeap h)
		in 
			(* 2: calcola il rightvalue di tutti i campi dell'oggetto e li assegna.  *)
			( objV x, initCampi( programmaSintattico, x, y ))
		end
	| regolaRightExpr (programmaSintattico, buildEnv a, _ , buildHeap h) =( intV 999, buildHeap h);  

use "PrintToJava.sml";
use "ProgrammiEsempio.sml";

print (stampaProgramma esempioDispensa);
print (let val (x,y ) =regolaRightExpr( esempioDispensa, buildEnv [], newS( nomeCl "A"), buildHeap []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);
print (let val (x,y ) =regolaRightExpr( esempioDispensa, buildEnv [], newS( nomeCl "B"), buildHeap []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);
print (let val (x,y ) =regolaRightExpr( esempioDispensa, buildEnv [], newS( nomeCl "weird"), buildHeap []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);

controllaTipoProgramma( esempioDispensa )