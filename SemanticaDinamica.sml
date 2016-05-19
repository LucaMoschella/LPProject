use "SintassiAstratta.sml";
use "SemanticaStatica.sml";


datatype  Loc = locazione of int ;
val currentLocInt: int ref = ref 0; (* DA PROVARE UNIFICAZIONE CON LA FUNZIONE *)
fun nextLoc () = (currentLocInt := (!currentLocInt) + 1; locazione (!currentLocInt));

datatype Obj = 	istanza of nomeClasse * ((nomeClasse * nomeCampo * Loc ) list);
datatype Val = valInt of int | valObj of Obj | valNull | valAssente;
datatype Env = ambiente of ((Varpiu * Val) list); (* VALUTARE L'ASSOCIAIONE CON LOC, E NON VAL *)
datatype Heap = memoria of ((Loc * Val) list);


exception VarNotFoundInEnv
exception LocNotFoundInHeap
exception ValIsNotObj
exception ValIsNotInt
exception InitCampoNonTrovato

(* GESTIONE AMBIENTE *)
fun getValEnv( ambiente [], var:Varpiu ) = raise VarNotFoundInEnv
	| getValEnv( ambiente ((k,v)::l), var:Varpiu) = if (k = var)then (v) else (getValEnv(ambiente l,var)); 

(* GESTIONE HEAP *)
fun concatHeap( memoria h1, memoria h2) = memoria ( h1 @ h2) ;

fun changeHeapApp( memoria [], memoria newHeap, key, value  ) = memoria newHeap
	| changeHeapApp( memoria ((keyh,v)::l), memoria newHeap, key, value  ) = 
		if( keyh = key) then changeHeapApp(  memoria l, memoria ((keyh, value)::newHeap), key, value) else changeHeapApp(  memoria l, memoria ((keyh, v)::newHeap), key, value)
and changeHeap( h, key, value  ) = changeHeapApp(h, memoria [], key, value);

fun getValHeap( memoria [], loc ) = raise LocNotFoundInHeap
	| getValHeap( memoria ((k,v)::l), loc) = if (k = loc)then (v) else (getValHeap(memoria l,loc)); 

(* FUNZIONI DI COMODO *)
fun getSuperClasseOggetto(programma, istanza(c,(_)))=getExtendedClass(cercaClasseInProgramma ( programma, c ));

fun getSuperCampi(programma, istanza ( n, l))= getSuperCampiAppoggio(programma,istanza ( n, l),istanza ( n, []))
and
	 getSuperCampiAppoggio(programma, istanza (n1, []), istanza (n2, l))=l

	 | getSuperCampiAppoggio(programma, istanza(n1, (nomec,nomef,loc)::l), istanza(n2,l2))= 
	 if (nomec=n1)
	 then getSuperCampiAppoggio(programma,istanza(n1,l),istanza(n2,l2))
	 else getSuperCampiAppoggio(programma, istanza(n1,l), istanza(n2,((nomec,nomef,loc)::l2)) )
and
	getObjFromVal( valObj obj) = obj
	| getObjFromVal( _ ) = raise ValIsNotObj;

fun tipoDefault( intero ) = valInt 0
	| tipoDefault( class _ ) =valNull ;

fun cbody( programma,  nomec ) = let val ( defClass( _ , _ , campi, _ ) ) = cercaClasseInProgramma ( programma, nomec ) 
								in campi end;

(* Prende un ( oggetto, classe ) => ( obj, heap) dove obj = oggetto + campi in classe, con i valori nell'heap *)
fun aggiornaObjAndHeap([], ncl, istanza(n, l2), memoria h) = (istanza(ncl,l2), memoria h)
	| aggiornaObjAndHeap( defCampo( t, nc, r)::l1, ncl, istanza(n, l2), memoria h)= (	let val x = nextLoc() 
																						in aggiornaObjAndHeap( l1, ncl, istanza(n, (ncl,nc,x)::l2), memoria( (x, tipoDefault(t))::h)) 
																						end)
	
and alloc (programma, obj, ncl) = aggiornaObjAndHeap( cbody(programma, ncl), ncl, obj, memoria [])

and allocaOggetto( programma, new (Object), memoria h ) = ( istanza( Object, []) , memoria h)
	| allocaOggetto( programma, new ( c ), memoria h ) =
		(* Sale verso Object*)
			let val (x, y) = allocaOggetto (programma, new( getExtendedClass( cercaClasseInProgramma( programma, c))), memoria h)
			in (* Riscende, e ad ogni passo in discesa: *)
				let  
					(* 1: espande l'oggetto con la classse corrente e inizializza i campi nell'heap con i valori di default*)
					val (x1, y1) = alloc( programma,  x, c) 
				in
					(* 2: Torna l'oggetto creato, espandendo l'heap con i campi aggiunti *)
					( x1,  concatHeap(y, y1) )
				end
			end;

(* Inizializza la locazione nell'heap conil corretto right value.  
QUI I CAMPI DEVONO ESSERE INZIIZALIZZATI RICHIAMANDO regolaRightExpr, CON UN AMBIENTE IN CUI è PRESENTE (THIS, OBJ) *)
fun cercaInitCampoApp ( programma, [], nomeC campo ) = raise InitCampoNonTrovato
	| cercaInitCampoApp ( programma, (defCampo( tipoca, nomeC nomeca, rightca))::campi , nomeC campo ) =
			if( nomeca = campo ) then rightca else cercaInitCampoApp(programma, campi, nomeC campo )
and cercaInitCampo( programma, nomec, nomecl) = cercaInitCampoApp(programma, cbody ( programma, nomecl ), nomec );

fun initCampiApp( programma, obj, istanza( nomec, []),  memoria h  ) =  memoria h  

	| initCampiApp( programma, obj, istanza( nomec, (classecampo, nomecampo, loccampo)::l), memoria h ) = 
		let val (x, _) = regolaRightExpr( programma, 
										ambiente [(varThis, valObj obj )],
										cercaInitCampo(programma, nomecampo, classecampo), 
										memoria h)
		in
			initCampiApp( programma, obj, istanza( nomec, l), changeHeap( memoria h , loccampo , x ))
		end
and initCampi( programma, obj, mem ) = initCampiApp( programma, obj, obj, mem)
(* fine emtodi per inziializzare i campi! *)

(*REGOLE PER VALUTARE RIGHT EXPRESSION *)
and regolaRightExpr (programma, ambiente a, isvariabile(v), memoria h) = (getValEnv(ambiente a, varNome v),memoria h)

	| regolaRightExpr (programma, ambiente a, isint n, memoria h) = (valInt(n), memoria h)

 	| regolaRightExpr (programma, ambiente a, kw this, memoria h) = (getValEnv(ambiente a, varThis), memoria h)

	| regolaRightExpr (programma, ambiente a, kw super, memoria h) = (let val x = getObjFromVal (getValEnv(ambiente a, varThis)) 
									in
										(
										valObj ( istanza ( getSuperClasseOggetto(programma, x), getSuperCampi(programma, x))),
										memoria h 
										)
									end)

	| regolaRightExpr (programma, ambiente a, kw null, memoria h) = (valNull, memoria h)

	| regolaRightExpr (programma, ambiente a, new newObj, memoria h) =  
		(* Alloca l'intero oggetto*)
		let val (x, y) = allocaOggetto (programma, new newObj, memoria h)
		in 
			(* 2: calcola il rightvalue di tutti i campi dell'oggetto e li assegna.  *)
			( valObj x, initCampi( programma, x, y ))
		end
	| regolaRightExpr (programma, ambiente a, _ , memoria h) =( valInt 999, memoria h);  

use "PrintToJava.sml";
use "ProgrammiEsempio.sml";

print (stampaProgramma esempioDispensa);
print (let val (x,y ) =regolaRightExpr( esempioDispensa, ambiente [], new( nomeCl "A"), memoria []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);
print (let val (x,y ) =regolaRightExpr( esempioDispensa, ambiente [], new( nomeCl "B"), memoria []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);
print (let val (x,y ) =regolaRightExpr( esempioDispensa, ambiente [], new( nomeCl "weird"), memoria []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);

controllaTipoProgramma( esempioDispensa )