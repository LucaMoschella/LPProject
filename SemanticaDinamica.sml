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
exception ValIsNotObj
exception ValIsNotInt
(* GESTIONE AMBIENTE *)

fun getValEnv( ambiente [], var:Varpiu ) = raise VarNotFoundInEnv
	| getValEnv( ambiente ((k,v)::l), var:Varpiu) = if (k = var)then (v) else (getValEnv(ambiente l,var)); 
(* GESTIONE HEAP *)

fun concatHeap( memoria h1, memoria h2) = memoria ( h1 @ h2) ;

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

fun aggiornaObjAndHeap([], ncl, istanza(n, l2), memoria h) = (istanza(ncl,l2), memoria h)
	| aggiornaObjAndHeap( defCampo( t, nc, r)::l1, ncl, istanza(n, l2), memoria h)= (	let val x = nextLoc() 
																						in aggiornaObjAndHeap( l1, ncl, istanza(n, (ncl,nc,x)::l2), memoria( (x, tipoDefault(t))::h)) 
																						end);

(* Prende un ( oggetto, classe ) => ( obj, heap) dove obj = oggetto + campi in classe, con i valori nell'heap *)
fun alloc (programma, obj, ncl) = aggiornaObjAndHeap( cbody(programma, ncl), ncl, obj, memoria []);

(* Inizializza la locazione nell'heap conil corretto right value. *)
fun initCampi( programma, obj, memoria h ) = QUI I CAMPI DEVONO ESSERE INZIIZALIZZATI RICHIAMANDO regolaRightExpr, CON UN AMBIENTE IN CUI è PRESENTE (THIS, OBJ)

(*REGOLE PER VALUTARE RIGHT EXPRESSION *)
fun regolaRightExpr (programma, ambiente a, isvariabile(v), memoria h) = (getValEnv(ambiente a, varNome v),memoria h)

	| regolaRightExpr (programma, ambiente a, isint n, memoria h) = (valInt(n), memoria h)

 	| regolaRightExpr (programma, ambiente a, kw this, memoria h) = (getValEnv(ambiente a, varThis), memoria h)

	| regolaRightExpr (programma, ambiente a, kw super, memoria h) = (let val x = getObjFromVal (getValEnv(ambiente a, varThis)) 
									in
										(valObj (istanza (getSuperClasseOggetto(programma, x), getSuperCampi(programma, x))), memoria h )
									end)

	| regolaRightExpr (programma, ambiente a, kw null, memoria h) = (valNull, memoria h)

	| regolaRightExpr (programma, ambiente a, new( Object), memoria h) = (valObj( istanza( Object, [])) , memoria h)

	| regolaRightExpr (programma, ambiente a, new( c ), memoria h) =  
		let val (x, y) = regolaRightExpr (programma, ambiente a, new( getExtendedClass( cercaClasseInProgramma( programma, c))), memoria h)
		in 
			let 
				val (x1, y1) = alloc( programma, getObjFromVal x, c) 
			in
				let 
					val h = initCampi( programma, x1, concatHeap(y, y1) )
				in
					( valObj x1, h)
				end
			end
		end;

use "PrintToJava.sml";
use "ProgrammiEsempio.sml";

print (stampaProgramma esempioDispensa);
print (let val (x,y ) =regolaRightExpr( esempioDispensa, ambiente [], new( nomeCl "weird"), memoria []);
in 
	"Oggetto: " ^ stampaVal(x) ^"\nHeap: " ^ stampaHeap(y) ^ "\n"
end);