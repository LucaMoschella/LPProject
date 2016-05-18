use "SintassiAstratta.sml";
use "SemanticaStatica.sml";

use "PrintToJava.sml";
use "ProgrammiEsempio.sml";



datatype  Loc = locazione of int ;
val currentLocInt: int ref = ref 0; (* DA PROVARE UNIFICAZIONE CON LA FUNZIONE *)
fun nextLoc () = (currentLocInt := (!currentLocInt) + 1; locazione (!currentLocInt));

datatype Obj = 	istanza of nomeClasse * ((nomeClasse * nomeCampo * Loc ) list);
datatype Val = valInt of int | valObj of Obj | valNull | valAssente;
datatype Env = ambiente of ((Varpiu * Val) list); (* VALUTARE L'ASSOCIAIONE CON LOC, E NON VAL *)
datatype Heap = memoria of ((Loc * Val) list);


(* GESTIONE AMBIENTE *)
exception VarNotFoundInEnv
exception ValIsNotObj
exception ValIsNotInt

fun getValEnv( ambiente [], var:Varpiu ) = raise VarNotFoundInEnv
	| getValEnv( ambiente ((k,v)::l), var:Varpiu) = if (k = var)then (v) else (getValEnv(ambiente l,var)); 

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

(*REGOLE*)
fun regolaVariabile (programma, a, v, h) = (getValEnv(a, v),h);



fun regolaSuper (programma, a, h) = (let val x = getObjFromVal (getValEnv(a, varThis)) 
									in
										(istanza (getSuperClasseOggetto(programma, x), getSuperCampi(programma, x)), h )
									end);

fun regolaNull (programma, a, h) = (kw null, h);

fun regolaInt (programma, a, valInt(n), h) = (valInt(n), h)
| regolaInt (programma, a, _, h) = raise ValIsNotInt;

