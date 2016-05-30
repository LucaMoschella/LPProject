fun stampaListaApp( [], newline:string, ind:string, midsep:string, endsep:string, ender:string, f) = ender |
	stampaListaApp( l::[], newline:string, ind:string, midsep:string, endsep:string, ender:string,f) =  newline ^ (f (ind,l)) ^ endsep ^ ender |
	stampaListaApp( l::lista, newline:string, ind:string, midsep:string, endsep:string, ender:string, f) =  
		newline ^ (f (ind,l)) ^ midsep ^ (stampaListaApp (lista,newline, ind, midsep, endsep, ender, f))
		
and stampaLista( [], newline:string, ind:string, starter:string, presep:string, midsep:string, endsep:string, ender:string, f) = starter ^ ender
	| stampaLista( l, newline:string, ind:string, starter:string, presep:string, midsep:string, endsep:string, ender:string, f) = 
		starter ^ presep ^ (stampaListaApp(  l, newline, ind, midsep:string, endsep:string, ender, f));

fun stampaListaInLine( l, ind:string, starter:string, presep:string, midsep:string, endsep:string, ender:string, f) = 
		stampaLista(  l, "", ind, starter, presep, midsep:string, endsep:string, ender, f);

fun stampaListaNewLine( l, ind:string, starter:string, presep:string, midsep:string, endsep:string, ender:string, f) = 
		stampaLista(  l, "\n", ind, starter, presep, midsep:string, endsep:string, ender, f);


val DEF_IND = "    ";

(********************************* FUNZIONI PER STAMPARE NOMI ************************************************)
fun stampaNomeCampo (nomeC s) = s;
fun stampaNomeVar (nomeV s) = s;
fun stampaNomeMetodo (nomeM s) = s;
fun stampaNomeClasse (nomeCl s) = s |
	stampaNomeClasse Object = "Object";

fun stampaNomeTipoS (intS) = "int"
	| stampaNomeTipoS (classeS a) = stampaNomeClasse a;

fun stampaNomeTipoT (classeT s) = stampaNomeClasse( s )
	| stampaNomeTipoT ( intT) = "int"
	| stampaNomeTipoT (T) = "T";

fun stampaNomeVarPiu ( varPiuNome n ) = stampaNomeVar n
	| stampaNomeVarPiu (this) = "this";


(********************************* FUNZIONI PER STAMPARE SINTASSI ASTRATTA ************************************************)
fun stampaDefVariabileS (ind, defVarS (t,n)) = ind ^ (stampaNomeTipoS t ) ^ " " ^ (stampaNomeVar n)

and	stampaDefCampoS (ind, defCampoS (t, n, v )) = ind ^ (stampaNomeTipoS t ) ^ " " ^ (stampaNomeCampo n) ^ " = " ^ (stampaEspressioneS ("",v))

and stampaEspressioneS (ind, varExprS n) = ind ^ (stampaNomeVar n )
	| stampaEspressioneS (ind, intExprS i) = ind ^ (Int.toString i)
	| stampaEspressioneS (ind, thisS) = ind ^ ("this")
	| stampaEspressioneS (ind, superS)  = ind ^ ("super")
	| stampaEspressioneS (ind, nullS) = ind ^ ("null")
	| stampaEspressioneS (ind, newS c) = ind ^ ("new " ^ (stampaNomeClasse c) ^ "()")
	| stampaEspressioneS (ind, accessoCampoS (v, nomeC c)) = ind ^ ((stampaEspressioneS ("",v)) ^ "." ^ (c))
	| stampaEspressioneS (ind, chiamataMetodoS (v,n, args)) = 
		ind ^ ((stampaEspressioneS ("",v)) ^ "." ^ (stampaNomeMetodo n )  ^ (stampaListaInLine(args, "", "(", "",  ", ", "", ")",  stampaEspressioneS )))

and stampaComandoS  (ind, assegnamentoVarS (n,v)) = 
		ind ^ (stampaNomeVar n) ^ " = " ^ (stampaEspressioneS ("",v))

	| stampaComandoS (ind, assegnamentoCampoS ( left , n, right )) = 
		ind ^  ( stampaEspressioneS ("",left)) ^ "." ^ (stampaNomeCampo n ) ^ " = " ^ (stampaEspressioneS ("",right))

	| stampaComandoS (ind, returnS r) = 
		ind ^  "return " ^ (stampaEspressioneS ("",r))	

and stampaMetodoS ( ind:string, defMetodoS (t, n, args, locals, commands )) = 
		ind ^ "public " ^ (stampaNomeTipoS t ) ^ " " ^ (stampaNomeMetodo n) ^  (stampaListaInLine(args, "", "(", "",", ", "",")",  stampaDefVariabileS )) ^ "\n" ^
		ind ^ "{" ^ (stampaListaNewLine(locals, ind ^ DEF_IND, "", "", ";", ";\n","",  stampaDefVariabileS )) ^ 
					(stampaListaNewLine(commands, ind ^ DEF_IND,"", "", ";", ";", "", stampaComandoS )) ^ "\n" ^
		ind ^ "}"			

and stampaClasseS ( ind:string, defClasseS (n, nfrom, campi, metodi ) ) = 
		ind ^ "public class " ^ (stampaNomeClasse n) ^ " extends " ^ (stampaNomeClasse nfrom) ^ "\n" ^
		ind ^ "{" ^ (stampaListaNewLine(campi, ind ^ DEF_IND, "", "", ";", ";\n", "", stampaDefCampoS )) ^ 
					(stampaListaNewLine(metodi, ind ^ DEF_IND, "",  "", "\n", "", "", stampaMetodoS )) ^ "\n" ^
		ind ^ "}"

and stampaProgrammaS ( codiceS l ) = 
	stampaListaNewLine( l, DEF_IND, "\nProgramma Java:", "", "\n", "", "\n",  stampaClasseS); 



(********************************* FUNZIONI PER STAMPARE SINTASSI ASTRATTA TIPATA ************************************************)
fun stampaCoppia( x1:string, x2:string) = "(" ^ x1 ^ " : " ^ x2 ^ ")";
(*********************************)

fun stampaDefVariabileT (ind, defVarT (t,n, ts)) = ind ^ (stampaNomeTipoS t ) ^ " " ^ (stampaNomeVar n)  ^ " : " ^ ( stampaNomeTipoT ts)

and	stampaDefCampoT (ind, defCampoT (t, n, v, ts )) = ind ^ (stampaNomeTipoS t ) ^ " " ^ (stampaNomeCampo n) ^ " = " ^ (stampaEspressioneT ("",v)) ^ " : " ^ ( stampaNomeTipoT ts)

and stampaEspressioneT (ind, varExprT (n,t)) = ind ^ (stampaCoppia(stampaNomeVar n, stampaNomeTipoT t ))
	| stampaEspressioneT (ind, intExprT (i,t)) = ind ^ (stampaCoppia( (Int.toString i), stampaNomeTipoT t )) 
	| stampaEspressioneT (ind, thisT t) = ind ^ (stampaCoppia( ("this"), stampaNomeTipoT t )) 
	| stampaEspressioneT (ind, superT t)  = ind ^ (stampaCoppia( ("super"), stampaNomeTipoT t ))  
	| stampaEspressioneT (ind, nullT t) = ind ^ (stampaCoppia( ("null"), stampaNomeTipoT t )) 
	| stampaEspressioneT (ind, newT (c, t)) = ind ^ (stampaCoppia(  ("new " ^ (stampaNomeClasse c) ^ "()"), stampaNomeTipoT t ))
	| stampaEspressioneT (ind, accessoCampoT (v, nomeC c, t)) = ind ^ (stampaCoppia( ((stampaEspressioneT ("",v)) ^ "." ^ (c)), stampaNomeTipoT t )) 
	| stampaEspressioneT (ind, chiamataMetodoT (v, n, args, t)) = 
		ind ^ (stampaCoppia( ((stampaEspressioneT ("",v)) ^ "." ^ (stampaNomeMetodo n )  ^ (stampaListaInLine(args, "", "(", "",  ", ", "", ")",  stampaEspressioneT ))), stampaNomeTipoT t ))  

and stampaComandoT  (ind, assegnamentoVarT (n,v)) = 
		ind ^ (stampaNomeVar n) ^ " = " ^ (stampaEspressioneT ("",v))

	| stampaComandoT (ind, assegnamentoCampoT ( left , n, right )) = 
		ind ^  ( stampaEspressioneT ("",left)) ^ "." ^ (stampaNomeCampo n ) ^ " = " ^ (stampaEspressioneT ("",right))

	| stampaComandoT (ind, returnT r) = 
		ind ^  "return " ^ (stampaEspressioneT ("",r))	

and stampaMetodoT ( ind:string, defMetodoT (t, n, args, locals, commands )) = 
		ind ^ "public " ^ (stampaNomeTipoS t ) ^ " " ^ (stampaNomeMetodo n) ^  (stampaListaInLine(args, "", "(", "",", ", "",")",  stampaDefVariabileT )) ^ "\n" ^
		ind ^ "{" ^ (stampaListaNewLine(locals, ind ^ DEF_IND, "", "", ";", ";\n","",  stampaDefVariabileT )) ^ 
					(stampaListaNewLine(commands, ind ^ DEF_IND,"", "", ";", ";", "", stampaComandoT )) ^ "\n" ^
		ind ^ "}"			

and stampaClasseT ( ind:string, defClasseT (n, nfrom, campi, metodi ) ) = 
		ind ^ "public class " ^ (stampaNomeClasse n) ^ " extends " ^ (stampaNomeClasse nfrom) ^ "\n" ^
		ind ^ "{" ^ (stampaListaNewLine(campi, ind ^ DEF_IND, "", "", ";", ";\n", "", stampaDefCampoT )) ^ 
					(stampaListaNewLine(metodi, ind ^ DEF_IND, "",  "", "\n", "", "", stampaMetodoT )) ^ "\n" ^
		ind ^ "}"

and stampaProgrammaT ( codiceT l ) = 
	stampaListaNewLine( l, DEF_IND, "\nProgramma Java tipato:", "", "\n", "", "\n",  stampaClasseT); 


(********************************* FUNZIONI PER STAMPARE INFORMAZIONI DI ESECUZIONE ************************************************)
fun stampaLoc ( buildLoc i) = "loc#" ^ (Int.toString i);

fun stampaTriplaCampiObj( ind, (nomec, nomeca, lo) ) = ind ^ "(" ^ (stampaNomeClasse nomec) ^ ":" ^ ( stampaNomeCampo nomeca) ^ ":" ^ (stampaLoc lo) ^ ")";
fun stampaObj( istanza( nomec , l) ) = "{obj:" ^ (stampaNomeClasse( nomec )) ^ " - " ^ (stampaListaInLine(l, "", "Campi: <", "", ", ", "", ">", stampaTriplaCampiObj )) ^ "}";

fun stampaVal( noV ) = "*"
	| stampaVal( nullV ) = "null"
	| stampaVal( objV( obj ) )  = stampaObj obj
	| stampaVal( intV i ) = Int.toString i;


(********************************* FUNZIONI PER STAMPARE DATA LIST ************************************************)
exception NotContesto;
exception NotEnv;
exception NotHeap;
exception NotDataList;

fun stampaCoppiaVarVal( ind, (variabileSintattica, v) ) = "" ^ (stampaNomeVarPiu variabileSintattica) ^ ":" ^ ( stampaVal v) ^ "";
fun stampaCoppiaLocVal( ind, (locaz, v)) = "" ^ (stampaLoc locaz) ^ ":" ^ ( stampaVal v) ^ "";
fun stampaCoppiaVarPiuType( ind, (v, t)) = ind ^ "(" ^ (stampaNomeVarPiu v) ^ ":" ^ (stampaNomeTipoT t) ^ ")";
fun stampaCoppiaData( ind, (s1, s2)) = ind ^ "(" ^ s1 ^ ":" ^ s2 ^ ")";

fun stampaContesto (buildContesto l) = stampaListaInLine(l, "", "Contesto: [", "", "; ", "", "]\n", stampaCoppiaVarPiuType)
	| stampaContesto( _ ) = raise NotContesto;

fun stampaEnv (buildEnv  l) = stampaListaInLine(l, "", "Ambiente: [", "", "; ", "", "]\n", stampaCoppiaVarVal)
	| stampaEnv( _ ) = raise NotEnv;

fun stampaHeap (buildHeap l) = stampaListaInLine(l, "", "Heap: [", "", "; ", "", "]\n", stampaCoppiaLocVal)
	| stampaHeap( _ ) = raise NotHeap;

fun stampaDataList (buildData l) = stampaListaInLine(l, "", "Data: [", "", "; ", "", "]\n", stampaCoppiaData)
	| stampaDataList( _ ) = raise NotDataList;