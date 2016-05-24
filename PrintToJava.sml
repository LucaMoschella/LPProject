fun stampaListaApp( [], newline:string, ind:string, midsep:string, endsep:string, ender:string, metodoSintattico) = ender |
	stampaListaApp( l::[], newline:string, ind:string, midsep:string, endsep:string, ender:string,metodoSintattico) =  newline ^ (metodoSintattico (ind,l)) ^ endsep ^ ender |
	stampaListaApp( l::lista, newline:string, ind:string, midsep:string, endsep:string, ender:string, metodoSintattico) =  
		newline ^ (metodoSintattico (ind,l)) ^ midsep ^ (stampaListaApp (lista,newline, ind, midsep, endsep, ender, metodoSintattico))
		
and stampaLista( [], newline:string, ind:string, starter:string, presep:string, midsep:string, endsep:string, ender:string, metodoSintattico) = starter ^ ender
	|stampaLista( l, newline:string, ind:string, starter:string, presep:string, midsep:string, endsep:string, ender:string, metodoSintattico) = 
		starter ^ presep ^ (stampaListaApp(  l, newline, ind, midsep:string, endsep:string, ender, metodoSintattico));

fun stampaListaInLine( l, ind:string, starter:string, presep:string, midsep:string, endsep:string, ender:string, metodoSintattico) = 
		stampaLista(  l, "", ind, starter, presep, midsep:string, endsep:string, ender, metodoSintattico);

fun stampaListaNewLine( l, ind:string, starter:string, presep:string, midsep:string, endsep:string, ender:string, metodoSintattico) = 
		stampaLista(  l, "\n", ind, starter, presep, midsep:string, endsep:string, ender, metodoSintattico);


val DEF_IND = "    ";

(********************************* FUNZIONI PER STAMPARE NOMI ************************************************)
fun stampaNomeCampo (nomeC s) = s;
fun stampaNomeVar (nomeV s) = s;
fun stampaNomeMetodo (nomeM s) = s;
fun stampaNomeClasse (nomeCl s) = s |
	stampaNomeClasse Object = "Object";

fun stampaNomeTipo (intS) = "int"
	| stampaNomeTipo (classeS a) = stampaNomeClasse a;

(********************************* FUNZIONI PER STAMPARE SINTASSI ASTRATTA ************************************************)
fun stampaDefVariabileS (ind, defVarS (t,n)) = ind ^ (stampaNomeTipo t ) ^ " " ^ (stampaNomeVar n)

and	stampaDefCampoS (ind, defCampoS (t, n, v )) = ind ^ (stampaNomeTipo t ) ^ " " ^ (stampaNomeCampo n) ^ " = " ^ (stampaEspressioneS ("",v))

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
		ind ^ "public " ^ (stampaNomeTipo t ) ^ " " ^ (stampaNomeMetodo n) ^  (stampaListaInLine(args, "", "(", "",", ", "",")",  stampaDefVariabileS )) ^ "\n" ^
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
fun stampaTypes (classeT s) = stampaNomeClasse( s )
	| stampaTypes ( intT) = "int"
	| stampaTypes (T) = "T";

fun stampaVarPiu ( varNome n ) = stampaNomeVar n
	| stampaVarPiu (this) = "this";

fun stampaCoppiaVarPiuType( ind, (v, t)) = ind ^ "(" ^ (stampaVarPiu v) ^ ":" ^ (stampaTypes t) ^ ")"
and stampaContesto ( buildContesto l) = stampaListaInLine(l, "", "[", "", "; ", "", "]\n", stampaCoppiaVarPiuType);

fun stampaCoppia( x1:string, x2:string) = "(" ^ x1 ^ " : " ^ x2 ^ ")";

fun stampaDefVariabileT (ind, defVarT (t,n, ts)) = ind ^ (stampaNomeTipo t ) ^ " " ^ (stampaNomeVar n)  ^ " : " ^ ( stampaTypes ts)

and	stampaDefCampoT (ind, defCampoT (t, n, v, ts )) = ind ^ (stampaNomeTipo t ) ^ " " ^ (stampaNomeCampo n) ^ " = " ^ (stampaEspressioneT ("",v)) ^ " : " ^ ( stampaTypes ts)

and stampaEspressioneT (ind, varExprT (n,t)) = ind ^ (stampaCoppia(stampaNomeVar n, stampaTypes t ))
	| stampaEspressioneT (ind, intExprT (i,t)) = ind ^ (stampaCoppia( (Int.toString i), stampaTypes t )) 
	| stampaEspressioneT (ind, thisT t) = ind ^ (stampaCoppia( ("this"), stampaTypes t )) 
	| stampaEspressioneT (ind, superT t)  = ind ^ (stampaCoppia( ("super"), stampaTypes t ))  
	| stampaEspressioneT (ind, nullT t) = ind ^ (stampaCoppia( ("null"), stampaTypes t )) 
	| stampaEspressioneT (ind, newT (c, t)) = ind ^ (stampaCoppia(  ("new " ^ (stampaNomeClasse c) ^ "()"), stampaTypes t ))
	| stampaEspressioneT (ind, accessoCampoT (v, nomeC c, t)) = ind ^ (stampaCoppia( ((stampaEspressioneT ("",v)) ^ "." ^ (c)), stampaTypes t )) 
	| stampaEspressioneT (ind, chiamataMetodoT (v, n, args, t)) = 
		ind ^ (stampaCoppia( ((stampaEspressioneT ("",v)) ^ "." ^ (stampaNomeMetodo n )  ^ (stampaListaInLine(args, "", "(", "",  ", ", "", ")",  stampaEspressioneT ))), stampaTypes t ))  

and stampaComandoT  (ind, assegnamentoVarT (n,v)) = 
		ind ^ (stampaNomeVar n) ^ " = " ^ (stampaEspressioneT ("",v))

	| stampaComandoT (ind, assegnamentoCampoT ( left , n, right )) = 
		ind ^  ( stampaEspressioneT ("",left)) ^ "." ^ (stampaNomeCampo n ) ^ " = " ^ (stampaEspressioneT ("",right))

	| stampaComandoT (ind, returnT r) = 
		ind ^  "return " ^ (stampaEspressioneT ("",r))	

and stampaMetodoT ( ind:string, defMetodoT (t, n, args, locals, commands )) = 
		ind ^ "public " ^ (stampaNomeTipo t ) ^ " " ^ (stampaNomeMetodo n) ^  (stampaListaInLine(args, "", "(", "",", ", "",")",  stampaDefVariabileT )) ^ "\n" ^
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

(***************** STAMPA SEMNATICA DINAMICA **********************)

(** DA SOTITUIRE CON QUELLE SOPRA **)

(*
fun stampaLoc ( buildLoc i) = "buildLoc#" ^ (Int.toString i);

fun stampaTriplaCampiObj( ind, nomec, nomeca, lo ) = ind ^ "" ^ (stampaNomeClasse nomec) ^ ":" ^ ( stampaNomeCampo nomeca) ^ ":" ^ (stampaLoc lo) ^ "";
fun stampaObj( istanza( nomec , l) ) = "{ obj:" ^ (stampaNomeClasse( nomec )) ^ " - Campi: " ^ (stampaListaInLine(l, "[", ", ", "]", stampaTriplaCampiObj )) ^ " }"  ;

fun stampaVal( noV ) = "*"
	| stampaVal( nullV ) = "nullS"
	| stampaVal( objV( obj ) )  = stampaObj obj
	| stampaVal( intV i ) = Int.toString i;

fun stampaCoppiaVarVal( variabileSintattica, v ) = "" ^ (stampaVarPiu variabileSintattica) ^ ":" ^ ( stampaVal v) ^ "";
fun stampaEnv( buildEnv l) = stampaListaInLine(l, "[", ", ", "]", stampaCoppiaVarVal );

fun stampaCoppiaLocVal( locaz, v) = "" ^ (stampaLoc locaz) ^ ":" ^ ( stampaVal v) ^ "";
fun stampaHeap( buildHeap l) = stampaListaInLine(l, "[", ", ", "]", stampaCoppiaLocVal );
	*)