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

(********************************* FUNZIONI PER STAMPARE SINTASSI ASTRATTA ************************************************)
fun stampaNomeCampo (nomeC s) = s;
fun stampaNomeVar (nomeV s) = s;
fun stampaNomeMetodo (nomeM s) = s;
fun stampaNomeClasse (nomeCl s) = s |
	stampaNomeClasse Object = "Object";
fun stampaNomeTipo (intS) = "int"
	| stampaNomeTipo (classeS a) = stampaNomeClasse a;

(*********************************)

fun stampaDefVariabile (ind, defVarS (t,n)) = ind ^ (stampaNomeTipo t ) ^ " " ^ (stampaNomeVar n)

and	stampaDefCampo (ind, defCampoS (t, n, v )) = ind ^ (stampaNomeTipo t ) ^ " " ^ (stampaNomeCampo n) ^ " = " ^ (stampaRightValue ("",v))

and stampaComando  (ind, assegnamentoVarS (n,v)) = 
		ind ^ (stampaNomeVar n) ^ " = " ^ (stampaRightValue ("",v))

	| stampaComando (ind, assegnamentoCampoS ( left , n, right )) = 
		ind ^  ( stampaRightValue ("",left)) ^ "." ^ (stampaNomeCampo n ) ^ " = " ^ (stampaRightValue ("",right))

	| stampaComando (ind, returnS r) = 
		ind ^  "return " ^ (stampaRightValue ("",r))	

and stampaMetodo ( ind:string, defMetodoS (t, n, args, locals, commands )) = 
		ind ^ "public " ^ (stampaNomeTipo t ) ^ " " ^ (stampaNomeMetodo n) ^  (stampaListaInLine(args, "", "(", "",", ", "",")",  stampaDefVariabile )) ^ "\n" ^
		ind ^ "{" ^ 	(stampaListaNewLine(locals, ind ^ DEF_IND, "", "", ";", ";\n","",  stampaDefVariabile )) ^ 
						(stampaListaNewLine(commands, ind ^ DEF_IND,"", "", ";", ";", "", stampaComando )) ^ "\n" ^
		ind ^ "}\n"			

and stampaClasse ( ind:string, defClasseS (n, nfrom, campi, metodi ) ) = 
		ind ^ "public class " ^ (stampaNomeClasse n) ^ " extends " ^ (stampaNomeClasse nfrom) ^ "\n" ^
		ind ^ "{" ^ 	(stampaListaNewLine(campi, ind ^ DEF_IND, "", "", ";", ";\n", "", stampaDefCampo )) ^ 
						(stampaListaNewLine(metodi, ind ^ DEF_IND, "",  "", "", "", "", stampaMetodo ))  ^
		ind ^ "}\n"

and stampaRightValue (ind,  varExprS n) = ind ^ (stampaNomeVar n )
	| stampaRightValue (ind,  intExprS i) = ind ^ (Int.toString i)
	| stampaRightValue (ind,  thisS) = ind ^ ("this")
	| stampaRightValue (ind,  superS)  = ind ^ ("super")
	| stampaRightValue (ind,  nullS) = ind ^ ("null")
	| stampaRightValue (ind, newS c) = ind ^ ("new " ^ (stampaNomeClasse c) ^ "()")
	| stampaRightValue (ind, accessoCampoS (v, nomeC c)) = ind ^ ((stampaRightValue ("",v)) ^ "." ^ (c))
	| stampaRightValue (ind, chiamataMetodoS (v,n, args)) = ind ^ ((stampaRightValue ("",v)) ^ "." ^ (stampaNomeMetodo n )  ^ (stampaListaInLine(args, "", "(", "",  ", ", "", ")",  stampaRightValue )))

and stampaProgramma ( codiceS l ) = 
	stampaListaNewLine( l, DEF_IND, "Programma Java:","", "", "","",  stampaClasse); 

(* FUNZIONI PER STAMPARE SEMANTICA STATICA *)
fun stampaTypes (classeT s) = stampaNomeClasse( s )
	| stampaTypes ( intT) = "int"
	| stampaTypes (T) = "T";

fun stampaVarPiu ( varNome n ) = stampaNomeVar n
	| stampaVarPiu (this) = "this";

fun stampaCoppiaVarPiuType( ind, (v, t)) = ind ^ "(" ^ (stampaVarPiu v) ^ ":" ^ (stampaTypes t) ^ ")"
and stampaContesto ( buildContesto l) = stampaListaInLine(l, "", "[", "", "; ", "", "]\n", stampaCoppiaVarPiuType);


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