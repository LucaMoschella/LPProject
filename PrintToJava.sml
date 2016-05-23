(* FUNZIONI PER STAMPARE SINTASSI *)


fun stampaLista( [],  ind:string, sepchar:string, metodoSintattico) = "" |
	stampaLista( l::[], ind:string, sepchar:string, metodoSintattico) = ind ^ (metodoSintattico l) |
	stampaLista( l::lista, ind:string, sepchar:string, metodoSintattico) = ind ^ (metodoSintattico l) ^ sepchar ^ (stampaLista (lista, ind, sepchar, metodoSintattico));


fun stampaNomeCampo (nomeC s) = s;
fun stampaNomeVar (nomeV s) = s;
fun stampaNomeMetodo (nomeM s) = s;
fun stampaNomeClasse (nomeCl s) = s |
	stampaNomeClasse Object = "Object";


fun stampaTipo intS = "int"
	| stampaTipo (classeS a) = stampaNomeClasse a
and
	stampaVariabile ( defVarS (t,n)) = (stampaTipo t ) ^ " " ^ (stampaNomeVar n)
and
	stampaCampo ( defCampoS (t, n, v )) = (stampaTipo t ) ^ " " ^ (stampaNomeCampo n) ^ " = " ^ (stampaRightValue v)
and
	stampaMetodo ( defMetodoS (t,n, args, locals, commands )) = 	(stampaTipo t ) ^ " " ^ (stampaNomeMetodo n) ^ "( " ^ (stampaLista(args,"", ", ", stampaVariabile )) ^ " ) " ^
		"\n{\n" ^ (stampaLista(locals,"    ", ";\n", stampaVariabile )) ^ "\n" ^ (stampaLista(commands,"    ", ";\n", stampaComando )) ^ "\n}"
and
	stampaClasse (defClasseS (n, nfrom, campi, metodi ) ) = 
		"classeS " ^ (stampaNomeClasse n) ^ " extends " ^ (stampaNomeClasse nfrom) ^
		"\n{\n" ^ (stampaLista(campi,"    ", ";\n", stampaCampo )) ^ "\n" ^ (stampaLista(metodi,"    ", "\n", stampaMetodo )) ^ "\n}\n"
and
	stampaComando  (assegnamentoVarS (n,v)) = (stampaNomeVar n) ^ " = " ^ (stampaRightValue v)
	| stampaComando ( assegnamentoCampoS ( left , n, right )) = ( stampaRightValue left) ^ "." ^ (stampaNomeCampo n ) ^ " = " ^ (stampaRightValue right)
	| stampaComando ( returnS r) = "returnS " ^ (stampaRightValue r)	
and 
	stampaRightValue ( varExprS n) = stampaNomeVar n 
	| stampaRightValue ( intExprS i) = Int.toString i
	| stampaRightValue ( thisS) = "thisS"
	| stampaRightValue ( superS)  = "superS"
	| stampaRightValue ( nullS) = "nullS"
	| stampaRightValue (newS c) = "newS " ^ (stampaNomeClasse c) ^ "()"
	| stampaRightValue (accessoCampoS (v, nomeC c)) = (stampaRightValue v) ^ "." ^ (c)
	| stampaRightValue (chiamataMetodoS (v,n, args)) = (stampaRightValue v) ^ "." ^ (stampaNomeMetodo n ) ^ "( " ^ (stampaLista(args, "",  ", ", stampaRightValue )) ^" )"
and
	stampaProgramma ( codiceS l ) = stampaLista( l, "", "\n", stampaClasse); 

(* FUNZIONI PER STAMPARE SEMANTICA STATICA *)
fun stampaTypes (classeT s) = stampaNomeClasse( s )
	| stampaTypes ( intT) = "int"
	| stampaTypes (T) = "T";
fun stampaVarPiu ( varNome n ) = stampaNomeVar n
	| stampaVarPiu (thisT) = "thisS";
fun stampaContesto ( buildContesto []) = "\n"
	| stampaContesto ( buildContesto( (v,t)::l)) = "( " ^ (stampaVarPiu v) ^ ":" ^ (stampaTypes t) ^ " ) ; " ^ (stampaContesto (buildContesto l));


(***************** STAMPA SEMNATICA DINAMICA **********************)

(** DA SOTITUIRE CON QUELLE SOPRA **)
fun stampaListaInLineApp( [],  midsep:string, endsep:string, metodoSintattico) = "" |
	stampaListaInLineApp( l::[], midsep:string, endsep:string, metodoSintattico) =  (metodoSintattico l) ^ endsep |
	stampaListaInLineApp( l::lista, midsep:string, endsep:string, metodoSintattico) =  (metodoSintattico l) ^ midsep ^ (stampaListaInLineApp (lista, midsep, endsep, metodoSintattico))
and stampaListaInLine( l, ind:string, midsep:string, endsep:string, metodoSintattico) = ind ^ stampaListaInLineApp(  l, midsep:string, endsep:string, metodoSintattico)

fun stampaListaNewLineApp( [], ind:string, midsep:string, endsep:string, metodoSintattico) = "" |
	stampaListaNewLineApp( l::[], ind:string, midsep:string, endsep:string, metodoSintattico) =   "\n" ^ ind ^ (metodoSintattico l) ^ endsep |
	stampaListaNewLineApp( l::lista, ind:string, midsep:string, endsep:string, metodoSintattico) =  "\n" ^ ind ^ (metodoSintattico l) ^ midsep ^ (stampaListaNewLineApp (lista, ind, midsep, endsep, metodoSintattico));


fun stampaLoc ( buildLoc i) = "buildLoc#" ^ (Int.toString i);

fun stampaTriplaCampiObj( nomec, nomeca, lo ) = "" ^ (stampaNomeClasse nomec) ^ ":" ^ ( stampaNomeCampo nomeca) ^ ":" ^ (stampaLoc lo) ^ "";
fun stampaObj( istanza( nomec , l) ) = "{ obj:" ^ (stampaNomeClasse( nomec )) ^ " - Campi: " ^ (stampaListaInLine(l, "[", ", ", "]", stampaTriplaCampiObj )) ^ " }"  ;

fun stampaVal( noV ) = "*"
	| stampaVal( nullV ) = "nullS"
	| stampaVal( objV( obj ) )  = stampaObj obj
	| stampaVal( intV i ) = Int.toString i;

fun stampaCoppiaVarVal( variabileSintattica, v ) = "" ^ (stampaVarPiu variabileSintattica) ^ ":" ^ ( stampaVal v) ^ "";
fun stampaEnv( buildEnv l) = stampaListaInLine(l, "[", ", ", "]", stampaCoppiaVarVal );

fun stampaCoppiaLocVal( locaz, v) = "" ^ (stampaLoc locaz) ^ ":" ^ ( stampaVal v) ^ "";
fun stampaHeap( buildHeap l) = stampaListaInLine(l, "[", ", ", "]", stampaCoppiaLocVal );