(* FUNZIONI PER STAMPARE SINTASSI *)


fun stampaLista( [],  ind:string, sepchar:string, metodo) = "" |
	stampaLista( l::[], ind:string, sepchar:string, metodo) = ind ^ (metodo l) |
	stampaLista( l::lista, ind:string, sepchar:string, metodo) = ind ^ (metodo l) ^ sepchar ^ (stampaLista (lista, ind, sepchar, metodo));


fun stampaNomeCampo (nomeC s) = s;
fun stampaNomeVar (nomeV s) = s;
fun stampaNomeMetodo (nomeM s) = s;
fun stampaNomeClasse (nomeCl s) = s |
	stampaNomeClasse Object = "Object";


fun stampaTipo intero = "int"
	| stampaTipo (class a) = stampaNomeClasse a
and
	stampaVariabile ( defvariabile (t,n)) = (stampaTipo t ) ^ " " ^ (stampaNomeVar n)
and
	stampaCampo ( defCampo (t, n, v )) = (stampaTipo t ) ^ " " ^ (stampaNomeCampo n) ^ " = " ^ (stampaRightValue v)
and
	stampaMetodo ( defMetodo (t,n, args, locals, commands )) = 	(stampaTipo t ) ^ " " ^ (stampaNomeMetodo n) ^ "( " ^ (stampaLista(args,"", ", ", stampaVariabile )) ^ " ) " ^
		"\n{\n" ^ (stampaLista(locals,"    ", ";\n", stampaVariabile )) ^ "\n" ^ (stampaLista(commands,"    ", ";\n", stampaComando )) ^ "\n}"
and
	stampaClasse (defClass (n, nfrom, campi, metodi ) ) = 
		(stampaNomeClasse n) ^ " extends " ^ (stampaNomeClasse nfrom) ^
		"\n{\n" ^ (stampaLista(campi,"    ", ";\n", stampaCampo )) ^ "\n" ^ (stampaLista(metodi,"    ", "\n", stampaMetodo )) ^ "\n}\n"
and
	stampaComando  (assegnamentoVar (n,v)) = (stampaNomeVar n) ^ " = " ^ (stampaRightValue v)
	| stampaComando ( assegnamentoCampo ( left , n, right )) = ( stampaRightValue left) ^ "." ^ (stampaNomeCampo n ) ^ " = " ^ (stampaRightValue right)
	| stampaComando ( return r) = "return " ^ (stampaRightValue r)	
and 
	stampaRightValue ( isvariabile n) = stampaNomeVar n 
	| stampaRightValue ( isint i) = Int.toString i
	| stampaRightValue (kw this) = "this"
	| stampaRightValue (kw super)  = "super"
	| stampaRightValue (kw null) = "null"
	| stampaRightValue (new c) = "new " ^ (stampaNomeClasse c) ^ "()"
	| stampaRightValue (accessocampo (v, nomeC c)) = (stampaRightValue v) ^ "." ^ (c)
	| stampaRightValue (chiamatametodo (v,n, args)) = (stampaRightValue v) ^ "." ^ (stampaNomeMetodo n ) ^ "( " ^ (stampaLista(args, "",  ", ", stampaRightValue )) ^" )"
and
	stampaProgramma ( codice l ) = stampaLista( l, "", "\n", stampaClasse); 

(* FUNZIONI PER STAMPARE SEMANTICA STATICA *)
fun stampaTypes (tyC s) = stampaNomeClasse( s )
	| stampaTypes ( myInt) = "int"
	| stampaTypes (T) = "T";
fun stampaVarPiu ( varNome n ) = stampaNomeVar n
	| stampaVarPiu (varThis) = "this";
fun stampaContesto ( tipiList []) = "\n"
	| stampaContesto ( tipiList( (v,t)::l)) = "( " ^ (stampaVarPiu v) ^ ":" ^ (stampaTypes t) ^ " ) ; " ^ (stampaContesto (tipiList l));



