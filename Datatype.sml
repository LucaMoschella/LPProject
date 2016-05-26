(********** SISTEMA DEI TIPI **********)
datatype varPiu = varNome of nomeVariabile | this;


(********** SISTEMA DI ESECUZIONE **********)
datatype locazione = buildLoc of int ;
val currentLocInt: int ref = ref 0; 
fun nextLoc () = (currentLocInt := (!currentLocInt) + 1; buildLoc (!currentLocInt));

datatype obj = 	istanza of nomeClasse * ((nomeClasse * nomeCampo * locazione) list);
datatype valore = intV of int | objV of obj | nullV | noV;


(********** DATATYPE POLIMORFO PER GESTIRE AMBIENTI, HEAP E CONTESTO **********)
(* preferiamo lasciare le tre cose separate, anche se sono uguali: *)
(* danno maggior chiarezza al codice, ma essendo polimorfe, *)
(* permettono di non ripetere le funzioni! *)
datatype ('a, 'b) dataList = 	buildContesto of ('a * 'b) list 
							|	buildEnv of ('a * 'b) list 
							|	buildHeap of ('a * 'b) list;


(* funzione di comodo per evitare di fare troppi casi nelle fun *)
fun getCL(buildContesto data) = (buildContesto, data)
| getCL(buildEnv data) = (buildEnv, data)
| getCL(buildHeap data) = (buildHeap, data);


(********** ECCEZIONI INTERNE **********)
exception KeyNotFound;


(********** FUNZIONI POLIMORFE **********)
fun concat(data1, data2) = 	let val (x1, y1) = getCL(data1) 
								val (x2, y2) = getCL(data2)
							in x1( y1 @ y2 ) end;


fun containsKeyL([], k ) = false
	| containsKeyL((a,b)::l, k) = if a = k then true else containsKeyL(l, k)
and containsKey(data, k) = let val (x, y) = getCL(data) in containsKeyL(y, k) end;


fun containsValueL([], v ) = false
	| containsValueL((a,b)::l, v) = if b = v then true else containsValueL(l, v)
and containsValue(data, v) = let val (x, y) = getCL(data) in containsValueL(y, v) end;


fun getL([], k) = raise KeyNotFound
	| getL((a,b)::l, k) =	if a = k then b else getL(l, k)
and get(data, k) = let val (x, y) = getCL(data) in getL(y, k) end;


fun isEmpty( data ) = let val (x, y) = getCL(data) in y = [] end;


fun put(data, k, v) = let val (x, y) = getCL(data) in x( (k,v)::y ) end;


fun putAll(data, l) = let val (x, y) = getCL(data) in x( l @ y ) end;


fun setApp([], k, v, found) = if found then [] else raise KeyNotFound
	| setApp((a,b)::l, k, v, found) = 
		if a = k then (a,v)::setApp(l, k, v, true) else (a,b)::setApp(l, k, v, found)
and setL(l, k, v) = setApp(l, k, v, false)
and set(data, k, v) = let val (x, y) = getCL(data) in setL(y, k, v) end;


fun removeL([], k ) =  [] 
	| removeL((a,b)::l, k) = if a = k then removeL(l, k) else (a,b)::removeL(l, k)
and remove(data, k) = let val (x, y) = getCL(data) in removeL(y, k) end;