(********** SISTEMA DEI TIPI **********)
datatype varPiu = varPiuNome of nomeVariabile | this;


(********** SISTEMA DI ESECUZIONE **********)
datatype locazione = buildLoc of int ;
val currentLocInt: int ref = ref 0; 
fun nextLoc () = (currentLocInt := (!currentLocInt) + 1; buildLoc (!currentLocInt));

datatype obj = 	istanza of nomeClasse * ((nomeClasse * nomeCampo * locazione) list);
datatype valore = intV of int | objV of obj | nullV | noV;


(********** DATATYPE POLIMORFO PER GESTIRE AMBIENTI, HEAP, CONTESTO E DATI VARI **********)
(* preferiamo (?) lasciare le quattro cose separate, anche se sono uguali: *)
(* danno maggior chiarezza al codice, ma essendo polimorfe, *)
(* permettono di non ripetere le funzioni! *)
datatype ('a, 'b) dataList = 	buildContesto of ('a * 'b) list (* varpiu * tiposemantico*)
							|	buildEnv of ('a * 'b) list (* varpiu * valori*)
							|	buildHeap of ('a * 'b) list (* loc * valori*)
							|	buildData of ('a * 'b) list;


(* funzione di comodo per evitare di fare troppi casi nelle fun *)
fun getCL(buildContesto data) = (buildContesto, data)
| getCL(buildEnv data) = (buildEnv, data)
| getCL(buildHeap data) = (buildHeap, data)
| getCL(buildData data) = (buildData, data);


(********** ECCEZIONI INTERNE **********)
exception KeyNotFound;
exception InternalError;


(******************** FUNZIONI POLIMORFE ********************)

(********** funzioni di check **********)
fun isEmpty( data ) = let val (x, y) = getCL(data) in y = [] end;

fun containsKeyL([], k ) = false
	| containsKeyL((a,b)::l, k) = if a = k then true else containsKeyL(l, k)
and containsKey(data, k) = let val (x, y) = getCL(data) in containsKeyL(y, k) end;

fun containsAllKeyL(y, [] ) = true
	| containsAllKeyL(y, k::l) = if not (containsKeyL( y, k)) then false else containsAllKeyL(y, l)
and containsAllKey(data, l) = let val (x, y) = getCL(data) in containsAllKeyL(y, l) end;

fun containsValueL([], v ) = false
	| containsValueL((a,b)::l, v) = if b = v then true else containsValueL(l, v)
and containsValue(data, v) = let val (x, y) = getCL(data) in containsValueL(y, v) end;

fun containsDuplicatedKeyL([] ) = false
	| containsDuplicatedKeyL((a,b)::l) = if containsKeyL(l, a) then true else containsDuplicatedKeyL(l)
and containsDuplicatedKey(data) = let val (x, y) = getCL(data) in containsDuplicatedKeyL(y) end;


(********** funzioni di get **********)
fun getList(data) = let val (x, y) = getCL(data) in y end;

fun getL([], k) = raise KeyNotFound
	| getL((a,b)::l, k) = if a = k then b else getL(l, k)
and get(data, k) = let val (x, y) = getCL(data) in getL(y, k) end;

fun findL([], k, f) = raise KeyNotFound
	| findL( (a,b)::l, k, f) = if f (a, k) then b else findL(l, k, f)
and find( data, k, f ) = let val (x, y) = getCL(data) in findL(y, k, f) end;

fun getDuplicatedKeyL( [] ) = raise KeyNotFound
	| getDuplicatedKeyL((a,b)::l) = if containsKeyL(l, a) then a else getDuplicatedKeyL(l)
and getDuplicatedKey(data) = let val (x, y) = getCL(data) in getDuplicatedKeyL(y) end;


(********** funzioni di set **********)
fun setApp([], k, v, found) = if found then [] else raise KeyNotFound
	| setApp((a,b)::l, k, v, found) = 
		if a = k then (a,v)::setApp(l, k, v, true) else (a,b)::setApp(l, k, v, found)
and setL(l, k, v) = setApp(l, k, v, false)
and set(data, k, v) = let val (x, y) = getCL(data) in x (setL(y, k, v)) end;


(********** funzioni di put **********)
fun concat(data1, data2) = 	let val (x1, y1) = getCL(data1) 
								val (x2, y2) = getCL(data2)
							in x1( y1 @ y2 ) end;

fun headAddPair(data, a) = let val (x, y) = getCL(data) in x( a::y ) end;

fun headPut(data, k, v) = let val (x, y) = getCL(data) in x( (k,v)::y ) end;

fun headPutFun(data, v, f) = let val (x,y) = f v in headPut( data, x, y) end;

fun headPutAll(data, []) = data
	| headPutAll(data, (x, y)::l) = headPut( headPutAll(data, l), x, y);

fun headPutAllFun(data, [], f) = data
	| headPutAllFun(data, a::l, f) = let val (x,y) = f a in headPut( headPutAllFun(data, l, f), x, y) end;

(**********)

fun tailAddPair(data, a) = let val (x, y) = getCL(data) in x( y@[a] ) end;

fun tailPut(data, k, v) = let val (x, y) = getCL(data) in x( y@[(k,v)] ) end;

fun tailPutFun(data, v, f) = let val (x,y) = f v in tailPut( data, x, y) end;

fun tailPutAll(data, []) = data
	| tailPutAll(data, (x,y)::l) = tailPutAll(tailPut( data, x, y), l) ;

fun tailPutAllFun(data, [], f) = data
	| tailPutAllFun(data, a::l, f) = let val (x,y) = f a in tailPutAllFun(tailPut( data, x, y), l, f) end;

(********** funzioni di remove **********)
fun removeL([], k ) =  [] 
	| removeL((a,b)::l, k) = if a = k then removeL(l, k) else (a,b)::removeL(l, k)
and remove(data, k) = let val (x, y) = getCL(data) in x (removeL(y, k)) end;



(********** FUNZIONI AUSILIARIE SU LISTE SEMPLICI **********)

(* Modifica la lista con gli elementi generati dall'applicazione di f ad ogni elemento della lista *)
fun fList( [], f) = []
	| fList( a::l, f) = f a :: fList(l, f);


(* converte una lista ricordandosi di quello che ha già convertito (lo passa alla funzione di conversione) *)
fun f2List( [], f, g, z) = []
	| f2List( a::l, f, g, z) = f (a, g (a, z)) :: f2List(l, f, g, g( a, z));


(* Modifica la lista, con la lista degli elementi generati dall'applicazione di f ad ogni elemento della lista *)
fun f3List( [], f) = []
	| f3List( a::l, f) = f a @ f3List(l, f);

fun f4List( [], f, p, e, heap) = ([], heap)
	| f4List( a::l, f, p, e, heap) = 
		let 
			val (x, y) = f(p, e, a, heap) 
			val (x2, y2) = f4List(l, f, p, e, y) 
		in (x :: x2, y2)
		end;

(* crea una nuova lista creando coppie con gli elementi generati dalla conversione delle due passate in input*)
fun f5List( [], f1, [], f2) = []
	| f5List( l1, f1, [], f2) = raise InternalError
	| f5List( [], f1, l2, f2) = raise InternalError
	| f5List( a::l1, f1, b::l2, f2) = ( f1 a, f2 b )  :: f5List(l1, f1, l2, f2); 
(*
val l1 = [(1,1),(2,2),(3,3),(4,4),(5,5)];
val l2 = [(6,6),(7,7),(8,8),(9,9),(10,10)];

val x = buildData [(1,1),(2,2),(3,3),(4,4),(5,5)];
val y = buildData [(6,6),(7,7),(8,8),(9,9),(10,10)];

headPutAll( y, l1);
*)