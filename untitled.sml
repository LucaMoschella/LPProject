(*
fun cbody( programmaSintattico,  nomec ) = let val ( defClasseS( _ , _ , campi, _ ) ) = cercaClasseInProgramma ( programmaSintattico, nomec ) in campi end;


fun mbody (programmaSintattico, Object, nomeM m, parametri) = raise MethodNotFound
	|mbody (programmaSintattico, nomeCl c, nomeM m, parametri) = cercaMetodo(programmaSintattico, cercaClasseInProgramma(programmaSintattico, nomeCl c) , nomeM m, parametri)
and
	cercaMetodo(programmaSintattico, defClasseS( n1, n2, campi, []), nomeM m, parametri) = mbody(programmaSintattico, n2, nomeM m, parametri )
	| cercaMetodo(programmaSintattico, defClasseS( n1, n2, campi, (defMetodoS(t,nomeM m,args,locals,cmds))::metodi), nomeM metodoSintattico, parametri) =
			if( (m = metodoSintattico) andalso (parametriCompatibili(programmaSintattico, args, parametri))) (* parametri deve contere tipi dal datatype types
				then defMetodoS(t,nomeM m,args,locals,cmds)
				else cercaMetodo(programmaSintattico, defClasseS (n1, n2 , campi , metodi), nomeM metodoSintattico, parametri );




print (stampaProgramma esempio);
print ( stampaMetodo( mbody(esempio, nomeCl "Classe1", nomeM "metodo3", [classeT(nomeCl "Classe2")] )) ^ "\n");*)

(*  REGOLE PER L'ESECUZIONE *)

(* variabileSintattica *)

fun initCampiApp( programmaSintattico, obj, istanza( nomec, []),  buildHeap h  ) =  buildHeap h  

	| initCampiApp( programmaSintattico, obj, istanza( nomec, (classecampo, nomecampo, loccampo)::l), buildHeap h ) = 
		initCampiApp( programmaSintattico, 
			obj, 
			istanza( nomec, l), 
			changeHeap( buildHeap h ,
						loccampo , 
						regolaRightExpr( programmaSintattico, 
										buildEnv [(thisT, objV obj )],
										cercaInitCampo(programmaSintattico, nomecampo, classecampo), 
										buildHeap h) ) )
		

and initCampi( programmaSintattico, obj, mem ) = initCampiApp( programmaSintattico, obj, obj, mem);