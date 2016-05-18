(*
fun cbody( programma,  nomec ) = let val ( defClass( _ , _ , campi, _ ) ) = cercaClasseInProgramma ( programma, nomec ) in campi end;


fun mbody (programma, Object, nomeM m, parametri) = raise MethodNotFound
	|mbody (programma, nomeCl c, nomeM m, parametri) = cercaMetodo(programma, cercaClasseInProgramma(programma, nomeCl c) , nomeM m, parametri)
and
	cercaMetodo(programma, defClass( n1, n2, campi, []), nomeM m, parametri) = mbody(programma, n2, nomeM m, parametri )
	| cercaMetodo(programma, defClass( n1, n2, campi, (defMetodo(t,nomeM m,args,locals,cmds))::metodi), nomeM metodo, parametri) =
			if( (m = metodo) andalso (parametriCompatibili(programma, args, parametri))) (* parametri deve contere tipi dal datatype types
				then defMetodo(t,nomeM m,args,locals,cmds)
				else cercaMetodo(programma, defClass (n1, n2 , campi , metodi), nomeM metodo, parametri );




print (stampaProgramma esempio);
print ( stampaMetodo( mbody(esempio, nomeCl "Classe1", nomeM "metodo3", [tyC(nomeCl "Classe2")] )) ^ "\n");*)

(*  REGOLE PER L'ESECUZIONE *)

(* variabile *)