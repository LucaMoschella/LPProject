datatype variabile = var of string;

datatype costante = const of int | vero | falso | null;

datatype leftExpression = lexpVar of variabile;

datatype rightExpression = rexpConst of costante |
						rexpLexp of leftExpression |
						sum of rightExpression * rightExpression |
						lessThen of rightExpression * rightExpression |
						lessEq of rightExpression * rightExpression;

datatype program = skip |
				runSeq of program * program |
				ifThenElse of rightExpression * program * program |
				whileDo of rightExpression * program |
				assign of leftExpression * rightExpression | 
				declare of variabile * rightExpression * program |
				proc of variabile * variabile * program * program |
				call of program * rightExpression;

datatype metodo = metodo of variabile * program;

datatype classeDef = empty of unit | campo of variabile | met of metodo | aggiungi of classeDef * classeDef;
datatype classe = def of variabile * classeDef;

datatype oggettoDef = istanzia of classe | call of 