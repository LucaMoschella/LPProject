datatype variabileSintattica = var of string;

datatype costante = const of int | vero | falso | nullS;

datatype leftExpression = lexpVar of variabileSintattica;

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
				declare of variabileSintattica * rightExpression * program |
				proc of variabileSintattica * variabileSintattica * program * program |
				call of program * rightExpression;

datatype metodoSintattico = metodoSintattico of variabileSintattica * program;

datatype classeDef = empty of unit | campoSintattico of variabileSintattica | met of metodoSintattico | aggiungi of classeDef * classeDef;
datatype classeSintattica = def of variabileSintattica * classeDef;

datatype oggettoDef = istanzia of classeSintattica | call of 