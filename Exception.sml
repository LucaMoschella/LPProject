(********** ECCEZIONI **********)
(********** tipi **********)
exception VarNameNotValid  of nomeVariabile;
exception ClassExtNotValid  of nomeClasse;

exception UnknownVar of varPiu;

exception FieldNotFound of nomeCampo;
exception MethodNotFound of nomeMetodo;
exception ClassNotFound of nomeClasse;
exception ReturnNotFound of nomeMetodo;

exception WrongSemToSint;
exception TypeIsNotAClass;
exception ExpIsNotAVar;

exception TypeErrorDefField of tipoSintattico * nomeCampo * espressioneTipata;
exception TypeErrorReturn of nomeMetodo * tipoSintattico * espressioneTipata;
exception TypeErrorAssignVar of nomeMetodo * espressioneTipata * espressioneTipata;
exception TypeErrorAssignField of nomeMetodo *espressioneTipata *espressioneTipata *espressioneTipata;

exception OverrideMismatch of nomeMetodo * tipoSintattico  * nomeClasse
exception MultipleMetodoDef of nomeMetodo * nomeClasse
exception MultipleCampoDef of nomeCampo * nomeClasse
exception MultipleArgsDef of nomeVariabile * nomeClasse * nomeMetodo
exception MultipleLocalsDef of nomeVariabile * nomeClasse * nomeMetodo

(********** esecuzione **********)
exception RuntimeErrorVarNotFoundInEnv;
exception RuntimeErrorLocNotFoundInHeap;
exception RuntimeErrorValIsNotObj;
exception RuntimeErrorValIsNotInt;
exception RuntimeErrorInitCampoNonTrovato;