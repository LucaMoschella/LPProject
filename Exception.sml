(********** ECCEZIONI **********)
(********** tipi **********)
exception ClassExtNotValid  of nomeClasse;

exception UnknownVar of varPiu;

exception FieldNotFound of nomeCampo;
exception MethodNotFound of nomeMetodo;
exception ClassNotFound of nomeClasse;
exception ReturnNotFound of nomeMetodo * nomeClasse;

exception WrongSemToSint;
exception TypeIsNotAClass;
exception ExpIsNotAVar;

exception TypeErrorDefField of tipoSintattico * nomeCampo * espressioneTipata * nomeClasse;
exception TypeErrorReturn of nomeMetodo * tipoSintattico * espressioneTipata * nomeClasse;
exception TypeErrorAssignVar of nomeMetodo * espressioneTipata * espressioneTipata * nomeClasse * nomeVariabile;
exception TypeErrorAssignField of nomeMetodo *espressioneTipata *espressioneTipata *espressioneTipata * nomeClasse * nomeCampo;
exception TypeErrorOverrideMismatch of nomeMetodo * tipoSintattico  * nomeClasse * tipoSintattico  * nomeClasse;

exception MultipleMetodoDef of nomeMetodo * nomeClasse;
exception MultipleCampoDef of nomeCampo * nomeClasse;
exception MultipleArgsDef of nomeVariabile * nomeClasse * nomeMetodo;
exception MultipleLocalsDef of nomeVariabile * nomeClasse * nomeMetodo;
exception MultipleClasseDef of nomeClasse;

(********** esecuzione **********)
exception RuntimeErrorVarNotFoundInEnv;
exception RuntimeErrorLocNotFoundInHeap;
exception RuntimeErrorValIsNotObj;
exception RuntimeErrorValIsNotInt;
exception RuntimeErrorInitCampoNonTrovato;