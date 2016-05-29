(********** ECCEZIONI **********)
(********** tipi **********)
exception ClassExtNotValid  of nomeClasse;

exception UnknownVar of varPiu;
exception UnknownVarInMetod of varPiu * nomeClasse * nomeMetodo;
exception VarNotInitialized of nomeVariabile;
exception VarNotInitializedInMetod of nomeVariabile * nomeClasse * nomeMetodo;

exception FieldNotFound of nomeCampo;
exception MethodNotFound of nomeMetodo;
exception ClassNotFound of nomeClasse;
exception ReturnNotFound of nomeMetodo * nomeClasse;

exception WrongSemToSint;
exception TypeIsNotAClass;

exception TypeErrorDefField of tipoSintattico * nomeCampo * espressioneTipata * nomeClasse;
exception TypeErrorReturn of nomeMetodo * tipoSintattico * espressioneTipata * nomeClasse;
exception TypeErrorAssignVar of nomeMetodo * espressioneTipata * espressioneTipata * nomeClasse * nomeVariabile;
exception TypeErrorAssignField of nomeMetodo * espressioneTipata * espressioneTipata * espressioneTipata * nomeClasse * nomeCampo;
exception TypeErrorOverrideMismatch of nomeMetodo * tipoSintattico  * nomeClasse * tipoSintattico  * nomeClasse;

exception MultipleClasseDef of nomeClasse;
exception MultipleCampoDef of nomeCampo * nomeClasse;
exception MultipleMetodoDef of nomeMetodo * nomeClasse;
exception MultipleArgsDef of nomeVariabile * nomeClasse * nomeMetodo;
exception MultipleLocalsDef of nomeVariabile * nomeClasse * nomeMetodo;
exception MultipleLocalsArgsDef of nomeVariabile * nomeClasse * nomeMetodo;

(********** esecuzione **********)
exception RuntimeError
exception RuntimeErrorVarNotFoundInEnv;
exception RuntimeErrorLocNotFoundInHeap;
exception RuntimeErrorValIsNotObj;
exception RuntimeErrorValIsNotInt;
exception RuntimeErrorInitCampoNonTrovato;