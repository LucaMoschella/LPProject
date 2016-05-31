(********** ECCEZIONI **********)
(********** tipi **********)
exception ClassExtNotValid  of nomeClasse;

exception UnknownVar of nomeVariabile;
exception UnknownVarInMetodo of nomeVariabile * nomeClasse * nomeMetodo;
exception UnknownVarInClasse of nomeVariabile * nomeClasse;

exception VarNotInitialized of nomeVariabile;
exception VarNotInitializedInMetodo of nomeVariabile * nomeClasse * nomeMetodo;
exception CampoNotDef of nomeClasse * nomeCampo

exception FieldNotFound of nomeCampo * nomeClasse;
exception MethodNotFound of nomeMetodo * nomeClasse;
exception ClassExtNotFound of nomeClasse * nomeClasse;
exception ReturnNotFound of nomeMetodo * nomeClasse;

exception WrongSemToSint; (* internal error *)

exception TypeIsNotAClass;
exception TypeIsNotAClassInMetodo of nomeMetodo * nomeClasse;
exception TypeIsNotAClassInClasse of nomeClasse;

exception TypeIsNotAClassCampo of tipoSintattico * nomeCampo * nomeClasse;
exception TypeIsNotAClassMetodo of tipoSintattico * nomeMetodo * nomeClasse
exception TypeIsNotAClassArgs of tipoSintattico * nomeVariabile * nomeMetodo * nomeClasse;
exception TypeIsNotAClassLocals of tipoSintattico * nomeVariabile * nomeMetodo * nomeClasse;

exception TypeIsNotAClassNew of nomeClasse;
exception TypeIsNotAClassNewInMetodo of nomeClasse * nomeClasse * nomeMetodo;
exception TypeIsNotAClassNewInClasse of nomeClasse * nomeClasse;

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
exception RuntimeError;
exception MissingMain;