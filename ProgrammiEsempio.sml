val esempio = codice( [
defClass(
        nomeCl "Classe1",
        Object,
        [
            defCampo ( intero, nomeC ("a"), isint 5 ),
            defCampo ( class Object, nomeC ("b"), kw null ) 
        ],
        [
            defMetodo ( intero, nomeM "metodo1", [defvariabile (intero, nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return (isvariabile (nomeV "input"))]) ,
            defMetodo ( class (nomeCl "Classe1"), nomeM "metodo2", [defvariabile (intero, nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return (isvariabile (nomeV "input"))]) 
        ]
        ),
defClass(
        nomeCl "Classe2",
        nomeCl "Classe1",
        [
          
            defCampo ( class (nomeCl "Classe1"), nomeC ("a"), new ( nomeCl "Classe1") ) ,
            defCampo ( intero, nomeC ("c"), isint 55 ),
            defCampo ( class (nomeCl "Classe1"), nomeC ("obj1"), new ( nomeCl "Classe1") ) 
        ],
        [
         defMetodo ( class (nomeCl "Classe1"), nomeM "metodo3", [defvariabile (intero, nomeV "input"),defvariabile (intero, nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return (isvariabile (nomeV "input"))]) ,
         defMetodo ( intero, nomeM "metodo3", [defvariabile (class (nomeCl "Classe1"), nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return (isvariabile (nomeV "input"))]) ,
        defMetodo ( class (nomeCl "Classe2"), nomeM "metodo4", [defvariabile (intero, nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return (isvariabile (nomeV "input"))]) 

        ]
        ) 
]
);