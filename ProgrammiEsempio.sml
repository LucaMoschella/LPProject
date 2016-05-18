val esempio = codice( [
defClass(
        nomeCl "Classe1",
        Object,
        [
            defCampo ( intero, nomeC ("a"), isint 5 ),
            defCampo ( class Object, nomeC ("b"), kw null ) 
        ],
        [


            defMetodo ( intero, nomeM "metodo1", [defvariabile (intero, nomeV "input")], [], [assegnamentoVar(nomeV "input", isint 5), return (isvariabile (nomeV "input"))]) 
            ,
           defMetodo ( class (nomeCl "Classe1"), nomeM "metodo3", [defvariabile (class (nomeCl "Classe2"), nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null),return (isvariabile (nomeV "input"))]) 

     
        ]
        )

,
defClass(
        nomeCl "Classe2",
        nomeCl "Classe1",
        [
          
            defCampo ( class (nomeCl "Classe1"), nomeC ("a"), new ( nomeCl "Classe1") ) ,
            defCampo ( intero, nomeC ("c"), isint 55 ),
            defCampo ( class (nomeCl "Classe1"), nomeC ("obj1"), new ( nomeCl "Classe1") ) 
        ],
        [
         defMetodo ( class (nomeCl "Classe1"), nomeM "metodo3", [defvariabile (class (nomeCl "Classe2"), nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return (isvariabile (nomeV "input"))]) ,
         defMetodo ( intero, nomeM "metodo3", [defvariabile (class (nomeCl "Classe1"), nomeV "input")], [], [assegnamentoVar(nomeV "input", kw null), return ( isint 5)]) 

        ]
        ) 


]
);

val esempioDispensa = codice( 
[
	defClass(
	        nomeCl "A",
	        Object,
	        [
	            defCampo ( intero, nomeC ("f"),chiamatametodo ((kw this) , nomeM "m" , []))
	        ],
	        [
	            defMetodo ( intero, nomeM "m", [], [], [return (isint 3)])
	        ]
	        ),

	defClass(
	        nomeCl "B",
	        nomeCl "A",
	        [
	            defCampo ( intero, nomeC ("g"),chiamatametodo ((kw this) , nomeM "m" , []))
	        ],
	        [
	            defMetodo ( intero, nomeM "m", [], [], [return (accessocampo( kw this, nomeC "g"))])
	        ]
	        ),

	defClass(
        nomeCl "weird",
         nomeCl "B",
        [],
        [
            defMetodo ( intero, nomeM "main", 
            	[], (*args*)
            	[defvariabile( class( nomeCl "B"), nomeV "b")], (*locals*)
            	[ assegnamentoVar( nomeV "b", new( nomeCl "B")) ,return (accessocampo( isvariabile( nomeV "b"), nomeC "f"))]) (*cmds*)
        ]
        )
]
);