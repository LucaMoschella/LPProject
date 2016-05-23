val esempio = codiceS( [
defClasseS(
        nomeCl "Classe1",
        Object,
        [
            defCampoS ( intS, nomeC ("a"), intExprS 5 ),
            defCampoS ( classeS Object, nomeC ("b"),  nullS ) 
        ],
        [


            defMetodoS ( intS, nomeM "metodo1", [defVarS (intS, nomeV "input")], [], [assegnamentoVarS(nomeV "input", intExprS 5), returnS (varExprS (nomeV "input"))]) 
            ,
           defMetodoS ( classeS (nomeCl "Classe1"), nomeM "metodo3", [defVarS (classeS (nomeCl "Classe2"), nomeV "input")], [], [assegnamentoVarS(nomeV "input",  nullS),returnS (varExprS (nomeV "input"))]) 

     
        ]
        )

,
defClasseS(
        nomeCl "Classe2",
        nomeCl "Classe1",
        [
          
            defCampoS ( classeS (nomeCl "Classe1"), nomeC ("a"), newS ( nomeCl "Classe1") ) ,
            defCampoS ( intS, nomeC ("c"), intExprS 55 ),
            defCampoS ( classeS (nomeCl "Classe1"), nomeC ("obj1"), newS ( nomeCl "Classe1") ) 
        ],
        [
         defMetodoS ( classeS (nomeCl "Classe1"), nomeM "metodo3", [defVarS (classeS (nomeCl "Classe2"), nomeV "input")], [], [assegnamentoVarS(nomeV "input",  nullS), returnS (varExprS (nomeV "input"))]) ,
         defMetodoS ( intS, nomeM "metodo3", [defVarS (classeS (nomeCl "Classe1"), nomeV "input")], [], [assegnamentoVarS(nomeV "input",  nullS), returnS ( intExprS 5)]) 

        ]
        ) 


]
);

val esempioDispensa = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        (*    defCampoS ( intS, nomeC ("f"),chiamataMetodoS (( thisS) , nomeM "m" , [])) *)
	        	    defCampoS ( classeS(nomeCl "A" ), nomeC ("f"),  thisS)



	        ],
	        [
	            defMetodoS ( intS, nomeM "m", [], [], [returnS (intExprS 3)])
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	            defCampoS ( intS, nomeC ("g"),chiamataMetodoS (( thisS) , nomeM "m" , []))
	        ],
	        [
	            defMetodoS ( intS, nomeM "m", [], [], [returnS (accessoCampoS(  thisS, nomeC "g"))])
	        ]
	        ),

	defClasseS(
        nomeCl "weird",
         nomeCl "B",
        [ defCampoS ( intS, nomeC ("g"), intExprS 42)],
        [
            defMetodoS ( intS, nomeM "main", 
            	[	           ], (*args*)
            	[defVarS( classeS( nomeCl "B"), nomeV "b")], (*locals*)
            	[ assegnamentoVarS( nomeV "b", newS( nomeCl "B")) ,returnS (accessoCampoS( varExprS( nomeV "b"), nomeC "f"))]) (*cmds*)
        ]
        )
]
);