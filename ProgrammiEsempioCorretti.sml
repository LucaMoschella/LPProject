(**************************** DISPENSE ***********************************)
val programmaStatDin1 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	           defCampoS ( intS, nomeC ("f"), intExprS 1) 
	        ],
	        [
	            defMetodoS ( intS, nomeM "get_f", [], [], [returnS (accessoCampoS( thisS, nomeC "f"))])
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	            defCampoS ( intS, nomeC ("f"), intExprS 2)
	        ],
	        [
	            defMetodoS ( intS, nomeM "get_f", [], [], [returnS (accessoCampoS( thisS, nomeC "f"))])
	        ]
	        ),

	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            		defVarS( classeS( nomeCl "A"), (nomeV "b")),
	            		defVarS( intS, (nomeV "res1")),
	            		defVarS( intS, (nomeV "res2"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "b"), newS( nomeCl "B")),
	            		assegnamentoVarS( (nomeV "res1"), accessoCampoS( varExprS(nomeV "b"), nomeC "f")),
	            		assegnamentoVarS( (nomeV "res2"), chiamataMetodoS (varExprS(nomeV "b") , nomeM "get_f" , [])),

	            		returnS ( varExprS((nomeV "res1") ))
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaStatDin2 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	           defCampoS ( intS, nomeC ("f"), intExprS 1) 
	        ],
	        [
	            defMetodoS ( intS, nomeM "get_f", [], [], [returnS (accessoCampoS( thisS, nomeC "f"))])
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	            defCampoS ( intS, nomeC ("f"), intExprS 2)
	        ],
	        [
	            defMetodoS ( intS, nomeM "get_f", [], [], [returnS (accessoCampoS( thisS, nomeC "f"))])
	        ]
	        ),

	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            		defVarS( classeS( nomeCl "A"), (nomeV "b")),
	            		defVarS( intS, (nomeV "res1")),
	            		defVarS( intS, (nomeV "res2"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "b"), newS( nomeCl "B")),
	            		assegnamentoVarS( (nomeV "res1"), accessoCampoS( varExprS(nomeV "b"), nomeC "f")),
	            		assegnamentoVarS( (nomeV "res2"), chiamataMetodoS (varExprS(nomeV "b") , nomeM "get_f" , [])),

	            		returnS ( varExprS((nomeV "res2") ))
	            	]) (*cmds*)
	        ]
	        )
]
);



val programmaWeird = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	           defCampoS ( intS, nomeC ("f"),  chiamataMetodoS ( thisS , nomeM "m" , []))
	        ],
	        [
	            defMetodoS ( intS, nomeM "m", [], [], [returnS (intExprS 3)])
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	            defCampoS ( intS, nomeC ("g"),  chiamataMetodoS ( thisS , nomeM "m" , []))
	        ],
	        [
	            defMetodoS ( intS, nomeM "m", [], [], [returnS (accessoCampoS (( thisS) , nomeC "g"))])
	        ]
	        ),

	defClasseS(
	        nomeCl "weird",
	        Object,
	        [],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            		defVarS( classeS( nomeCl "B"), (nomeV "b"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "b"), newS( nomeCl "B")),
	            		returnS (accessoCampoS( varExprS(nomeV "b"), nomeC "f"))
	            	]) (*cmds*)
	        ]
	        )
]
);

(**************************** Programmi corretti ***********************************)

val programmaOverrideOK = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        	defCampoS ( classeS( nomeCl "A"), nomeC ("a"),  nullS)
	        ],
	        [	       
	        	defMetodoS ( classeS( Object ), nomeM "set", [defVarS( classeS( nomeCl "A"), (nomeV "a"))], [], 
	        		[
	        			returnS (nullS)
	        		])	        
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        	defCampoS ( classeS( nomeCl "A"), nomeC ("b"),  nullS)
	        ],
	        [
	        	defMetodoS ( classeS( nomeCl "A"), nomeM "set", [defVarS( classeS( nomeCl "A"), (nomeV "b"))], [], 
	        		[
	        			assegnamentoCampoS(thisS, nomeC "b", varExprS (nomeV "b")),
	        			returnS (thisS)
	        		])	        
	        ]
	        ),

	defClasseS(
	        nomeCl "C",
	        nomeCl "B",
	        [
	        	defCampoS ( classeS( nomeCl "C"), nomeC ("c"),  nullS)
	        ],
	        [
	        	defMetodoS ( classeS( nomeCl "C"), nomeM "set", [defVarS( classeS( nomeCl "C"), (nomeV "c"))], [], 
	        		[
	        			returnS (nullS)
	        		])
	        ]
	        ),

	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [],
	        [
	            defMetodoS ( classeS(Object), nomeM "main", 
	            	[	           ], 
	            	[
	            		defVarS( classeS( nomeCl "A"), (nomeV "a")),
	            		defVarS( classeS( Object ), (nomeV "resA"))
	            	], 
	            	[ 
	            		assegnamentoVarS( (nomeV "a"), newS( nomeCl "C")),

	            		assegnamentoVarS( (nomeV "resA"), chiamataMetodoS (varExprS( (nomeV "a")) , nomeM "set" , [newS( nomeCl "C")])),

	            		returnS (varExprS (nomeV "resA"))
					]) 
	        ]
	        )

]
);

val programmaSuperOK = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        	defCampoS (intS, nomeC ("a"),  intExprS 17)
	        ],
	        [	       
	        	defMetodoS ( classeS( nomeCl "A"), nomeM "get", [], [], 
	        		[
	        			returnS (newS( nomeCl "A"))
	        		])	        
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        	defCampoS (intS, nomeC ("b"),  intExprS 20)
	        ],
	        [
	        	defMetodoS ( classeS( nomeCl "A"), nomeM "get", [], [], 
	        		[
	        			returnS (chiamataMetodoS (superS, nomeM "get" , []))
	        		])	        
	        ]
	        ),

	defClasseS(
	        nomeCl "C",
	        nomeCl "B",
	        [
	        	defCampoS (intS, nomeC ("c"),  intExprS 23)
	        ],
	        [
	        	defMetodoS ( classeS( nomeCl "A"), nomeM "get", [], [], 
	        		[
	        			returnS (chiamataMetodoS (superS, nomeM "get" , []))
	        		])
	        ]
	        ),

	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [],
	        [
	            defMetodoS ( classeS( nomeCl "A"), nomeM "main", [], 
	            	[
	            		defVarS( classeS( nomeCl "C"), (nomeV "c")),
	            		defVarS( classeS( nomeCl "A"), (nomeV "resA"))
	            	], 
	            	[ 
	            		assegnamentoVarS( (nomeV "c"), newS( nomeCl "C")),

	            		assegnamentoVarS( (nomeV "resA"), chiamataMetodoS (varExprS( (nomeV "c")) , nomeM "get" , [])),

	            		returnS (varExprS (nomeV "resA"))
					]) 
	        ]
	        )

]
);

val programmaEredOK = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        	defCampoS (intS, nomeC ("x"),  intExprS 1)
	        ],
	        [	       
     
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        	defCampoS (intS, nomeC ("x"),  intExprS 2)
	        ],
	        [
	        	defMetodoS ( intS, nomeM "get", [], [], 
	        		[
	        			returnS (accessoCampoS (superS, nomeC "x" ))
	        		])	        
	        ]
	        ),

	defClasseS(
	        nomeCl "C",
	        nomeCl "B",
	        [
	        	defCampoS (intS, nomeC ("x"),  intExprS 3)
	        ],
	        [
	        ]
	        ),

	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [],
	        [
	            defMetodoS ( intS, nomeM "main", [], 
	            	[
	            	], 
	            	[ 
	            		returnS (chiamataMetodoS ( newS( nomeCl "C") , nomeM "get" , []))
					]) 
	        ]
	        )

]
);