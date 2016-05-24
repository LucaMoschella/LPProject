(**************************** DISPENSE ***********************************)

val programmaStatDin = codiceS( 
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
	            		assegnamentoVarS( (nomeV "res1"), accessoCampoS( varExprS(nomeV "b"), nomeC "f")),
	            		assegnamentoVarS( (nomeV "res2"), chiamataMetodoS (varExprS(nomeV "b") , nomeM "get_f" , [])),

	            		returnS ( intExprS 0)
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
	           defCampoS ( intS, nomeC ("f"), chiamataMetodoS (( thisS) , nomeM "m" , [])) 



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
	        Object,
	        [],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[defVarS( classeS( nomeCl "B"), (nomeV "b"))], (*locals*)
	            	[ assegnamentoVarS( (nomeV "b"), newS( nomeCl "B")) ,returnS (accessoCampoS( varExprS(nomeV "b"), nomeC "f"))]) (*cmds*)
	        ]
	        )
]
);

(**************************** VARIE ***********************************)

val programmaOverride1 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [	        	
	        	defMetodoS ( intS, nomeM "get_f", [], [], [returnS (intExprS 1)])
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        ],
	        [
	        	defMetodoS ( classeS( nomeCl "A"), nomeM "get_f", [], [], [returnS (newS( nomeCl "B"))])
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a")),
	            		defVarS( classeS( nomeCl "B"), (nomeV "b")),
	            		defVarS( intS, (nomeV "resA")),
	            		defVarS( classeS( nomeCl "A"), (nomeV "resB"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "a"), newS( nomeCl "B")),
	            		assegnamentoVarS( (nomeV "b"), newS( nomeCl "B")),

	            		assegnamentoVarS( (nomeV "resA"), chiamataMetodoS (varExprS( (nomeV "a")) , nomeM "get_f" , [])),
	            		assegnamentoVarS( (nomeV "resB"), chiamataMetodoS (varExprS( (nomeV "b")) , nomeM "get_f" , [])),

	            		returnS ( intExprS 0)
					]) (*cmds*)
	        ]
	        )
]
);

val programmaOverride2 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [	        	
	        	defMetodoS ( intS, nomeM "get_f", [], [], [returnS (intExprS 1)])
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        ],
	        [
	        	defMetodoS ( classeS( nomeCl "A"), nomeM "get_f", [], [], [returnS (newS( nomeCl "B"))])
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a")),
	            		defVarS( classeS( nomeCl "B"), (nomeV "b")),
	            		defVarS( intS, (nomeV "resA")),
	            		defVarS( classeS( nomeCl "A"), (nomeV "resB"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "a"), newS( nomeCl "B")),
	            		assegnamentoVarS( (nomeV "b"), newS( nomeCl "B")),

	            		assegnamentoVarS( (nomeV "resA"), chiamataMetodoS (( varExprS(nomeV "b")) , nomeM "get_f" , [])),
	            		assegnamentoVarS( (nomeV "resB"), chiamataMetodoS (( varExprS(nomeV "a")) , nomeM "get_f" , [])),

	            		returnS ( intExprS 0)
					]) (*cmds*)
	        ]
	        )
]
);

val programmaOverride3 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [	        	
	        	defMetodoS ( intS, nomeM "get_f", [], [], [returnS (intExprS 1)])
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        ],
	        [
	        	defMetodoS ( classeS( nomeCl "A"), nomeM "get_f", [], [], [returnS (newS( nomeCl "B"))])
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a")),
	            		defVarS( classeS( nomeCl "B"), (nomeV "b")),
	            		defVarS( intS, (nomeV "resA")),
	            		defVarS( intS, (nomeV "resB"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "a"), newS( nomeCl "B")),
	            		assegnamentoVarS( (nomeV "b"), newS( nomeCl "B")),

	            		assegnamentoVarS( (nomeV "resA"), chiamataMetodoS (( varExprS(nomeV "b")) , nomeM "get_f" , [])),
	            		assegnamentoVarS( (nomeV "resB"), chiamataMetodoS (( varExprS(nomeV "a")) , nomeM "get_f" , [])),

	            		returnS ( intExprS 0)
					]) (*cmds*)
	        ]
	        )
]
);

val programmaOverride4 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [	        	
	        	defMetodoS ( intS, nomeM "get_f", [], [], [returnS (intExprS 1)])
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        ],
	        [
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a")),
	            		defVarS( classeS( nomeCl "B"), (nomeV "b")),
	            		defVarS( intS, (nomeV "resA")),
	            		defVarS( intS, (nomeV "resB"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "a"), newS( nomeCl "B")),
	            		assegnamentoVarS( (nomeV "b"), newS( nomeCl "B")),

	            		assegnamentoVarS( (nomeV "resA"), chiamataMetodoS (( varExprS(nomeV "b")) , nomeM "get_f" , [])),
	            		assegnamentoVarS( (nomeV "resB"), chiamataMetodoS (( varExprS(nomeV "a")) , nomeM "get_f" , [])),

	            		returnS ( intExprS 0)
					]) (*cmds*)
	        ]
	        )
]
);

val programmaInizializzazione1 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	            defCampoS ( intS, nomeC ("f"), intExprS 2)
	        ],
	        [
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a"))
	            	], (*locals*)
	            	[ 
	            		returnS ( accessoCampoS( varExprS( (nomeV "a")), nomeC "f"))
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaInizializzazione2 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [
	    		defMetodoS ( intS, nomeM "get_f", [], [], [returnS (intExprS 1)])
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a"))
	            	], (*locals*)
	            	[ 
	            		returnS ( chiamataMetodoS (varExprS( (nomeV "a")) , nomeM "get_f" , []) )
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaVisibilita1 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	            defCampoS ( intS, nomeC ("f"), intExprS 2)
	        ],
	        [
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "a"), newS( nomeCl "B")),
	            		returnS ( accessoCampoS(( varExprS(nomeV "a")), nomeC "f"))
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaVisibilita2 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        ],
	        [
	        	defMetodoS ( intS, nomeM "get_f", [], [], [returnS (intExprS 1)])
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "a"), newS( nomeCl "B")),
	            		returnS ( chiamataMetodoS (( varExprS(nomeV "a")) , nomeM "get_f" , []) )
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaCast1 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	            defCampoS ( intS, nomeC ("f"), intExprS 2)
	        ],
	        [
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a")),
	            		defVarS( classeS( nomeCl "B"), (nomeV "b"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "a"), newS( nomeCl "B")),
	            		assegnamentoVarS( (nomeV "b"), ( varExprS(nomeV "a"))),

	            		returnS ( accessoCampoS( ( varExprS(nomeV "a")), nomeC "f"))
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaCast2 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        ],
	        [
	        	defMetodoS ( intS, nomeM "get_f", [], [], [returnS (intExprS 1)])
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a")),
	            		defVarS( classeS( nomeCl "B"), (nomeV "b"))
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "a"), newS( nomeCl "B")),
	            		assegnamentoVarS( (nomeV "b"), ( varExprS(nomeV "a"))),

	            		returnS ( chiamataMetodoS (varExprS( (nomeV "a")) , nomeM "get_f" , []) )
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaCast3 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        ],
	        [
	        	defMetodoS ( intS, nomeM "get_f", [], [], [returnS (intExprS 1)])
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
	            		defVarS( classeS( nomeCl "A"), (nomeV "a")),
	            		defVarS( classeS( nomeCl "B"), (nomeV "b")),

	            		defVarS( intS, nomeV "res")
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( (nomeV "b"), newS( nomeCl "B")),
	            		assegnamentoVarS( ( nomeV "res"), chiamataMetodoS (varExprS((nomeV "b")) , nomeM "get_f" , [])),
	            		assegnamentoVarS( (nomeV "a"), varExprS ((nomeV "b"))),
	            		assegnamentoVarS( ( nomeV "res"), chiamataMetodoS (varExprS((nomeV "a")) , nomeM "get_f" , [])),
	            		returnS ( intExprS 0 )
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaCampo1 = codiceS( 
[
	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	           defCampoS ( intS, nomeC ("f"), intExprS 1) 



	        ],
	        [
	            defMetodoS ( intS, nomeM "m", [], [], [
	            	assegnamentoCampoS(thisS, nomeC "f", intExprS 32),
	            	assegnamentoCampoS(thisS, nomeC "f", accessoCampoS(thisS, nomeC "f")),

	            	returnS (accessoCampoS(thisS, nomeC "f"))])
	        ]
	        ),


	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[], (*locals*)
	            	[returnS (chiamataMetodoS( newS( nomeCl "A"), nomeM "m", []))]) (*cmds*)
	        ]
	        )
]
);