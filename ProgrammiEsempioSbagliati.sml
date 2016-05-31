val programmaClassExtNotValid = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        nomeCl "esempio",
	        [],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS ( intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaUnknownVarInMetodo = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS ( varExprS(nomeV "unkownvar"))
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaUnknownVarInClasse = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        	defCampoS( intS, nomeC "c", varExprS(nomeV "unkownvar"))
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaVarNotInitializedInMetodo = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            		defVarS( intS, nomeV "v")
	            	], (*locals*)
	            	[ 
		            	returnS (varExprS( nomeV "v"))
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaCampoNotDef = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        	defCampoS( intS, nomeC "c", accessoCampoS(thisS, nomeC "next")),
	        	defCampoS( intS, nomeC "next", intExprS 0)

	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaFieldNotFound = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (accessoCampoS(thisS, nomeC "c"))
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaMethodNotFound = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (chiamataMetodoS(thisS, nomeM "m", []))
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaClassExtNotFound = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        nomeCl "whatclass",
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaReturnNotFound = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeIsNotAClassInMetodo = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
	            		returnS (chiamataMetodoS(intExprS 0, nomeM "m", []))
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeIsNotAClassInClasse = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        	defCampoS( intS, nomeC "c", accessoCampoS(intExprS 0, nomeC "o"))
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);


val programmaTypeIsNotAClassCampo = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        	defCampoS( classeS(nomeCl "whatclass"), nomeC "c", nullS)
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeIsNotAClassMetodo = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( classeS(nomeCl "whatclass"), nomeM "main", 
	            	[	           ], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeIsNotAClassArgs = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            		defVarS( classeS(nomeCl "whatclass"), nomeV "a" )
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeIsNotAClassLocals = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            		defVarS( classeS(nomeCl "whatclass"), nomeV "a" )	            	
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeIsNotAClassNewInMetodo = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (newS( nomeCl "whatclass"))
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeIsNotAClassNewInClasse = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        	defCampoS( intS, nomeC "c", newS( nomeCl "whatclass") )
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 9)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeErrorDefField = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        	defCampoS( intS, nomeC "c", nullS )
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 9)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeErrorReturn = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (nullS)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeErrorAssignVar = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            		defVarS( intS, nomeV "c")
	            	], (*locals*)
	            	[ 
	            		assegnamentoVarS( nomeV "c", nullS),
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeErrorAssignField = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        	defCampoS( intS, nomeC "c", intExprS 0 )
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
	            		assegnamentoCampoS(thisS, nomeC "c", nullS),
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaTypeErrorOverrideMismatch = codiceS( 
[

	defClasseS(
	        nomeCl "A",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "m", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        ),

	defClasseS(
	        nomeCl "B",
	        nomeCl "A",
	        [
	        ],
	        [
	            defMetodoS ( classeS Object, nomeM "m", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (nullS)
	            	]) (*cmds*)
	        ]
	        ),

	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        	defCampoS( intS, nomeC "c", intExprS 0 )
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);


val programmaMultipleClasseDef = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	        ]
	        ),

	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaMultipleCampoDef = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        	defCampoS( intS, nomeC "c", intExprS 0 ),
	        	defCampoS( classeS(nomeCl "esempio"), nomeC "c", intExprS 0 )
	        ],
	        [
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaMultipleMetodoDef = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [
	        	defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]), (*cmds*)
	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaMultipleArgsDef = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [

	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            		defVarS(intS, nomeV "a"),
	            		defVarS(classeS Object, nomeV "a")
	            	], (*args*)
	            	[
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaMultipleLocalsDef = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [

	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            		
	            	], (*args*)
	            	[
	            		defVarS(intS, nomeV "a"),
	            		defVarS(classeS Object, nomeV "a")
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaMultipleLocalsArgsDef = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [

	            defMetodoS ( intS, nomeM "main", 
	            	[	    
	            		defVarS(classeS Object, nomeV "a")
	            	], (*args*)
	            	[
	            		defVarS(intS, nomeV "a")
	            	], (*locals*)
	            	[ 
		            	returnS (intExprS 0)
	            	]) (*cmds*)
	        ]
	        )
]
);

val programmaMissingMain = codiceS( 
[
	defClasseS(
	        nomeCl "esempio",
	        Object,
	        [
	        ],
	        [      
	        ]
	        )
]
);