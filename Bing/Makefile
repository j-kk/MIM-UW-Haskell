# Makefile generated by BNFC.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : Interpreter

# Rules for building the parser.

AbsBing.hs LexBing.x ParBing.y PrintBing.hs TestBing.hs Interpreter.hs Logic.hs : Bing.cf
	bnfc --haskell Bing.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

TestBing : AbsBing.hs LexBing.hs ParBing.hs PrintBing.hs TestBing.hs Commons.hs TypeChecking.hs
	${GHC} ${GHC_OPTS} $@

Interpreter : AbsBing.hs LexBing.hs ParBing.hs PrintBing.hs Logic.hs Interpreter.hs Commons.hs TypeChecking.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsBing.hs AbsBing.hs.bak ComposOp.hs ComposOp.hs.bak DocBing.txt DocBing.txt.bak ErrM.hs ErrM.hs.bak LayoutBing.hs LayoutBing.hs.bak LexBing.x LexBing.x.bak ParBing.y ParBing.y.bak PrintBing.hs PrintBing.hs.bak SkelBing.hs SkelBing.hs.bak TestBing.hs TestBing.hs.bak XMLBing.hs XMLBing.hs.bak ASTBing.agda ASTBing.agda.bak ParserBing.agda ParserBing.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak Bing.dtd Bing.dtd.bak TestBing Interpreter LexBing.hs ParBing.hs ParBing.info ParDataBing.hs


# EOF
