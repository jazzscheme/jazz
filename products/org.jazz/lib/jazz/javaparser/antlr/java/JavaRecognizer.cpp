/* $ANTLR 2.7.1: "java.g" -> "JavaRecognizer.cpp"$ */
#include "JavaRecognizer.hpp"
#include "antlr/NoViableAltException.hpp"
#include "antlr/SemanticException.hpp"
#line 1 "java.g"

#line 8 "JavaRecognizer.cpp"
JavaRecognizer::JavaRecognizer(ANTLR_USE_NAMESPACE(antlr)TokenBuffer& tokenBuf, int k)
: ANTLR_USE_NAMESPACE(antlr)LLkParser(tokenBuf,k)
{
	setTokenNames(_tokenNames);
}

JavaRecognizer::JavaRecognizer(ANTLR_USE_NAMESPACE(antlr)TokenBuffer& tokenBuf)
: ANTLR_USE_NAMESPACE(antlr)LLkParser(tokenBuf,2)
{
	setTokenNames(_tokenNames);
}

JavaRecognizer::JavaRecognizer(ANTLR_USE_NAMESPACE(antlr)TokenStream& lexer, int k)
: ANTLR_USE_NAMESPACE(antlr)LLkParser(lexer,k)
{
	setTokenNames(_tokenNames);
}

JavaRecognizer::JavaRecognizer(ANTLR_USE_NAMESPACE(antlr)TokenStream& lexer)
: ANTLR_USE_NAMESPACE(antlr)LLkParser(lexer,2)
{
	setTokenNames(_tokenNames);
}

JavaRecognizer::JavaRecognizer(const ANTLR_USE_NAMESPACE(antlr)ParserSharedInputState& state)
: ANTLR_USE_NAMESPACE(antlr)LLkParser(state,2)
{
	setTokenNames(_tokenNames);
}

void JavaRecognizer::compilationUnit() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST compilationUnit_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case LITERAL_package:
		{
			packageDefinition();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
		case FINAL:
		case ABSTRACT:
		case STRICTFP:
		case SEMI:
		case LITERAL_import:
		case LITERAL_private:
		case LITERAL_public:
		case LITERAL_protected:
		case LITERAL_static:
		case LITERAL_transient:
		case LITERAL_native:
		case LITERAL_threadsafe:
		case LITERAL_synchronized:
		case LITERAL_volatile:
		case LITERAL_class:
		case LITERAL_interface:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		{
		for (;;) {
			if ((LA(1)==LITERAL_import)) {
				importDefinition();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop4;
			}
			
		}
		_loop4:;
		}
		{
		for (;;) {
			if ((_tokenSet_0.member(LA(1)))) {
				typeDefinition();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop6;
			}
			
		}
		_loop6:;
		}
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp1_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp1_AST = astFactory.create(LT(1));
		match(ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE);
		compilationUnit_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_1);
		} else {
			throw ex;
		}
	}
	returnAST = compilationUnit_AST;
}

void JavaRecognizer::packageDefinition() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST packageDefinition_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  p = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST p_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		p = LT(1);
		if (inputState->guessing==0) {
			p_AST = astFactory.create(p);
			astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(p_AST));
		}
		match(LITERAL_package);
		if ( inputState->guessing==0 ) {
#line 112 "java.g"
			p_AST->setType(PACKAGE_DEF);
#line 145 "JavaRecognizer.cpp"
		}
		identifier();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp2_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp2_AST = astFactory.create(LT(1));
		match(SEMI);
		packageDefinition_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_2);
		} else {
			throw ex;
		}
	}
	returnAST = packageDefinition_AST;
}

void JavaRecognizer::importDefinition() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST importDefinition_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  i = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST i_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		i = LT(1);
		if (inputState->guessing==0) {
			i_AST = astFactory.create(i);
			astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(i_AST));
		}
		match(LITERAL_import);
		if ( inputState->guessing==0 ) {
#line 119 "java.g"
			i_AST->setType(IMPORT);
#line 185 "JavaRecognizer.cpp"
		}
		identifierStar();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp3_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp3_AST = astFactory.create(LT(1));
		match(SEMI);
		importDefinition_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_2);
		} else {
			throw ex;
		}
	}
	returnAST = importDefinition_AST;
}

void JavaRecognizer::typeDefinition() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST typeDefinition_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST m_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case FINAL:
		case ABSTRACT:
		case STRICTFP:
		case LITERAL_private:
		case LITERAL_public:
		case LITERAL_protected:
		case LITERAL_static:
		case LITERAL_transient:
		case LITERAL_native:
		case LITERAL_threadsafe:
		case LITERAL_synchronized:
		case LITERAL_volatile:
		case LITERAL_class:
		case LITERAL_interface:
		{
			modifiers();
			if (inputState->guessing==0) {
				m_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
			}
			{
			switch ( LA(1)) {
			case LITERAL_class:
			{
				classDefinition(m_AST);
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
				break;
			}
			case LITERAL_interface:
			{
				interfaceDefinition(m_AST);
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			typeDefinition_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case SEMI:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp4_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp4_AST = astFactory.create(LT(1));
			match(SEMI);
			typeDefinition_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_3);
		} else {
			throw ex;
		}
	}
	returnAST = typeDefinition_AST;
}

void JavaRecognizer::identifier() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST identifier_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp5_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp5_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp5_AST));
		}
		match(IDENT);
		{
		for (;;) {
			if ((LA(1)==DOT)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp6_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp6_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp6_AST));
				}
				match(DOT);
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp7_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp7_AST = astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp7_AST));
				}
				match(IDENT);
			}
			else {
				goto _loop26;
			}
			
		}
		_loop26:;
		}
		identifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_4);
		} else {
			throw ex;
		}
	}
	returnAST = identifier_AST;
}

void JavaRecognizer::identifierStar() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST identifierStar_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp8_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp8_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp8_AST));
		}
		match(IDENT);
		{
		for (;;) {
			if ((LA(1)==DOT) && (LA(2)==IDENT)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp9_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp9_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp9_AST));
				}
				match(DOT);
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp10_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp10_AST = astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp10_AST));
				}
				match(IDENT);
			}
			else {
				goto _loop29;
			}
			
		}
		_loop29:;
		}
		{
		switch ( LA(1)) {
		case DOT:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp11_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp11_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp11_AST));
			}
			match(DOT);
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp12_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp12_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp12_AST));
			}
			match(STAR);
			break;
		}
		case SEMI:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		identifierStar_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_5);
		} else {
			throw ex;
		}
	}
	returnAST = identifierStar_AST;
}

void JavaRecognizer::modifiers() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST modifiers_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				modifier();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop14;
			}
			
		}
		_loop14:;
		}
		if ( inputState->guessing==0 ) {
			modifiers_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 146 "java.g"
			modifiers_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(MODIFIERS,"MODIFIERS"))->add(modifiers_AST)));
#line 439 "JavaRecognizer.cpp"
			currentAST.root = modifiers_AST;
			if ( modifiers_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				modifiers_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = modifiers_AST->getFirstChild();
			else
				currentAST.child = modifiers_AST;
			currentAST.advanceChildToEnd();
		}
		modifiers_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_7);
		} else {
			throw ex;
		}
	}
	returnAST = modifiers_AST;
}

void JavaRecognizer::classDefinition(
	ANTLR_USE_NAMESPACE(antlr)RefAST modifiers
) {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST classDefinition_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST sc_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST ic_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST cb_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp13_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp13_AST = astFactory.create(LT(1));
		}
		match(LITERAL_class);
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp14_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp14_AST = astFactory.create(LT(1));
		}
		match(IDENT);
		superClassClause();
		if (inputState->guessing==0) {
			sc_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		implementsClause();
		if (inputState->guessing==0) {
			ic_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		classBlock();
		if (inputState->guessing==0) {
			cb_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		if ( inputState->guessing==0 ) {
			classDefinition_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 240 "java.g"
			classDefinition_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(6))->add(astFactory.create(CLASS_DEF,"CLASS_DEF"))->add(modifiers)->add(tmp14_AST)->add(sc_AST)->add(ic_AST)->add(cb_AST)));
#line 499 "JavaRecognizer.cpp"
			currentAST.root = classDefinition_AST;
			if ( classDefinition_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				classDefinition_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = classDefinition_AST->getFirstChild();
			else
				currentAST.child = classDefinition_AST;
			currentAST.advanceChildToEnd();
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_8);
		} else {
			throw ex;
		}
	}
	returnAST = classDefinition_AST;
}

void JavaRecognizer::interfaceDefinition(
	ANTLR_USE_NAMESPACE(antlr)RefAST modifiers
) {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST interfaceDefinition_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST ie_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST cb_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp15_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp15_AST = astFactory.create(LT(1));
		}
		match(LITERAL_interface);
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp16_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp16_AST = astFactory.create(LT(1));
		}
		match(IDENT);
		interfaceExtends();
		if (inputState->guessing==0) {
			ie_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		classBlock();
		if (inputState->guessing==0) {
			cb_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		if ( inputState->guessing==0 ) {
			interfaceDefinition_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 256 "java.g"
			interfaceDefinition_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(5))->add(astFactory.create(INTERFACE_DEF,"INTERFACE_DEF"))->add(modifiers)->add(tmp16_AST)->add(ie_AST)->add(cb_AST)));
#line 553 "JavaRecognizer.cpp"
			currentAST.root = interfaceDefinition_AST;
			if ( interfaceDefinition_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				interfaceDefinition_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = interfaceDefinition_AST->getFirstChild();
			else
				currentAST.child = interfaceDefinition_AST;
			currentAST.advanceChildToEnd();
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_9);
		} else {
			throw ex;
		}
	}
	returnAST = interfaceDefinition_AST;
}

/** A declaration is the creation of a reference or primitive-type variable
 *  Create a separate Type/Var tree for each var in the var list.
 */
void JavaRecognizer::declaration() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST declaration_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST m_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST t_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST v_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		modifiers();
		if (inputState->guessing==0) {
			m_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		typeSpec(false);
		if (inputState->guessing==0) {
			t_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		variableDefinitions(m_AST,t_AST);
		if (inputState->guessing==0) {
			v_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		if ( inputState->guessing==0 ) {
			declaration_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 137 "java.g"
			declaration_AST = v_AST;
#line 603 "JavaRecognizer.cpp"
			currentAST.root = declaration_AST;
			if ( declaration_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				declaration_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = declaration_AST->getFirstChild();
			else
				currentAST.child = declaration_AST;
			currentAST.advanceChildToEnd();
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_5);
		} else {
			throw ex;
		}
	}
	returnAST = declaration_AST;
}

void JavaRecognizer::typeSpec(
	bool addImagNode
) {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST typeSpec_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case IDENT:
		{
			classTypeSpec(addImagNode);
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			typeSpec_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		{
			builtInTypeSpec(addImagNode);
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			typeSpec_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_10);
		} else {
			throw ex;
		}
	}
	returnAST = typeSpec_AST;
}

void JavaRecognizer::variableDefinitions(
	ANTLR_USE_NAMESPACE(antlr)RefAST mods, ANTLR_USE_NAMESPACE(antlr)RefAST t
) {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST variableDefinitions_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		variableDeclarator(getASTFactory().dupTree(mods),
						   getASTFactory().dupTree(t));
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==COMMA)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp17_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp17_AST = astFactory.create(LT(1));
				match(COMMA);
				variableDeclarator(getASTFactory().dupTree(mods),
							   getASTFactory().dupTree(t));
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop54;
			}
			
		}
		_loop54:;
		}
		variableDefinitions_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_5);
		} else {
			throw ex;
		}
	}
	returnAST = variableDefinitions_AST;
}

void JavaRecognizer::modifier() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST modifier_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case LITERAL_private:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp18_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp18_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp18_AST));
			}
			match(LITERAL_private);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_public:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp19_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp19_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp19_AST));
			}
			match(LITERAL_public);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_protected:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp20_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp20_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp20_AST));
			}
			match(LITERAL_protected);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_static:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp21_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp21_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp21_AST));
			}
			match(LITERAL_static);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_transient:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp22_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp22_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp22_AST));
			}
			match(LITERAL_transient);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case FINAL:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp23_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp23_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp23_AST));
			}
			match(FINAL);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case ABSTRACT:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp24_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp24_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp24_AST));
			}
			match(ABSTRACT);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_native:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp25_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp25_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp25_AST));
			}
			match(LITERAL_native);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_threadsafe:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp26_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp26_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp26_AST));
			}
			match(LITERAL_threadsafe);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_synchronized:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp27_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp27_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp27_AST));
			}
			match(LITERAL_synchronized);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_volatile:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp28_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp28_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp28_AST));
			}
			match(LITERAL_volatile);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case STRICTFP:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp29_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp29_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp29_AST));
			}
			match(STRICTFP);
			modifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_11);
		} else {
			throw ex;
		}
	}
	returnAST = modifier_AST;
}

void JavaRecognizer::classTypeSpec(
	bool addImagNode
) {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST classTypeSpec_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lb = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lb_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		identifier();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==LBRACK)) {
				lb = LT(1);
				if (inputState->guessing==0) {
					lb_AST = astFactory.create(lb);
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lb_AST));
				}
				match(LBRACK);
				if ( inputState->guessing==0 ) {
#line 160 "java.g"
					lb_AST->setType(ARRAY_DECLARATOR);
#line 907 "JavaRecognizer.cpp"
				}
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp30_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp30_AST = astFactory.create(LT(1));
				match(RBRACK);
			}
			else {
				goto _loop18;
			}
			
		}
		_loop18:;
		}
		if ( inputState->guessing==0 ) {
			classTypeSpec_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 161 "java.g"
			
						if ( addImagNode ) {
							classTypeSpec_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(TYPE,"TYPE"))->add(classTypeSpec_AST)));
						}
					
#line 928 "JavaRecognizer.cpp"
			currentAST.root = classTypeSpec_AST;
			if ( classTypeSpec_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				classTypeSpec_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = classTypeSpec_AST->getFirstChild();
			else
				currentAST.child = classTypeSpec_AST;
			currentAST.advanceChildToEnd();
		}
		classTypeSpec_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_10);
		} else {
			throw ex;
		}
	}
	returnAST = classTypeSpec_AST;
}

void JavaRecognizer::builtInTypeSpec(
	bool addImagNode
) {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST builtInTypeSpec_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lb = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lb_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		builtInType();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==LBRACK)) {
				lb = LT(1);
				if (inputState->guessing==0) {
					lb_AST = astFactory.create(lb);
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lb_AST));
				}
				match(LBRACK);
				if ( inputState->guessing==0 ) {
#line 171 "java.g"
					lb_AST->setType(ARRAY_DECLARATOR);
#line 977 "JavaRecognizer.cpp"
				}
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp31_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp31_AST = astFactory.create(LT(1));
				match(RBRACK);
			}
			else {
				goto _loop21;
			}
			
		}
		_loop21:;
		}
		if ( inputState->guessing==0 ) {
			builtInTypeSpec_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 172 "java.g"
			
						if ( addImagNode ) {
							builtInTypeSpec_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(TYPE,"TYPE"))->add(builtInTypeSpec_AST)));
						}
					
#line 998 "JavaRecognizer.cpp"
			currentAST.root = builtInTypeSpec_AST;
			if ( builtInTypeSpec_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				builtInTypeSpec_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = builtInTypeSpec_AST->getFirstChild();
			else
				currentAST.child = builtInTypeSpec_AST;
			currentAST.advanceChildToEnd();
		}
		builtInTypeSpec_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_10);
		} else {
			throw ex;
		}
	}
	returnAST = builtInTypeSpec_AST;
}

void JavaRecognizer::builtInType() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST builtInType_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case LITERAL_void:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp32_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp32_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp32_AST));
			}
			match(LITERAL_void);
			builtInType_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_boolean:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp33_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp33_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp33_AST));
			}
			match(LITERAL_boolean);
			builtInType_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_byte:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp34_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp34_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp34_AST));
			}
			match(LITERAL_byte);
			builtInType_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_char:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp35_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp35_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp35_AST));
			}
			match(LITERAL_char);
			builtInType_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_short:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp36_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp36_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp36_AST));
			}
			match(LITERAL_short);
			builtInType_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_int:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp37_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp37_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp37_AST));
			}
			match(LITERAL_int);
			builtInType_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_float:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp38_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp38_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp38_AST));
			}
			match(LITERAL_float);
			builtInType_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_long:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp39_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp39_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp39_AST));
			}
			match(LITERAL_long);
			builtInType_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_double:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp40_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp40_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp40_AST));
			}
			match(LITERAL_double);
			builtInType_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_12);
		} else {
			throw ex;
		}
	}
	returnAST = builtInType_AST;
}

void JavaRecognizer::type() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST type_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case IDENT:
		{
			identifier();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			type_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		{
			builtInType();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			type_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_13);
		} else {
			throw ex;
		}
	}
	returnAST = type_AST;
}

void JavaRecognizer::superClassClause() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST superClassClause_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST id_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case LITERAL_extends:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp41_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp41_AST = astFactory.create(LT(1));
			}
			match(LITERAL_extends);
			identifier();
			if (inputState->guessing==0) {
				id_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
			}
			break;
		}
		case LCURLY:
		case LITERAL_implements:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState->guessing==0 ) {
			superClassClause_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 246 "java.g"
			superClassClause_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(EXTENDS_CLAUSE,"EXTENDS_CLAUSE"))->add(id_AST)));
#line 1233 "JavaRecognizer.cpp"
			currentAST.root = superClassClause_AST;
			if ( superClassClause_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				superClassClause_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = superClassClause_AST->getFirstChild();
			else
				currentAST.child = superClassClause_AST;
			currentAST.advanceChildToEnd();
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_14);
		} else {
			throw ex;
		}
	}
	returnAST = superClassClause_AST;
}

void JavaRecognizer::implementsClause() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST implementsClause_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  i = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST i_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case LITERAL_implements:
		{
			i = LT(1);
			if (inputState->guessing==0) {
				i_AST = astFactory.create(i);
			}
			match(LITERAL_implements);
			identifier();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			{
			for (;;) {
				if ((LA(1)==COMMA)) {
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp42_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp42_AST = astFactory.create(LT(1));
					match(COMMA);
					identifier();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
				}
				else {
					goto _loop46;
				}
				
			}
			_loop46:;
			}
			break;
		}
		case LCURLY:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState->guessing==0 ) {
			implementsClause_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 285 "java.g"
			implementsClause_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(IMPLEMENTS_CLAUSE,"IMPLEMENTS_CLAUSE"))->add(implementsClause_AST)));
#line 1310 "JavaRecognizer.cpp"
			currentAST.root = implementsClause_AST;
			if ( implementsClause_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				implementsClause_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = implementsClause_AST->getFirstChild();
			else
				currentAST.child = implementsClause_AST;
			currentAST.advanceChildToEnd();
		}
		implementsClause_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_15);
		} else {
			throw ex;
		}
	}
	returnAST = implementsClause_AST;
}

void JavaRecognizer::classBlock() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST classBlock_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp43_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp43_AST = astFactory.create(LT(1));
		match(LCURLY);
		{
		for (;;) {
			switch ( LA(1)) {
			case FINAL:
			case ABSTRACT:
			case STRICTFP:
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			case LITERAL_private:
			case LITERAL_public:
			case LITERAL_protected:
			case LITERAL_static:
			case LITERAL_transient:
			case LITERAL_native:
			case LITERAL_threadsafe:
			case LITERAL_synchronized:
			case LITERAL_volatile:
			case LITERAL_class:
			case LITERAL_interface:
			case LCURLY:
			case SL_COMMENT:
			case ML_COMMENT:
			{
				field();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
				break;
			}
			case SEMI:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp44_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp44_AST = astFactory.create(LT(1));
				match(SEMI);
				break;
			}
			default:
			{
				goto _loop38;
			}
			}
		}
		_loop38:;
		}
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp45_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp45_AST = astFactory.create(LT(1));
		match(RCURLY);
		if ( inputState->guessing==0 ) {
			classBlock_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 267 "java.g"
			classBlock_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(OBJBLOCK,"OBJBLOCK"))->add(classBlock_AST)));
#line 1401 "JavaRecognizer.cpp"
			currentAST.root = classBlock_AST;
			if ( classBlock_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				classBlock_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = classBlock_AST->getFirstChild();
			else
				currentAST.child = classBlock_AST;
			currentAST.advanceChildToEnd();
		}
		classBlock_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_16);
		} else {
			throw ex;
		}
	}
	returnAST = classBlock_AST;
}

void JavaRecognizer::interfaceExtends() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST interfaceExtends_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  e = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST e_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case LITERAL_extends:
		{
			e = LT(1);
			if (inputState->guessing==0) {
				e_AST = astFactory.create(e);
			}
			match(LITERAL_extends);
			identifier();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			{
			for (;;) {
				if ((LA(1)==COMMA)) {
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp46_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp46_AST = astFactory.create(LT(1));
					match(COMMA);
					identifier();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
				}
				else {
					goto _loop42;
				}
				
			}
			_loop42:;
			}
			break;
		}
		case LCURLY:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState->guessing==0 ) {
			interfaceExtends_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 276 "java.g"
			interfaceExtends_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(EXTENDS_CLAUSE,"EXTENDS_CLAUSE"))->add(interfaceExtends_AST)));
#line 1479 "JavaRecognizer.cpp"
			currentAST.root = interfaceExtends_AST;
			if ( interfaceExtends_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				interfaceExtends_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = interfaceExtends_AST->getFirstChild();
			else
				currentAST.child = interfaceExtends_AST;
			currentAST.advanceChildToEnd();
		}
		interfaceExtends_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_15);
		} else {
			throw ex;
		}
	}
	returnAST = interfaceExtends_AST;
}

void JavaRecognizer::field() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST field_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST mods_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST h_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST s_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST cd_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST id_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST t_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST param_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST rt_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST tc_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST s2_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST v_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST s3_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST s4_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  sc = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST sc_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  ml = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST ml_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case LCURLY:
		{
			compoundStatement();
			if (inputState->guessing==0) {
				s4_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
			}
			if ( inputState->guessing==0 ) {
				field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 336 "java.g"
				field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(INSTANCE_INIT,"INSTANCE_INIT"))->add(s4_AST)));
#line 1536 "JavaRecognizer.cpp"
				currentAST.root = field_AST;
				if ( field_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
					field_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
					  currentAST.child = field_AST->getFirstChild();
				else
					currentAST.child = field_AST;
				currentAST.advanceChildToEnd();
			}
			break;
		}
		case SL_COMMENT:
		{
			sc = LT(1);
			if (inputState->guessing==0) {
				sc_AST = astFactory.create(sc);
			}
			match(SL_COMMENT);
			if ( inputState->guessing==0 ) {
				field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 338 "java.g"
				field_AST = sc_AST;
#line 1558 "JavaRecognizer.cpp"
				currentAST.root = field_AST;
				if ( field_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
					field_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
					  currentAST.child = field_AST->getFirstChild();
				else
					currentAST.child = field_AST;
				currentAST.advanceChildToEnd();
			}
			break;
		}
		case ML_COMMENT:
		{
			ml = LT(1);
			if (inputState->guessing==0) {
				ml_AST = astFactory.create(ml);
			}
			match(ML_COMMENT);
			if ( inputState->guessing==0 ) {
				field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 340 "java.g"
				field_AST = ml_AST;
#line 1580 "JavaRecognizer.cpp"
				currentAST.root = field_AST;
				if ( field_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
					field_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
					  currentAST.child = field_AST->getFirstChild();
				else
					currentAST.child = field_AST;
				currentAST.advanceChildToEnd();
			}
			break;
		}
		default:
			if ((_tokenSet_11.member(LA(1))) && (_tokenSet_17.member(LA(2)))) {
				modifiers();
				if (inputState->guessing==0) {
					mods_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
				}
				{
				switch ( LA(1)) {
				case LITERAL_class:
				{
					classDefinition(mods_AST);
					if (inputState->guessing==0) {
						cd_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
					}
					if ( inputState->guessing==0 ) {
						field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 300 "java.g"
						field_AST = cd_AST;
#line 1609 "JavaRecognizer.cpp"
						currentAST.root = field_AST;
						if ( field_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
							field_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
							  currentAST.child = field_AST->getFirstChild();
						else
							currentAST.child = field_AST;
						currentAST.advanceChildToEnd();
					}
					break;
				}
				case LITERAL_interface:
				{
					interfaceDefinition(mods_AST);
					if (inputState->guessing==0) {
						id_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
					}
					if ( inputState->guessing==0 ) {
						field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 303 "java.g"
						field_AST = id_AST;
#line 1630 "JavaRecognizer.cpp"
						currentAST.root = field_AST;
						if ( field_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
							field_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
							  currentAST.child = field_AST->getFirstChild();
						else
							currentAST.child = field_AST;
						currentAST.advanceChildToEnd();
					}
					break;
				}
				default:
					if ((LA(1)==IDENT) && (LA(2)==LPAREN)) {
						ctorHead();
						if (inputState->guessing==0) {
							h_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
						}
						compoundStatement();
						if (inputState->guessing==0) {
							s_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
						}
						if ( inputState->guessing==0 ) {
							field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 297 "java.g"
							field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(4))->add(astFactory.create(CTOR_DEF,"CTOR_DEF"))->add(mods_AST)->add(h_AST)->add(s_AST)));
#line 1655 "JavaRecognizer.cpp"
							currentAST.root = field_AST;
							if ( field_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
								field_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
								  currentAST.child = field_AST->getFirstChild();
							else
								currentAST.child = field_AST;
							currentAST.advanceChildToEnd();
						}
					}
					else if (((LA(1) >= LITERAL_void && LA(1) <= IDENT)) && (_tokenSet_18.member(LA(2)))) {
						typeSpec(false);
						if (inputState->guessing==0) {
							t_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
						}
						{
						if ((LA(1)==IDENT) && (LA(2)==LPAREN)) {
							ANTLR_USE_NAMESPACE(antlr)RefAST tmp47_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
							if (inputState->guessing==0) {
								tmp47_AST = astFactory.create(LT(1));
							}
							match(IDENT);
							ANTLR_USE_NAMESPACE(antlr)RefAST tmp48_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
							tmp48_AST = astFactory.create(LT(1));
							match(LPAREN);
							parameterDeclarationList();
							if (inputState->guessing==0) {
								param_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
							}
							ANTLR_USE_NAMESPACE(antlr)RefAST tmp49_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
							tmp49_AST = astFactory.create(LT(1));
							match(RPAREN);
							returnTypeBrackersOnEndOfMethodHead(t_AST);
							if (inputState->guessing==0) {
								rt_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
							}
							{
							switch ( LA(1)) {
							case LITERAL_throws:
							{
								throwsClause();
								if (inputState->guessing==0) {
									tc_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
								}
								break;
							}
							case SEMI:
							case LCURLY:
							{
								break;
							}
							default:
							{
								throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
							}
							}
							}
							{
							switch ( LA(1)) {
							case LCURLY:
							{
								compoundStatement();
								if (inputState->guessing==0) {
									s2_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
								}
								break;
							}
							case SEMI:
							{
								ANTLR_USE_NAMESPACE(antlr)RefAST tmp50_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
								if (inputState->guessing==0) {
									tmp50_AST = astFactory.create(LT(1));
								}
								match(SEMI);
								break;
							}
							default:
							{
								throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
							}
							}
							}
							if ( inputState->guessing==0 ) {
								field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 317 "java.g"
								field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(7))->add(astFactory.create(METHOD_DEF,"METHOD_DEF"))->add(mods_AST)->add(ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(TYPE,"TYPE"))->add(rt_AST))))->add(tmp47_AST)->add(param_AST)->add(tc_AST)->add(s2_AST)));
#line 1741 "JavaRecognizer.cpp"
								currentAST.root = field_AST;
								if ( field_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
									field_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
									  currentAST.child = field_AST->getFirstChild();
								else
									currentAST.child = field_AST;
								currentAST.advanceChildToEnd();
							}
						}
						else if ((LA(1)==IDENT) && (_tokenSet_19.member(LA(2)))) {
							variableDefinitions(mods_AST,t_AST);
							if (inputState->guessing==0) {
								v_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
							}
							ANTLR_USE_NAMESPACE(antlr)RefAST tmp51_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
							if (inputState->guessing==0) {
								tmp51_AST = astFactory.create(LT(1));
							}
							match(SEMI);
							if ( inputState->guessing==0 ) {
								field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 326 "java.g"
								field_AST = v_AST;
#line 1765 "JavaRecognizer.cpp"
								currentAST.root = field_AST;
								if ( field_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
									field_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
									  currentAST.child = field_AST->getFirstChild();
								else
									currentAST.child = field_AST;
								currentAST.advanceChildToEnd();
							}
						}
						else {
							throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
						}
						
						}
					}
				else {
					throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
				}
				}
				}
			}
			else if ((LA(1)==LITERAL_static) && (LA(2)==LCURLY)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp52_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp52_AST = astFactory.create(LT(1));
				}
				match(LITERAL_static);
				compoundStatement();
				if (inputState->guessing==0) {
					s3_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
				}
				if ( inputState->guessing==0 ) {
					field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 332 "java.g"
					field_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(STATIC_INIT,"STATIC_INIT"))->add(s3_AST)));
#line 1801 "JavaRecognizer.cpp"
					currentAST.root = field_AST;
					if ( field_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
						field_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
						  currentAST.child = field_AST->getFirstChild();
					else
						currentAST.child = field_AST;
					currentAST.advanceChildToEnd();
				}
			}
		else {
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_20);
		} else {
			throw ex;
		}
	}
	returnAST = field_AST;
}

void JavaRecognizer::ctorHead() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST ctorHead_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp53_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp53_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp53_AST));
		}
		match(IDENT);
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp54_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp54_AST = astFactory.create(LT(1));
		match(LPAREN);
		parameterDeclarationList();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp55_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp55_AST = astFactory.create(LT(1));
		match(RPAREN);
		{
		switch ( LA(1)) {
		case LITERAL_throws:
		{
			throwsClause();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		case LCURLY:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		ctorHead_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_15);
		} else {
			throw ex;
		}
	}
	returnAST = ctorHead_AST;
}

void JavaRecognizer::compoundStatement() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST compoundStatement_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lc = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lc_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		lc = LT(1);
		if (inputState->guessing==0) {
			lc_AST = astFactory.create(lc);
			astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lc_AST));
		}
		match(LCURLY);
		if ( inputState->guessing==0 ) {
#line 457 "java.g"
			lc_AST->setType(SLIST);
#line 1901 "JavaRecognizer.cpp"
		}
		{
		for (;;) {
			if ((_tokenSet_21.member(LA(1)))) {
				statement();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop87;
			}
			
		}
		_loop87:;
		}
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp56_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp56_AST = astFactory.create(LT(1));
		match(RCURLY);
		compoundStatement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_22);
		} else {
			throw ex;
		}
	}
	returnAST = compoundStatement_AST;
}

void JavaRecognizer::parameterDeclarationList() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST parameterDeclarationList_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case FINAL:
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		case IDENT:
		{
			parameterDeclaration();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			{
			for (;;) {
				if ((LA(1)==COMMA)) {
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp57_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp57_AST = astFactory.create(LT(1));
					match(COMMA);
					parameterDeclaration();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
				}
				else {
					goto _loop78;
				}
				
			}
			_loop78:;
			}
			break;
		}
		case RPAREN:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState->guessing==0 ) {
			parameterDeclarationList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 425 "java.g"
			parameterDeclarationList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(PARAMETERS,"PARAMETERS"))->add(parameterDeclarationList_AST)));
#line 1993 "JavaRecognizer.cpp"
			currentAST.root = parameterDeclarationList_AST;
			if ( parameterDeclarationList_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				parameterDeclarationList_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = parameterDeclarationList_AST->getFirstChild();
			else
				currentAST.child = parameterDeclarationList_AST;
			currentAST.advanceChildToEnd();
		}
		parameterDeclarationList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_23);
		} else {
			throw ex;
		}
	}
	returnAST = parameterDeclarationList_AST;
}

void JavaRecognizer::returnTypeBrackersOnEndOfMethodHead(
	ANTLR_USE_NAMESPACE(antlr)RefAST typ
) {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST returnTypeBrackersOnEndOfMethodHead_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lb = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lb_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		if ( inputState->guessing==0 ) {
			returnTypeBrackersOnEndOfMethodHead_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 418 "java.g"
			returnTypeBrackersOnEndOfMethodHead_AST = typ;
#line 2030 "JavaRecognizer.cpp"
			currentAST.root = returnTypeBrackersOnEndOfMethodHead_AST;
			if ( returnTypeBrackersOnEndOfMethodHead_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				returnTypeBrackersOnEndOfMethodHead_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = returnTypeBrackersOnEndOfMethodHead_AST->getFirstChild();
			else
				currentAST.child = returnTypeBrackersOnEndOfMethodHead_AST;
			currentAST.advanceChildToEnd();
		}
		{
		for (;;) {
			if ((LA(1)==LBRACK)) {
				lb = LT(1);
				if (inputState->guessing==0) {
					lb_AST = astFactory.create(lb);
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lb_AST));
				}
				match(LBRACK);
				if ( inputState->guessing==0 ) {
#line 419 "java.g"
					lb_AST->setType(ARRAY_DECLARATOR);
#line 2051 "JavaRecognizer.cpp"
				}
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp58_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp58_AST = astFactory.create(LT(1));
				match(RBRACK);
			}
			else {
				goto _loop74;
			}
			
		}
		_loop74:;
		}
		returnTypeBrackersOnEndOfMethodHead_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_24);
		} else {
			throw ex;
		}
	}
	returnAST = returnTypeBrackersOnEndOfMethodHead_AST;
}

void JavaRecognizer::throwsClause() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST throwsClause_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp59_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp59_AST = astFactory.create(LT(1));
			astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp59_AST));
		}
		match(LITERAL_throws);
		identifier();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==COMMA)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp60_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp60_AST = astFactory.create(LT(1));
				match(COMMA);
				identifier();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop71;
			}
			
		}
		_loop71:;
		}
		throwsClause_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_25);
		} else {
			throw ex;
		}
	}
	returnAST = throwsClause_AST;
}

/** Declaration of a variable.  This can be a class/instance variable,
 *   or a local variable in a method
 * It can also include possible initialization.
 */
void JavaRecognizer::variableDeclarator(
	ANTLR_USE_NAMESPACE(antlr)RefAST mods, ANTLR_USE_NAMESPACE(antlr)RefAST t
) {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST variableDeclarator_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  id = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST id_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST d_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST v_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		id = LT(1);
		if (inputState->guessing==0) {
			id_AST = astFactory.create(id);
		}
		match(IDENT);
		declaratorBrackets(t);
		if (inputState->guessing==0) {
			d_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		varInitializer();
		if (inputState->guessing==0) {
			v_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		if ( inputState->guessing==0 ) {
			variableDeclarator_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 358 "java.g"
			variableDeclarator_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(5))->add(astFactory.create(VARIABLE_DEF,"VARIABLE_DEF"))->add(mods)->add(ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(TYPE,"TYPE"))->add(d_AST))))->add(id_AST)->add(v_AST)));
#line 2159 "JavaRecognizer.cpp"
			currentAST.root = variableDeclarator_AST;
			if ( variableDeclarator_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				variableDeclarator_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = variableDeclarator_AST->getFirstChild();
			else
				currentAST.child = variableDeclarator_AST;
			currentAST.advanceChildToEnd();
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_26);
		} else {
			throw ex;
		}
	}
	returnAST = variableDeclarator_AST;
}

void JavaRecognizer::declaratorBrackets(
	ANTLR_USE_NAMESPACE(antlr)RefAST typ
) {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST declaratorBrackets_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lb = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lb_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		if ( inputState->guessing==0 ) {
			declaratorBrackets_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 362 "java.g"
			declaratorBrackets_AST=typ;
#line 2195 "JavaRecognizer.cpp"
			currentAST.root = declaratorBrackets_AST;
			if ( declaratorBrackets_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				declaratorBrackets_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = declaratorBrackets_AST->getFirstChild();
			else
				currentAST.child = declaratorBrackets_AST;
			currentAST.advanceChildToEnd();
		}
		{
		for (;;) {
			if ((LA(1)==LBRACK)) {
				lb = LT(1);
				if (inputState->guessing==0) {
					lb_AST = astFactory.create(lb);
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lb_AST));
				}
				match(LBRACK);
				if ( inputState->guessing==0 ) {
#line 363 "java.g"
					lb_AST->setType(ARRAY_DECLARATOR);
#line 2216 "JavaRecognizer.cpp"
				}
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp61_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp61_AST = astFactory.create(LT(1));
				match(RBRACK);
			}
			else {
				goto _loop58;
			}
			
		}
		_loop58:;
		}
		declaratorBrackets_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_27);
		} else {
			throw ex;
		}
	}
	returnAST = declaratorBrackets_AST;
}

void JavaRecognizer::varInitializer() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST varInitializer_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case ASSIGN:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp62_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp62_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp62_AST));
			}
			match(ASSIGN);
			initializer();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		case SEMI:
		case COMMA:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		varInitializer_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_26);
		} else {
			throw ex;
		}
	}
	returnAST = varInitializer_AST;
}

void JavaRecognizer::initializer() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST initializer_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		case IDENT:
		case LPAREN:
		case PLUS:
		case MINUS:
		case INC:
		case DEC:
		case BNOT:
		case LNOT:
		case LITERAL_this:
		case LITERAL_super:
		case LITERAL_true:
		case LITERAL_false:
		case LITERAL_null:
		case LITERAL_new:
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		{
			expression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			initializer_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LCURLY:
		{
			arrayInitializer();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			initializer_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_28);
		} else {
			throw ex;
		}
	}
	returnAST = initializer_AST;
}

void JavaRecognizer::arrayInitializer() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST arrayInitializer_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lc = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lc_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		lc = LT(1);
		if (inputState->guessing==0) {
			lc_AST = astFactory.create(lc);
			astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lc_AST));
		}
		match(LCURLY);
		if ( inputState->guessing==0 ) {
#line 372 "java.g"
			lc_AST->setType(ARRAY_INIT);
#line 2376 "JavaRecognizer.cpp"
		}
		{
		switch ( LA(1)) {
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		case IDENT:
		case LCURLY:
		case LPAREN:
		case PLUS:
		case MINUS:
		case INC:
		case DEC:
		case BNOT:
		case LNOT:
		case LITERAL_this:
		case LITERAL_super:
		case LITERAL_true:
		case LITERAL_false:
		case LITERAL_null:
		case LITERAL_new:
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		{
			initializer();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			{
			for (;;) {
				if ((LA(1)==COMMA) && (_tokenSet_29.member(LA(2)))) {
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp63_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp63_AST = astFactory.create(LT(1));
					match(COMMA);
					initializer();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
				}
				else {
					goto _loop64;
				}
				
			}
			_loop64:;
			}
			{
			switch ( LA(1)) {
			case COMMA:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp64_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp64_AST = astFactory.create(LT(1));
				match(COMMA);
				break;
			}
			case RCURLY:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			break;
		}
		case RCURLY:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp65_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp65_AST = astFactory.create(LT(1));
		match(RCURLY);
		arrayInitializer_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_30);
		} else {
			throw ex;
		}
	}
	returnAST = arrayInitializer_AST;
}

void JavaRecognizer::expression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST expression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		assignmentExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		if ( inputState->guessing==0 ) {
			expression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 646 "java.g"
			expression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(EXPR,"EXPR"))->add(expression_AST)));
#line 2493 "JavaRecognizer.cpp"
			currentAST.root = expression_AST;
			if ( expression_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				expression_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = expression_AST->getFirstChild();
			else
				currentAST.child = expression_AST;
			currentAST.advanceChildToEnd();
		}
		expression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_31);
		} else {
			throw ex;
		}
	}
	returnAST = expression_AST;
}

void JavaRecognizer::parameterDeclaration() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST parameterDeclaration_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST pm_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST t_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  id = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST id_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST pd_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		parameterModifier();
		if (inputState->guessing==0) {
			pm_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		typeSpec(false);
		if (inputState->guessing==0) {
			t_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		id = LT(1);
		if (inputState->guessing==0) {
			id_AST = astFactory.create(id);
		}
		match(IDENT);
		parameterDeclaratorBrackets(t_AST);
		if (inputState->guessing==0) {
			pd_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST);
		}
		if ( inputState->guessing==0 ) {
			parameterDeclaration_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 433 "java.g"
			parameterDeclaration_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(4))->add(astFactory.create(PARAMETER_DEF,"PARAMETER_DEF"))->add(pm_AST)->add(ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(TYPE,"TYPE"))->add(pd_AST))))->add(id_AST)));
#line 2548 "JavaRecognizer.cpp"
			currentAST.root = parameterDeclaration_AST;
			if ( parameterDeclaration_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				parameterDeclaration_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = parameterDeclaration_AST->getFirstChild();
			else
				currentAST.child = parameterDeclaration_AST;
			currentAST.advanceChildToEnd();
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_32);
		} else {
			throw ex;
		}
	}
	returnAST = parameterDeclaration_AST;
}

void JavaRecognizer::parameterModifier() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST parameterModifier_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  f = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST f_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case FINAL:
		{
			f = LT(1);
			if (inputState->guessing==0) {
				f_AST = astFactory.create(f);
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(f_AST));
			}
			match(FINAL);
			break;
		}
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		case IDENT:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState->guessing==0 ) {
			parameterModifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 444 "java.g"
			parameterModifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(MODIFIERS,"MODIFIERS"))->add(f_AST)));
#line 2613 "JavaRecognizer.cpp"
			currentAST.root = parameterModifier_AST;
			if ( parameterModifier_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				parameterModifier_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = parameterModifier_AST->getFirstChild();
			else
				currentAST.child = parameterModifier_AST;
			currentAST.advanceChildToEnd();
		}
		parameterModifier_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_33);
		} else {
			throw ex;
		}
	}
	returnAST = parameterModifier_AST;
}

void JavaRecognizer::parameterDeclaratorBrackets(
	ANTLR_USE_NAMESPACE(antlr)RefAST t
) {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST parameterDeclaratorBrackets_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lb = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lb_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		if ( inputState->guessing==0 ) {
			parameterDeclaratorBrackets_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 438 "java.g"
			parameterDeclaratorBrackets_AST = t;
#line 2650 "JavaRecognizer.cpp"
			currentAST.root = parameterDeclaratorBrackets_AST;
			if ( parameterDeclaratorBrackets_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				parameterDeclaratorBrackets_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = parameterDeclaratorBrackets_AST->getFirstChild();
			else
				currentAST.child = parameterDeclaratorBrackets_AST;
			currentAST.advanceChildToEnd();
		}
		{
		for (;;) {
			if ((LA(1)==LBRACK)) {
				lb = LT(1);
				if (inputState->guessing==0) {
					lb_AST = astFactory.create(lb);
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lb_AST));
				}
				match(LBRACK);
				if ( inputState->guessing==0 ) {
#line 439 "java.g"
					lb_AST->setType(ARRAY_DECLARATOR);
#line 2671 "JavaRecognizer.cpp"
				}
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp66_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp66_AST = astFactory.create(LT(1));
				match(RBRACK);
			}
			else {
				goto _loop82;
			}
			
		}
		_loop82:;
		}
		parameterDeclaratorBrackets_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_32);
		} else {
			throw ex;
		}
	}
	returnAST = parameterDeclaratorBrackets_AST;
}

void JavaRecognizer::statement() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST statement_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  c = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST c_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  s = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST s_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case LCURLY:
		{
			compoundStatement();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_class:
		{
			classDefinition(astFactory.create(MODIFIERS,"MODIFIERS"));
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_if:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp67_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp67_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp67_AST));
			}
			match(LITERAL_if);
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp68_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp68_AST = astFactory.create(LT(1));
			match(LPAREN);
			expression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp69_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp69_AST = astFactory.create(LT(1));
			match(RPAREN);
			statement();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			{
			if ((LA(1)==LITERAL_else) && (_tokenSet_21.member(LA(2)))) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp70_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp70_AST = astFactory.create(LT(1));
				match(LITERAL_else);
				statement();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else if ((_tokenSet_34.member(LA(1))) && (_tokenSet_35.member(LA(2)))) {
			}
			else {
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			
			}
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_for:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp71_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp71_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp71_AST));
			}
			match(LITERAL_for);
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp72_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp72_AST = astFactory.create(LT(1));
			match(LPAREN);
			forInit();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp73_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp73_AST = astFactory.create(LT(1));
			match(SEMI);
			forCond();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp74_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp74_AST = astFactory.create(LT(1));
			match(SEMI);
			forIter();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp75_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp75_AST = astFactory.create(LT(1));
			match(RPAREN);
			statement();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_while:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp76_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp76_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp76_AST));
			}
			match(LITERAL_while);
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp77_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp77_AST = astFactory.create(LT(1));
			match(LPAREN);
			expression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp78_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp78_AST = astFactory.create(LT(1));
			match(RPAREN);
			statement();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_do:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp79_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp79_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp79_AST));
			}
			match(LITERAL_do);
			statement();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp80_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp80_AST = astFactory.create(LT(1));
			match(LITERAL_while);
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp81_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp81_AST = astFactory.create(LT(1));
			match(LPAREN);
			expression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp82_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp82_AST = astFactory.create(LT(1));
			match(RPAREN);
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp83_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp83_AST = astFactory.create(LT(1));
			match(SEMI);
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_break:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp84_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp84_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp84_AST));
			}
			match(LITERAL_break);
			{
			switch ( LA(1)) {
			case IDENT:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp85_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp85_AST = astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp85_AST));
				}
				match(IDENT);
				break;
			}
			case SEMI:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp86_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp86_AST = astFactory.create(LT(1));
			match(SEMI);
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_continue:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp87_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp87_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp87_AST));
			}
			match(LITERAL_continue);
			{
			switch ( LA(1)) {
			case IDENT:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp88_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp88_AST = astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp88_AST));
				}
				match(IDENT);
				break;
			}
			case SEMI:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp89_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp89_AST = astFactory.create(LT(1));
			match(SEMI);
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_return:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp90_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp90_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp90_AST));
			}
			match(LITERAL_return);
			{
			switch ( LA(1)) {
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			case LPAREN:
			case PLUS:
			case MINUS:
			case INC:
			case DEC:
			case BNOT:
			case LNOT:
			case LITERAL_this:
			case LITERAL_super:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case LITERAL_new:
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				expression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
				break;
			}
			case SEMI:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp91_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp91_AST = astFactory.create(LT(1));
			match(SEMI);
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_switch:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp92_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp92_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp92_AST));
			}
			match(LITERAL_switch);
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp93_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp93_AST = astFactory.create(LT(1));
			match(LPAREN);
			expression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp94_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp94_AST = astFactory.create(LT(1));
			match(RPAREN);
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp95_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp95_AST = astFactory.create(LT(1));
			match(LCURLY);
			{
			for (;;) {
				if ((LA(1)==LITERAL_case||LA(1)==LITERAL_default)) {
					casesGroup();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
				}
				else {
					goto _loop96;
				}
				
			}
			_loop96:;
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp96_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp96_AST = astFactory.create(LT(1));
			match(RCURLY);
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_try:
		{
			tryBlock();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_throw:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp97_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp97_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp97_AST));
			}
			match(LITERAL_throw);
			expression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp98_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp98_AST = astFactory.create(LT(1));
			match(SEMI);
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_assert:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp99_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp99_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp99_AST));
			}
			match(LITERAL_assert);
			expression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			{
			switch ( LA(1)) {
			case COLON:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp100_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp100_AST = astFactory.create(LT(1));
				match(COLON);
				expression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
				break;
			}
			case SEMI:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp101_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp101_AST = astFactory.create(LT(1));
			match(SEMI);
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case SEMI:
		{
			s = LT(1);
			if (inputState->guessing==0) {
				s_AST = astFactory.create(s);
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(s_AST));
			}
			match(SEMI);
			if ( inputState->guessing==0 ) {
#line 547 "java.g"
				s_AST->setType(EMPTY_STAT);
#line 3117 "JavaRecognizer.cpp"
			}
			statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
			if ((LA(1)==FINAL) && (LA(2)==LITERAL_class)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp102_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp102_AST = astFactory.create(LT(1));
				match(FINAL);
				classDefinition(ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(MODIFIERS,"MODIFIERS"))->add(astFactory.create(FINAL,"final")))));
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
				statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			}
			else if ((LA(1)==ABSTRACT) && (LA(2)==LITERAL_class)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp103_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp103_AST = astFactory.create(LT(1));
				match(ABSTRACT);
				classDefinition(ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(MODIFIERS,"MODIFIERS"))->add(astFactory.create(ABSTRACT,"abstract")))));
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
				statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			}
			else {
				bool synPredMatched90 = false;
				if (((_tokenSet_36.member(LA(1))) && (_tokenSet_37.member(LA(2))))) {
					int _m90 = mark();
					synPredMatched90 = true;
					inputState->guessing++;
					try {
						{
						declaration();
						}
					}
					catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& pe) {
						synPredMatched90 = false;
					}
					rewind(_m90);
					inputState->guessing--;
				}
				if ( synPredMatched90 ) {
					declaration();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp104_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp104_AST = astFactory.create(LT(1));
					match(SEMI);
					statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
				}
				else if ((_tokenSet_38.member(LA(1))) && (_tokenSet_39.member(LA(2)))) {
					expression();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp105_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp105_AST = astFactory.create(LT(1));
					match(SEMI);
					statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
				}
				else if ((LA(1)==IDENT) && (LA(2)==COLON)) {
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp106_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp106_AST = astFactory.create(LT(1));
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp106_AST));
					}
					match(IDENT);
					c = LT(1);
					if (inputState->guessing==0) {
						c_AST = astFactory.create(c);
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(c_AST));
					}
					match(COLON);
					if ( inputState->guessing==0 ) {
#line 489 "java.g"
						c_AST->setType(LABELED_STAT);
#line 3196 "JavaRecognizer.cpp"
					}
					statement();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
					statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
				}
				else if ((LA(1)==LITERAL_synchronized) && (LA(2)==LPAREN)) {
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp107_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp107_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp107_AST));
					}
					match(LITERAL_synchronized);
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp108_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp108_AST = astFactory.create(LT(1));
					match(LPAREN);
					expression();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp109_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp109_AST = astFactory.create(LT(1));
					match(RPAREN);
					compoundStatement();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
					statement_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
				}
		else {
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_34);
		} else {
			throw ex;
		}
	}
	returnAST = statement_AST;
}

void JavaRecognizer::forInit() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST forInit_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		bool synPredMatched109 = false;
		if (((_tokenSet_36.member(LA(1))) && (_tokenSet_37.member(LA(2))))) {
			int _m109 = mark();
			synPredMatched109 = true;
			inputState->guessing++;
			try {
				{
				declaration();
				}
			}
			catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& pe) {
				synPredMatched109 = false;
			}
			rewind(_m109);
			inputState->guessing--;
		}
		if ( synPredMatched109 ) {
			declaration();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
		}
		else if ((_tokenSet_38.member(LA(1))) && (_tokenSet_40.member(LA(2)))) {
			expressionList();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
		}
		else if ((LA(1)==SEMI)) {
		}
		else {
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		
		}
		if ( inputState->guessing==0 ) {
			forInit_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 582 "java.g"
			forInit_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(FOR_INIT,"FOR_INIT"))->add(forInit_AST)));
#line 3290 "JavaRecognizer.cpp"
			currentAST.root = forInit_AST;
			if ( forInit_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				forInit_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = forInit_AST->getFirstChild();
			else
				currentAST.child = forInit_AST;
			currentAST.advanceChildToEnd();
		}
		forInit_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_5);
		} else {
			throw ex;
		}
	}
	returnAST = forInit_AST;
}

void JavaRecognizer::forCond() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST forCond_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		case IDENT:
		case LPAREN:
		case PLUS:
		case MINUS:
		case INC:
		case DEC:
		case BNOT:
		case LNOT:
		case LITERAL_this:
		case LITERAL_super:
		case LITERAL_true:
		case LITERAL_false:
		case LITERAL_null:
		case LITERAL_new:
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		{
			expression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		case SEMI:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState->guessing==0 ) {
			forCond_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 587 "java.g"
			forCond_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(FOR_CONDITION,"FOR_CONDITION"))->add(forCond_AST)));
#line 3369 "JavaRecognizer.cpp"
			currentAST.root = forCond_AST;
			if ( forCond_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				forCond_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = forCond_AST->getFirstChild();
			else
				currentAST.child = forCond_AST;
			currentAST.advanceChildToEnd();
		}
		forCond_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_5);
		} else {
			throw ex;
		}
	}
	returnAST = forCond_AST;
}

void JavaRecognizer::forIter() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST forIter_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		case IDENT:
		case LPAREN:
		case PLUS:
		case MINUS:
		case INC:
		case DEC:
		case BNOT:
		case LNOT:
		case LITERAL_this:
		case LITERAL_super:
		case LITERAL_true:
		case LITERAL_false:
		case LITERAL_null:
		case LITERAL_new:
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		{
			expressionList();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		case RPAREN:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState->guessing==0 ) {
			forIter_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 592 "java.g"
			forIter_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(FOR_ITERATOR,"FOR_ITERATOR"))->add(forIter_AST)));
#line 3448 "JavaRecognizer.cpp"
			currentAST.root = forIter_AST;
			if ( forIter_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				forIter_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = forIter_AST->getFirstChild();
			else
				currentAST.child = forIter_AST;
			currentAST.advanceChildToEnd();
		}
		forIter_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_23);
		} else {
			throw ex;
		}
	}
	returnAST = forIter_AST;
}

void JavaRecognizer::casesGroup() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST casesGroup_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		int _cnt100=0;
		for (;;) {
			if ((LA(1)==LITERAL_case||LA(1)==LITERAL_default) && (_tokenSet_41.member(LA(2)))) {
				aCase();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				if ( _cnt100>=1 ) { goto _loop100; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt100++;
		}
		_loop100:;
		}
		caseSList();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		if ( inputState->guessing==0 ) {
			casesGroup_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 563 "java.g"
			casesGroup_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(CASE_GROUP,"CASE_GROUP"))->add(casesGroup_AST)));
#line 3502 "JavaRecognizer.cpp"
			currentAST.root = casesGroup_AST;
			if ( casesGroup_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				casesGroup_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = casesGroup_AST->getFirstChild();
			else
				currentAST.child = casesGroup_AST;
			currentAST.advanceChildToEnd();
		}
		casesGroup_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_42);
		} else {
			throw ex;
		}
	}
	returnAST = casesGroup_AST;
}

void JavaRecognizer::tryBlock() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST tryBlock_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp110_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp110_AST = astFactory.create(LT(1));
			astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp110_AST));
		}
		match(LITERAL_try);
		compoundStatement();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==LITERAL_catch)) {
				handler();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop116;
			}
			
		}
		_loop116:;
		}
		{
		switch ( LA(1)) {
		case LITERAL_finally:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp111_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp111_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp111_AST));
			}
			match(LITERAL_finally);
			compoundStatement();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		case FINAL:
		case ABSTRACT:
		case STRICTFP:
		case SEMI:
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		case IDENT:
		case LITERAL_private:
		case LITERAL_public:
		case LITERAL_protected:
		case LITERAL_static:
		case LITERAL_transient:
		case LITERAL_native:
		case LITERAL_threadsafe:
		case LITERAL_synchronized:
		case LITERAL_volatile:
		case LITERAL_class:
		case LCURLY:
		case RCURLY:
		case LPAREN:
		case LITERAL_if:
		case LITERAL_else:
		case LITERAL_for:
		case LITERAL_while:
		case LITERAL_do:
		case LITERAL_break:
		case LITERAL_continue:
		case LITERAL_return:
		case LITERAL_switch:
		case LITERAL_throw:
		case LITERAL_assert:
		case LITERAL_case:
		case LITERAL_default:
		case LITERAL_try:
		case PLUS:
		case MINUS:
		case INC:
		case DEC:
		case BNOT:
		case LNOT:
		case LITERAL_this:
		case LITERAL_super:
		case LITERAL_true:
		case LITERAL_false:
		case LITERAL_null:
		case LITERAL_new:
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		tryBlock_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_34);
		} else {
			throw ex;
		}
	}
	returnAST = tryBlock_AST;
}

void JavaRecognizer::aCase() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST aCase_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case LITERAL_case:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp112_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp112_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp112_AST));
			}
			match(LITERAL_case);
			expression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		case LITERAL_default:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp113_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp113_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp113_AST));
			}
			match(LITERAL_default);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp114_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp114_AST = astFactory.create(LT(1));
		match(COLON);
		aCase_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_43);
		} else {
			throw ex;
		}
	}
	returnAST = aCase_AST;
}

void JavaRecognizer::caseSList() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST caseSList_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		for (;;) {
			if ((_tokenSet_21.member(LA(1)))) {
				statement();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop105;
			}
			
		}
		_loop105:;
		}
		if ( inputState->guessing==0 ) {
			caseSList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 572 "java.g"
			caseSList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(SLIST,"SLIST"))->add(caseSList_AST)));
#line 3732 "JavaRecognizer.cpp"
			currentAST.root = caseSList_AST;
			if ( caseSList_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				caseSList_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = caseSList_AST->getFirstChild();
			else
				currentAST.child = caseSList_AST;
			currentAST.advanceChildToEnd();
		}
		caseSList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_42);
		} else {
			throw ex;
		}
	}
	returnAST = caseSList_AST;
}

void JavaRecognizer::expressionList() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST expressionList_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		expression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==COMMA)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp115_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp115_AST = astFactory.create(LT(1));
				match(COMMA);
				expression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop122;
			}
			
		}
		_loop122:;
		}
		if ( inputState->guessing==0 ) {
			expressionList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 653 "java.g"
			expressionList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(astFactory.make((new ANTLR_USE_NAMESPACE(antlr)ASTArray(2))->add(astFactory.create(ELIST,"ELIST"))->add(expressionList_AST)));
#line 3787 "JavaRecognizer.cpp"
			currentAST.root = expressionList_AST;
			if ( expressionList_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
				expressionList_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
				  currentAST.child = expressionList_AST->getFirstChild();
			else
				currentAST.child = expressionList_AST;
			currentAST.advanceChildToEnd();
		}
		expressionList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_44);
		} else {
			throw ex;
		}
	}
	returnAST = expressionList_AST;
}

void JavaRecognizer::handler() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST handler_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp116_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp116_AST = astFactory.create(LT(1));
			astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp116_AST));
		}
		match(LITERAL_catch);
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp117_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp117_AST = astFactory.create(LT(1));
		match(LPAREN);
		parameterDeclaration();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp118_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		tmp118_AST = astFactory.create(LT(1));
		match(RPAREN);
		compoundStatement();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		handler_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_45);
		} else {
			throw ex;
		}
	}
	returnAST = handler_AST;
}

void JavaRecognizer::assignmentExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST assignmentExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		conditionalExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		switch ( LA(1)) {
		case ASSIGN:
		case PLUS_ASSIGN:
		case MINUS_ASSIGN:
		case STAR_ASSIGN:
		case DIV_ASSIGN:
		case MOD_ASSIGN:
		case SR_ASSIGN:
		case BSR_ASSIGN:
		case SL_ASSIGN:
		case BAND_ASSIGN:
		case BXOR_ASSIGN:
		case BOR_ASSIGN:
		{
			{
			switch ( LA(1)) {
			case ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp119_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp119_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp119_AST));
				}
				match(ASSIGN);
				break;
			}
			case PLUS_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp120_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp120_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp120_AST));
				}
				match(PLUS_ASSIGN);
				break;
			}
			case MINUS_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp121_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp121_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp121_AST));
				}
				match(MINUS_ASSIGN);
				break;
			}
			case STAR_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp122_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp122_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp122_AST));
				}
				match(STAR_ASSIGN);
				break;
			}
			case DIV_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp123_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp123_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp123_AST));
				}
				match(DIV_ASSIGN);
				break;
			}
			case MOD_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp124_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp124_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp124_AST));
				}
				match(MOD_ASSIGN);
				break;
			}
			case SR_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp125_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp125_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp125_AST));
				}
				match(SR_ASSIGN);
				break;
			}
			case BSR_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp126_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp126_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp126_AST));
				}
				match(BSR_ASSIGN);
				break;
			}
			case SL_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp127_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp127_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp127_AST));
				}
				match(SL_ASSIGN);
				break;
			}
			case BAND_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp128_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp128_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp128_AST));
				}
				match(BAND_ASSIGN);
				break;
			}
			case BXOR_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp129_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp129_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp129_AST));
				}
				match(BXOR_ASSIGN);
				break;
			}
			case BOR_ASSIGN:
			{
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp130_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp130_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp130_AST));
				}
				match(BOR_ASSIGN);
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			assignmentExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		case SEMI:
		case RBRACK:
		case RCURLY:
		case COMMA:
		case RPAREN:
		case COLON:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		assignmentExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_31);
		} else {
			throw ex;
		}
	}
	returnAST = assignmentExpression_AST;
}

void JavaRecognizer::conditionalExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST conditionalExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		logicalOrExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		switch ( LA(1)) {
		case QUESTION:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp131_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp131_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp131_AST));
			}
			match(QUESTION);
			assignmentExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp132_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp132_AST = astFactory.create(LT(1));
			match(COLON);
			conditionalExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		case SEMI:
		case RBRACK:
		case RCURLY:
		case COMMA:
		case RPAREN:
		case ASSIGN:
		case COLON:
		case PLUS_ASSIGN:
		case MINUS_ASSIGN:
		case STAR_ASSIGN:
		case DIV_ASSIGN:
		case MOD_ASSIGN:
		case SR_ASSIGN:
		case BSR_ASSIGN:
		case SL_ASSIGN:
		case BAND_ASSIGN:
		case BXOR_ASSIGN:
		case BOR_ASSIGN:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		conditionalExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_46);
		} else {
			throw ex;
		}
	}
	returnAST = conditionalExpression_AST;
}

void JavaRecognizer::logicalOrExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST logicalOrExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		logicalAndExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==LOR)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp133_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp133_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp133_AST));
				}
				match(LOR);
				logicalAndExpression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop130;
			}
			
		}
		_loop130:;
		}
		logicalOrExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_47);
		} else {
			throw ex;
		}
	}
	returnAST = logicalOrExpression_AST;
}

void JavaRecognizer::logicalAndExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST logicalAndExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		inclusiveOrExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==LAND)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp134_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp134_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp134_AST));
				}
				match(LAND);
				inclusiveOrExpression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop133;
			}
			
		}
		_loop133:;
		}
		logicalAndExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_48);
		} else {
			throw ex;
		}
	}
	returnAST = logicalAndExpression_AST;
}

void JavaRecognizer::inclusiveOrExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST inclusiveOrExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		exclusiveOrExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==BOR)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp135_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp135_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp135_AST));
				}
				match(BOR);
				exclusiveOrExpression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop136;
			}
			
		}
		_loop136:;
		}
		inclusiveOrExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_49);
		} else {
			throw ex;
		}
	}
	returnAST = inclusiveOrExpression_AST;
}

void JavaRecognizer::exclusiveOrExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST exclusiveOrExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		andExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==BXOR)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp136_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp136_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp136_AST));
				}
				match(BXOR);
				andExpression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop139;
			}
			
		}
		_loop139:;
		}
		exclusiveOrExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_50);
		} else {
			throw ex;
		}
	}
	returnAST = exclusiveOrExpression_AST;
}

void JavaRecognizer::andExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST andExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		equalityExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==BAND)) {
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp137_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				if (inputState->guessing==0) {
					tmp137_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp137_AST));
				}
				match(BAND);
				equalityExpression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop142;
			}
			
		}
		_loop142:;
		}
		andExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_51);
		} else {
			throw ex;
		}
	}
	returnAST = andExpression_AST;
}

void JavaRecognizer::equalityExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST equalityExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		relationalExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==NOT_EQUAL||LA(1)==EQUAL)) {
				{
				switch ( LA(1)) {
				case NOT_EQUAL:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp138_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp138_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp138_AST));
					}
					match(NOT_EQUAL);
					break;
				}
				case EQUAL:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp139_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp139_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp139_AST));
					}
					match(EQUAL);
					break;
				}
				default:
				{
					throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
				}
				}
				}
				relationalExpression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop146;
			}
			
		}
		_loop146:;
		}
		equalityExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_52);
		} else {
			throw ex;
		}
	}
	returnAST = equalityExpression_AST;
}

void JavaRecognizer::relationalExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST relationalExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		shiftExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		switch ( LA(1)) {
		case SEMI:
		case RBRACK:
		case RCURLY:
		case COMMA:
		case RPAREN:
		case ASSIGN:
		case COLON:
		case PLUS_ASSIGN:
		case MINUS_ASSIGN:
		case STAR_ASSIGN:
		case DIV_ASSIGN:
		case MOD_ASSIGN:
		case SR_ASSIGN:
		case BSR_ASSIGN:
		case SL_ASSIGN:
		case BAND_ASSIGN:
		case BXOR_ASSIGN:
		case BOR_ASSIGN:
		case QUESTION:
		case LOR:
		case LAND:
		case BOR:
		case BXOR:
		case BAND:
		case NOT_EQUAL:
		case EQUAL:
		case LT_:
		case GT:
		case LE:
		case GE:
		{
			{
			for (;;) {
				if (((LA(1) >= LT_ && LA(1) <= GE))) {
					{
					switch ( LA(1)) {
					case LT_:
					{
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp140_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp140_AST = astFactory.create(LT(1));
							astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp140_AST));
						}
						match(LT_);
						break;
					}
					case GT:
					{
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp141_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp141_AST = astFactory.create(LT(1));
							astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp141_AST));
						}
						match(GT);
						break;
					}
					case LE:
					{
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp142_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp142_AST = astFactory.create(LT(1));
							astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp142_AST));
						}
						match(LE);
						break;
					}
					case GE:
					{
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp143_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp143_AST = astFactory.create(LT(1));
							astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp143_AST));
						}
						match(GE);
						break;
					}
					default:
					{
						throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
					}
					}
					}
					shiftExpression();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
				}
				else {
					goto _loop151;
				}
				
			}
			_loop151:;
			}
			break;
		}
		case LITERAL_instanceof:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp144_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp144_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp144_AST));
			}
			match(LITERAL_instanceof);
			typeSpec(true);
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		relationalExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_53);
		} else {
			throw ex;
		}
	}
	returnAST = relationalExpression_AST;
}

void JavaRecognizer::shiftExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST shiftExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		additiveExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if (((LA(1) >= SL && LA(1) <= BSR))) {
				{
				switch ( LA(1)) {
				case SL:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp145_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp145_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp145_AST));
					}
					match(SL);
					break;
				}
				case SR:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp146_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp146_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp146_AST));
					}
					match(SR);
					break;
				}
				case BSR:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp147_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp147_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp147_AST));
					}
					match(BSR);
					break;
				}
				default:
				{
					throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
				}
				}
				}
				additiveExpression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop155;
			}
			
		}
		_loop155:;
		}
		shiftExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_54);
		} else {
			throw ex;
		}
	}
	returnAST = shiftExpression_AST;
}

void JavaRecognizer::additiveExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST additiveExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		multiplicativeExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((LA(1)==PLUS||LA(1)==MINUS)) {
				{
				switch ( LA(1)) {
				case PLUS:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp148_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp148_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp148_AST));
					}
					match(PLUS);
					break;
				}
				case MINUS:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp149_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp149_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp149_AST));
					}
					match(MINUS);
					break;
				}
				default:
				{
					throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
				}
				}
				}
				multiplicativeExpression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop159;
			}
			
		}
		_loop159:;
		}
		additiveExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_55);
		} else {
			throw ex;
		}
	}
	returnAST = additiveExpression_AST;
}

void JavaRecognizer::multiplicativeExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST multiplicativeExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		unaryExpression();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		for (;;) {
			if ((_tokenSet_56.member(LA(1)))) {
				{
				switch ( LA(1)) {
				case STAR:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp150_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp150_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp150_AST));
					}
					match(STAR);
					break;
				}
				case DIV:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp151_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp151_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp151_AST));
					}
					match(DIV);
					break;
				}
				case MOD:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp152_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp152_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp152_AST));
					}
					match(MOD);
					break;
				}
				default:
				{
					throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
				}
				}
				}
				unaryExpression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				goto _loop163;
			}
			
		}
		_loop163:;
		}
		multiplicativeExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_57);
		} else {
			throw ex;
		}
	}
	returnAST = multiplicativeExpression_AST;
}

void JavaRecognizer::unaryExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST unaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case INC:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp153_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp153_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp153_AST));
			}
			match(INC);
			unaryExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			unaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case DEC:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp154_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp154_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp154_AST));
			}
			match(DEC);
			unaryExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			unaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case MINUS:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp155_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp155_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp155_AST));
			}
			match(MINUS);
			if ( inputState->guessing==0 ) {
#line 756 "java.g"
				tmp155_AST->setType(UNARY_MINUS);
#line 4815 "JavaRecognizer.cpp"
			}
			unaryExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			unaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case PLUS:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp156_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp156_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp156_AST));
			}
			match(PLUS);
			if ( inputState->guessing==0 ) {
#line 757 "java.g"
				tmp156_AST->setType(UNARY_PLUS);
#line 4835 "JavaRecognizer.cpp"
			}
			unaryExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			unaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		case IDENT:
		case LPAREN:
		case BNOT:
		case LNOT:
		case LITERAL_this:
		case LITERAL_super:
		case LITERAL_true:
		case LITERAL_false:
		case LITERAL_null:
		case LITERAL_new:
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		{
			unaryExpressionNotPlusMinus();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			unaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_58);
		} else {
			throw ex;
		}
	}
	returnAST = unaryExpression_AST;
}

void JavaRecognizer::unaryExpressionNotPlusMinus() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST unaryExpressionNotPlusMinus_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lpb = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lpb_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lp = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lp_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case BNOT:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp157_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp157_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp157_AST));
			}
			match(BNOT);
			unaryExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			unaryExpressionNotPlusMinus_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LNOT:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp158_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp158_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp158_AST));
			}
			match(LNOT);
			unaryExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			unaryExpressionNotPlusMinus_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		case IDENT:
		case LPAREN:
		case LITERAL_this:
		case LITERAL_super:
		case LITERAL_true:
		case LITERAL_false:
		case LITERAL_null:
		case LITERAL_new:
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		{
			{
			if ((LA(1)==LPAREN) && ((LA(2) >= LITERAL_void && LA(2) <= LITERAL_double))) {
				lpb = LT(1);
				if (inputState->guessing==0) {
					lpb_AST = astFactory.create(lpb);
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lpb_AST));
				}
				match(LPAREN);
				if ( inputState->guessing==0 ) {
#line 773 "java.g"
					lpb_AST->setType(TYPECAST);
#line 4967 "JavaRecognizer.cpp"
				}
				builtInTypeSpec(true);
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp159_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp159_AST = astFactory.create(LT(1));
				match(RPAREN);
				unaryExpression();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
			}
			else {
				bool synPredMatched168 = false;
				if (((LA(1)==LPAREN) && (LA(2)==IDENT))) {
					int _m168 = mark();
					synPredMatched168 = true;
					inputState->guessing++;
					try {
						{
						match(LPAREN);
						classTypeSpec(true);
						match(RPAREN);
						unaryExpressionNotPlusMinus();
						}
					}
					catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& pe) {
						synPredMatched168 = false;
					}
					rewind(_m168);
					inputState->guessing--;
				}
				if ( synPredMatched168 ) {
					lp = LT(1);
					if (inputState->guessing==0) {
						lp_AST = astFactory.create(lp);
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lp_AST));
					}
					match(LPAREN);
					if ( inputState->guessing==0 ) {
#line 780 "java.g"
						lp_AST->setType(TYPECAST);
#line 5011 "JavaRecognizer.cpp"
					}
					classTypeSpec(true);
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp160_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp160_AST = astFactory.create(LT(1));
					match(RPAREN);
					unaryExpressionNotPlusMinus();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
				}
				else if ((_tokenSet_59.member(LA(1))) && (_tokenSet_60.member(LA(2)))) {
					postfixExpression();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
				}
			else {
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			unaryExpressionNotPlusMinus_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_58);
		} else {
			throw ex;
		}
	}
	returnAST = unaryExpressionNotPlusMinus_AST;
}

void JavaRecognizer::postfixExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST postfixExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lbc = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lbc_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lb = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lb_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lp = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lp_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  in = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST in_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  de = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST de_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lbt = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lbt_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case IDENT:
		case LPAREN:
		case LITERAL_this:
		case LITERAL_super:
		case LITERAL_true:
		case LITERAL_false:
		case LITERAL_null:
		case LITERAL_new:
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		{
			primaryExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			{
			for (;;) {
				switch ( LA(1)) {
				case DOT:
				{
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp161_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					if (inputState->guessing==0) {
						tmp161_AST = astFactory.create(LT(1));
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp161_AST));
					}
					match(DOT);
					{
					switch ( LA(1)) {
					case IDENT:
					{
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp162_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp162_AST = astFactory.create(LT(1));
							astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp162_AST));
						}
						match(IDENT);
						break;
					}
					case LITERAL_this:
					{
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp163_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp163_AST = astFactory.create(LT(1));
							astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp163_AST));
						}
						match(LITERAL_this);
						break;
					}
					case LITERAL_class:
					{
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp164_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp164_AST = astFactory.create(LT(1));
							astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp164_AST));
						}
						match(LITERAL_class);
						break;
					}
					case LITERAL_new:
					{
						newExpression();
						if (inputState->guessing==0) {
							astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
						}
						break;
					}
					case LITERAL_super:
					{
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp165_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp165_AST = astFactory.create(LT(1));
							astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp165_AST));
						}
						match(LITERAL_super);
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp166_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp166_AST = astFactory.create(LT(1));
							astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp166_AST));
						}
						match(LPAREN);
						{
						switch ( LA(1)) {
						case LITERAL_void:
						case LITERAL_boolean:
						case LITERAL_byte:
						case LITERAL_char:
						case LITERAL_short:
						case LITERAL_int:
						case LITERAL_float:
						case LITERAL_long:
						case LITERAL_double:
						case IDENT:
						case LPAREN:
						case PLUS:
						case MINUS:
						case INC:
						case DEC:
						case BNOT:
						case LNOT:
						case LITERAL_this:
						case LITERAL_super:
						case LITERAL_true:
						case LITERAL_false:
						case LITERAL_null:
						case LITERAL_new:
						case NUM_INT:
						case CHAR_LITERAL:
						case STRING_LITERAL:
						case NUM_FLOAT:
						{
							expressionList();
							if (inputState->guessing==0) {
								astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
							}
							break;
						}
						case RPAREN:
						{
							break;
						}
						default:
						{
							throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
						}
						}
						}
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp167_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp167_AST = astFactory.create(LT(1));
							astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp167_AST));
						}
						match(RPAREN);
						break;
					}
					default:
					{
						throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
					}
					}
					}
					break;
				}
				case LPAREN:
				{
					lp = LT(1);
					if (inputState->guessing==0) {
						lp_AST = astFactory.create(lp);
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lp_AST));
					}
					match(LPAREN);
					if ( inputState->guessing==0 ) {
#line 813 "java.g"
						lp_AST->setType(METHOD_CALL);
#line 5231 "JavaRecognizer.cpp"
					}
					argList();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp168_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp168_AST = astFactory.create(LT(1));
					match(RPAREN);
					break;
				}
				default:
					if ((LA(1)==LBRACK) && (LA(2)==RBRACK)) {
						{
						int _cnt174=0;
						for (;;) {
							if ((LA(1)==LBRACK)) {
								lbc = LT(1);
								if (inputState->guessing==0) {
									lbc_AST = astFactory.create(lbc);
									astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lbc_AST));
								}
								match(LBRACK);
								if ( inputState->guessing==0 ) {
#line 802 "java.g"
									lbc_AST->setType(ARRAY_DECLARATOR);
#line 5257 "JavaRecognizer.cpp"
								}
								ANTLR_USE_NAMESPACE(antlr)RefAST tmp169_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
								tmp169_AST = astFactory.create(LT(1));
								match(RBRACK);
							}
							else {
								if ( _cnt174>=1 ) { goto _loop174; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
							}
							
							_cnt174++;
						}
						_loop174:;
						}
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp170_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp170_AST = astFactory.create(LT(1));
							astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp170_AST));
						}
						match(DOT);
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp171_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						if (inputState->guessing==0) {
							tmp171_AST = astFactory.create(LT(1));
							astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp171_AST));
						}
						match(LITERAL_class);
					}
					else if ((LA(1)==LBRACK) && (_tokenSet_38.member(LA(2)))) {
						lb = LT(1);
						if (inputState->guessing==0) {
							lb_AST = astFactory.create(lb);
							astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lb_AST));
						}
						match(LBRACK);
						if ( inputState->guessing==0 ) {
#line 806 "java.g"
							lb_AST->setType(INDEX_OP);
#line 5294 "JavaRecognizer.cpp"
						}
						expression();
						if (inputState->guessing==0) {
							astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
						}
						ANTLR_USE_NAMESPACE(antlr)RefAST tmp172_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
						tmp172_AST = astFactory.create(LT(1));
						match(RBRACK);
					}
				else {
					goto _loop175;
				}
				}
			}
			_loop175:;
			}
			{
			switch ( LA(1)) {
			case INC:
			{
				in = LT(1);
				if (inputState->guessing==0) {
					in_AST = astFactory.create(in);
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(in_AST));
				}
				match(INC);
				if ( inputState->guessing==0 ) {
#line 820 "java.g"
					in_AST->setType(POST_INC);
#line 5324 "JavaRecognizer.cpp"
				}
				break;
			}
			case DEC:
			{
				de = LT(1);
				if (inputState->guessing==0) {
					de_AST = astFactory.create(de);
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(de_AST));
				}
				match(DEC);
				if ( inputState->guessing==0 ) {
#line 821 "java.g"
					de_AST->setType(POST_DEC);
#line 5339 "JavaRecognizer.cpp"
				}
				break;
			}
			case SEMI:
			case RBRACK:
			case STAR:
			case RCURLY:
			case COMMA:
			case RPAREN:
			case ASSIGN:
			case COLON:
			case PLUS_ASSIGN:
			case MINUS_ASSIGN:
			case STAR_ASSIGN:
			case DIV_ASSIGN:
			case MOD_ASSIGN:
			case SR_ASSIGN:
			case BSR_ASSIGN:
			case SL_ASSIGN:
			case BAND_ASSIGN:
			case BXOR_ASSIGN:
			case BOR_ASSIGN:
			case QUESTION:
			case LOR:
			case LAND:
			case BOR:
			case BXOR:
			case BAND:
			case NOT_EQUAL:
			case EQUAL:
			case LT_:
			case GT:
			case LE:
			case GE:
			case LITERAL_instanceof:
			case SL:
			case SR:
			case BSR:
			case PLUS:
			case MINUS:
			case DIV:
			case MOD:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			postfixExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		{
			builtInType();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			{
			for (;;) {
				if ((LA(1)==LBRACK)) {
					lbt = LT(1);
					if (inputState->guessing==0) {
						lbt_AST = astFactory.create(lbt);
						astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lbt_AST));
					}
					match(LBRACK);
					if ( inputState->guessing==0 ) {
#line 827 "java.g"
						lbt_AST->setType(ARRAY_DECLARATOR);
#line 5420 "JavaRecognizer.cpp"
					}
					ANTLR_USE_NAMESPACE(antlr)RefAST tmp173_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
					tmp173_AST = astFactory.create(LT(1));
					match(RBRACK);
				}
				else {
					goto _loop178;
				}
				
			}
			_loop178:;
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp174_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp174_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp174_AST));
			}
			match(DOT);
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp175_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp175_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp175_AST));
			}
			match(LITERAL_class);
			postfixExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_58);
		} else {
			throw ex;
		}
	}
	returnAST = postfixExpression_AST;
}

void JavaRecognizer::primaryExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST primaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case IDENT:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp176_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp176_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp176_AST));
			}
			match(IDENT);
			primaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_new:
		{
			newExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			primaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		{
			constant();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			primaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_super:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp177_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp177_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp177_AST));
			}
			match(LITERAL_super);
			primaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_true:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp178_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp178_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp178_AST));
			}
			match(LITERAL_true);
			primaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_false:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp179_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp179_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp179_AST));
			}
			match(LITERAL_false);
			primaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_this:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp180_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp180_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp180_AST));
			}
			match(LITERAL_this);
			primaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LITERAL_null:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp181_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp181_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp181_AST));
			}
			match(LITERAL_null);
			primaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case LPAREN:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp182_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp182_AST = astFactory.create(LT(1));
			match(LPAREN);
			assignmentExpression();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp183_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp183_AST = astFactory.create(LT(1));
			match(RPAREN);
			primaryExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_30);
		} else {
			throw ex;
		}
	}
	returnAST = primaryExpression_AST;
}

/** object instantiation.
 *  Trees are built as illustrated by the following input/tree pairs:
 *  
 *  new T()
 *  
 *  new
 *   |
 *   T --  ELIST
 *           |
 *          arg1 -- arg2 -- .. -- argn
 *  
 *  new int[]
 *
 *  new
 *   |
 *  int -- ARRAY_DECLARATOR
 *  
 *  new int[] {1,2}
 *
 *  new
 *   |
 *  int -- ARRAY_DECLARATOR -- ARRAY_INIT
 *                                  |
 *                                EXPR -- EXPR
 *                                  |      |
 *                                  1      2
 *  
 *  new int[3]
 *  new
 *   |
 *  int -- ARRAY_DECLARATOR
 *                |
 *              EXPR
 *                |
 *                3
 *  
 *  new int[1][2]
 *  
 *  new
 *   |
 *  int -- ARRAY_DECLARATOR
 *               |
 *         ARRAY_DECLARATOR -- EXPR
 *               |              |
 *             EXPR             1
 *               |
 *               2
 *  
 */
void JavaRecognizer::newExpression() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST newExpression_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		ANTLR_USE_NAMESPACE(antlr)RefAST tmp184_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
		if (inputState->guessing==0) {
			tmp184_AST = astFactory.create(LT(1));
			astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp184_AST));
		}
		match(LITERAL_new);
		type();
		if (inputState->guessing==0) {
			astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
		}
		{
		switch ( LA(1)) {
		case LPAREN:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp185_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp185_AST = astFactory.create(LT(1));
			match(LPAREN);
			argList();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp186_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			tmp186_AST = astFactory.create(LT(1));
			match(RPAREN);
			{
			switch ( LA(1)) {
			case LCURLY:
			{
				classBlock();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
				break;
			}
			case SEMI:
			case LBRACK:
			case RBRACK:
			case DOT:
			case STAR:
			case RCURLY:
			case COMMA:
			case LPAREN:
			case RPAREN:
			case ASSIGN:
			case COLON:
			case PLUS_ASSIGN:
			case MINUS_ASSIGN:
			case STAR_ASSIGN:
			case DIV_ASSIGN:
			case MOD_ASSIGN:
			case SR_ASSIGN:
			case BSR_ASSIGN:
			case SL_ASSIGN:
			case BAND_ASSIGN:
			case BXOR_ASSIGN:
			case BOR_ASSIGN:
			case QUESTION:
			case LOR:
			case LAND:
			case BOR:
			case BXOR:
			case BAND:
			case NOT_EQUAL:
			case EQUAL:
			case LT_:
			case GT:
			case LE:
			case GE:
			case LITERAL_instanceof:
			case SL:
			case SR:
			case BSR:
			case PLUS:
			case MINUS:
			case DIV:
			case MOD:
			case INC:
			case DEC:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			break;
		}
		case LBRACK:
		{
			newArrayDeclarator();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			{
			switch ( LA(1)) {
			case LCURLY:
			{
				arrayInitializer();
				if (inputState->guessing==0) {
					astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
				}
				break;
			}
			case SEMI:
			case LBRACK:
			case RBRACK:
			case DOT:
			case STAR:
			case RCURLY:
			case COMMA:
			case LPAREN:
			case RPAREN:
			case ASSIGN:
			case COLON:
			case PLUS_ASSIGN:
			case MINUS_ASSIGN:
			case STAR_ASSIGN:
			case DIV_ASSIGN:
			case MOD_ASSIGN:
			case SR_ASSIGN:
			case BSR_ASSIGN:
			case SL_ASSIGN:
			case BAND_ASSIGN:
			case BXOR_ASSIGN:
			case BOR_ASSIGN:
			case QUESTION:
			case LOR:
			case LAND:
			case BOR:
			case BXOR:
			case BAND:
			case NOT_EQUAL:
			case EQUAL:
			case LT_:
			case GT:
			case LE:
			case GE:
			case LITERAL_instanceof:
			case SL:
			case SR:
			case BSR:
			case PLUS:
			case MINUS:
			case DIV:
			case MOD:
			case INC:
			case DEC:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		newExpression_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_30);
		} else {
			throw ex;
		}
	}
	returnAST = newExpression_AST;
}

void JavaRecognizer::argList() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST argList_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case LITERAL_void:
		case LITERAL_boolean:
		case LITERAL_byte:
		case LITERAL_char:
		case LITERAL_short:
		case LITERAL_int:
		case LITERAL_float:
		case LITERAL_long:
		case LITERAL_double:
		case IDENT:
		case LPAREN:
		case PLUS:
		case MINUS:
		case INC:
		case DEC:
		case BNOT:
		case LNOT:
		case LITERAL_this:
		case LITERAL_super:
		case LITERAL_true:
		case LITERAL_false:
		case LITERAL_null:
		case LITERAL_new:
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		{
			expressionList();
			if (inputState->guessing==0) {
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
			}
			break;
		}
		case RPAREN:
		{
			if ( inputState->guessing==0 ) {
				argList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
#line 912 "java.g"
				argList_AST = astFactory.create(ELIST,"ELIST");
#line 5876 "JavaRecognizer.cpp"
				currentAST.root = argList_AST;
				if ( argList_AST!=ANTLR_USE_NAMESPACE(antlr)nullAST &&
					argList_AST->getFirstChild() != ANTLR_USE_NAMESPACE(antlr)nullAST )
					  currentAST.child = argList_AST->getFirstChild();
				else
					currentAST.child = argList_AST;
				currentAST.advanceChildToEnd();
			}
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		argList_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_23);
		} else {
			throw ex;
		}
	}
	returnAST = argList_AST;
}

void JavaRecognizer::constant() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST constant_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		switch ( LA(1)) {
		case NUM_INT:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp187_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp187_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp187_AST));
			}
			match(NUM_INT);
			constant_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case CHAR_LITERAL:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp188_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp188_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp188_AST));
			}
			match(CHAR_LITERAL);
			constant_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case STRING_LITERAL:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp189_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp189_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp189_AST));
			}
			match(STRING_LITERAL);
			constant_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		case NUM_FLOAT:
		{
			ANTLR_USE_NAMESPACE(antlr)RefAST tmp190_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
			if (inputState->guessing==0) {
				tmp190_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(tmp190_AST));
			}
			match(NUM_FLOAT);
			constant_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_30);
		} else {
			throw ex;
		}
	}
	returnAST = constant_AST;
}

void JavaRecognizer::newArrayDeclarator() {
	returnAST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)ASTPair currentAST;
	ANTLR_USE_NAMESPACE(antlr)RefAST newArrayDeclarator_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	ANTLR_USE_NAMESPACE(antlr)RefToken  lb = ANTLR_USE_NAMESPACE(antlr)nullToken;
	ANTLR_USE_NAMESPACE(antlr)RefAST lb_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
	
	try {      // for error handling
		{
		int _cnt189=0;
		for (;;) {
			if ((LA(1)==LBRACK) && (_tokenSet_61.member(LA(2)))) {
				lb = LT(1);
				if (inputState->guessing==0) {
					lb_AST = astFactory.create(lb);
					astFactory.makeASTRoot(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(lb_AST));
				}
				match(LBRACK);
				if ( inputState->guessing==0 ) {
#line 927 "java.g"
					lb_AST->setType(ARRAY_DECLARATOR);
#line 5997 "JavaRecognizer.cpp"
				}
				{
				switch ( LA(1)) {
				case LITERAL_void:
				case LITERAL_boolean:
				case LITERAL_byte:
				case LITERAL_char:
				case LITERAL_short:
				case LITERAL_int:
				case LITERAL_float:
				case LITERAL_long:
				case LITERAL_double:
				case IDENT:
				case LPAREN:
				case PLUS:
				case MINUS:
				case INC:
				case DEC:
				case BNOT:
				case LNOT:
				case LITERAL_this:
				case LITERAL_super:
				case LITERAL_true:
				case LITERAL_false:
				case LITERAL_null:
				case LITERAL_new:
				case NUM_INT:
				case CHAR_LITERAL:
				case STRING_LITERAL:
				case NUM_FLOAT:
				{
					expression();
					if (inputState->guessing==0) {
						astFactory.addASTChild(currentAST, ANTLR_USE_NAMESPACE(antlr)RefAST(returnAST));
					}
					break;
				}
				case RBRACK:
				{
					break;
				}
				default:
				{
					throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
				}
				}
				}
				ANTLR_USE_NAMESPACE(antlr)RefAST tmp191_AST = ANTLR_USE_NAMESPACE(antlr)nullAST;
				tmp191_AST = astFactory.create(LT(1));
				match(RBRACK);
			}
			else {
				if ( _cnt189>=1 ) { goto _loop189; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt189++;
		}
		_loop189:;
		}
		newArrayDeclarator_AST = ANTLR_USE_NAMESPACE(antlr)RefAST(currentAST.root);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		if( inputState->guessing == 0 ) {
			reportError(ex);
			consume();
			consumeUntil(_tokenSet_62);
		} else {
			throw ex;
		}
	}
	returnAST = newArrayDeclarator_AST;
}

const char* JavaRecognizer::_tokenNames[] = {
	"<0>",
	"EOF",
	"<2>",
	"NULL_TREE_LOOKAHEAD",
	"BLOCK",
	"MODIFIERS",
	"OBJBLOCK",
	"SLIST",
	"CTOR_DEF",
	"METHOD_DEF",
	"VARIABLE_DEF",
	"INSTANCE_INIT",
	"STATIC_INIT",
	"TYPE",
	"CLASS_DEF",
	"INTERFACE_DEF",
	"PACKAGE_DEF",
	"ARRAY_DECLARATOR",
	"EXTENDS_CLAUSE",
	"IMPLEMENTS_CLAUSE",
	"PARAMETERS",
	"PARAMETER_DEF",
	"LABELED_STAT",
	"TYPECAST",
	"INDEX_OP",
	"POST_INC",
	"POST_DEC",
	"METHOD_CALL",
	"EXPR",
	"ARRAY_INIT",
	"IMPORT",
	"UNARY_MINUS",
	"UNARY_PLUS",
	"CASE_GROUP",
	"ELIST",
	"FOR_INIT",
	"FOR_CONDITION",
	"FOR_ITERATOR",
	"EMPTY_STAT",
	"\"final\"",
	"\"abstract\"",
	"\"strictfp\"",
	"\"package\"",
	"SEMI",
	"\"import\"",
	"LBRACK",
	"RBRACK",
	"\"void\"",
	"\"boolean\"",
	"\"byte\"",
	"\"char\"",
	"\"short\"",
	"\"int\"",
	"\"float\"",
	"\"long\"",
	"\"double\"",
	"IDENT",
	"DOT",
	"STAR",
	"\"private\"",
	"\"public\"",
	"\"protected\"",
	"\"static\"",
	"\"transient\"",
	"\"native\"",
	"\"threadsafe\"",
	"\"synchronized\"",
	"\"volatile\"",
	"\"class\"",
	"\"extends\"",
	"\"interface\"",
	"LCURLY",
	"RCURLY",
	"COMMA",
	"\"implements\"",
	"LPAREN",
	"RPAREN",
	"SL_COMMENT",
	"ML_COMMENT",
	"ASSIGN",
	"\"throws\"",
	"COLON",
	"\"if\"",
	"\"else\"",
	"\"for\"",
	"\"while\"",
	"\"do\"",
	"\"break\"",
	"\"continue\"",
	"\"return\"",
	"\"switch\"",
	"\"throw\"",
	"\"assert\"",
	"\"case\"",
	"\"default\"",
	"\"try\"",
	"\"finally\"",
	"\"catch\"",
	"PLUS_ASSIGN",
	"MINUS_ASSIGN",
	"STAR_ASSIGN",
	"DIV_ASSIGN",
	"MOD_ASSIGN",
	"SR_ASSIGN",
	"BSR_ASSIGN",
	"SL_ASSIGN",
	"BAND_ASSIGN",
	"BXOR_ASSIGN",
	"BOR_ASSIGN",
	"QUESTION",
	"LOR",
	"LAND",
	"BOR",
	"BXOR",
	"BAND",
	"NOT_EQUAL",
	"EQUAL",
	"LT_",
	"GT",
	"LE",
	"GE",
	"\"instanceof\"",
	"SL",
	"SR",
	"BSR",
	"PLUS",
	"MINUS",
	"DIV",
	"MOD",
	"INC",
	"DEC",
	"BNOT",
	"LNOT",
	"\"this\"",
	"\"super\"",
	"\"true\"",
	"\"false\"",
	"\"null\"",
	"\"new\"",
	"NUM_INT",
	"CHAR_LITERAL",
	"STRING_LITERAL",
	"NUM_FLOAT",
	"WS_",
	"SL_CONDITIONAL",
	"ESC",
	"HEX_DIGIT",
	"VOCAB",
	"EXPONENT",
	"FLOAT_SUFFIX",
	0
};

const unsigned long JavaRecognizer::_tokenSet_0_data_[] = { 0UL, 4160752512UL, 95UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" SEMI "private" "public" "protected" "static" 
// "transient" "native" "threadsafe" "synchronized" "volatile" "class" 
// "interface" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_0(_tokenSet_0_data_,8);
const unsigned long JavaRecognizer::_tokenSet_1_data_[] = { 2UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// EOF 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_1(_tokenSet_1_data_,6);
const unsigned long JavaRecognizer::_tokenSet_2_data_[] = { 2UL, 4160756608UL, 95UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// EOF "final" "abstract" "strictfp" SEMI "import" "private" "public" "protected" 
// "static" "transient" "native" "threadsafe" "synchronized" "volatile" 
// "class" "interface" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_2(_tokenSet_2_data_,8);
const unsigned long JavaRecognizer::_tokenSet_3_data_[] = { 2UL, 4160752512UL, 95UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// EOF "final" "abstract" "strictfp" SEMI "private" "public" "protected" 
// "static" "transient" "native" "threadsafe" "synchronized" "volatile" 
// "class" "interface" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_3(_tokenSet_3_data_,8);
const unsigned long JavaRecognizer::_tokenSet_4_data_[] = { 0UL, 16803840UL, 171904UL, 2097148UL, 0UL, 0UL, 0UL, 0UL };
// SEMI LBRACK RBRACK IDENT LCURLY RCURLY COMMA "implements" LPAREN RPAREN 
// ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN 
// SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN BXOR_ASSIGN BOR_ASSIGN QUESTION 
// LOR LAND BOR BXOR BAND NOT_EQUAL EQUAL 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_4(_tokenSet_4_data_,8);
const unsigned long JavaRecognizer::_tokenSet_5_data_[] = { 0UL, 2048UL, 0UL, 0UL, 0UL, 0UL };
// SEMI 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_5(_tokenSet_5_data_,6);
const unsigned long JavaRecognizer::_tokenSet_6_data_[] = { 0UL, 4160750464UL, 15UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" "private" "public" "protected" "static" 
// "transient" "native" "threadsafe" "synchronized" "volatile" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_6(_tokenSet_6_data_,8);
const unsigned long JavaRecognizer::_tokenSet_7_data_[] = { 0UL, 33521664UL, 80UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "void" "boolean" "byte" "char" "short" "int" "float" "long" "double" 
// IDENT "class" "interface" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_7(_tokenSet_7_data_,8);
const unsigned long JavaRecognizer::_tokenSet_8_data_[] = { 2UL, 4194274176UL, 4294732255UL, 1610612736UL, 32766UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// EOF "final" "abstract" "strictfp" SEMI "void" "boolean" "byte" "char" 
// "short" "int" "float" "long" "double" IDENT "private" "public" "protected" 
// "static" "transient" "native" "threadsafe" "synchronized" "volatile" 
// "class" "interface" LCURLY RCURLY LPAREN SL_COMMENT ML_COMMENT "if" 
// "else" "for" "while" "do" "break" "continue" "return" "switch" "throw" 
// "assert" "case" "default" "try" PLUS MINUS INC DEC BNOT LNOT "this" 
// "super" "true" "false" "null" "new" NUM_INT CHAR_LITERAL STRING_LITERAL 
// NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_8(_tokenSet_8_data_,12);
const unsigned long JavaRecognizer::_tokenSet_9_data_[] = { 2UL, 4194274176UL, 25055UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// EOF "final" "abstract" "strictfp" SEMI "void" "boolean" "byte" "char" 
// "short" "int" "float" "long" "double" IDENT "private" "public" "protected" 
// "static" "transient" "native" "threadsafe" "synchronized" "volatile" 
// "class" "interface" LCURLY RCURLY SL_COMMENT ML_COMMENT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_9(_tokenSet_9_data_,8);
const unsigned long JavaRecognizer::_tokenSet_10_data_[] = { 0UL, 16795648UL, 168704UL, 2097148UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK IDENT RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR BAND NOT_EQUAL EQUAL 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_10(_tokenSet_10_data_,8);
const unsigned long JavaRecognizer::_tokenSet_11_data_[] = { 0UL, 4194272128UL, 95UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" "void" "boolean" "byte" "char" "short" 
// "int" "float" "long" "double" IDENT "private" "public" "protected" "static" 
// "transient" "native" "threadsafe" "synchronized" "volatile" "class" 
// "interface" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_11(_tokenSet_11_data_,8);
const unsigned long JavaRecognizer::_tokenSet_12_data_[] = { 0UL, 50358272UL, 170752UL, 2097148UL, 0UL, 0UL, 0UL, 0UL };
// SEMI LBRACK RBRACK IDENT DOT RCURLY COMMA LPAREN RPAREN ASSIGN COLON 
// PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN 
// BSR_ASSIGN SL_ASSIGN BAND_ASSIGN BXOR_ASSIGN BOR_ASSIGN QUESTION LOR 
// LAND BOR BXOR BAND NOT_EQUAL EQUAL 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_12(_tokenSet_12_data_,8);
const unsigned long JavaRecognizer::_tokenSet_13_data_[] = { 0UL, 8192UL, 2048UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// LBRACK LPAREN 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_13(_tokenSet_13_data_,8);
const unsigned long JavaRecognizer::_tokenSet_14_data_[] = { 0UL, 0UL, 1152UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// LCURLY "implements" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_14(_tokenSet_14_data_,8);
const unsigned long JavaRecognizer::_tokenSet_15_data_[] = { 0UL, 0UL, 128UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// LCURLY 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_15(_tokenSet_15_data_,8);
const unsigned long JavaRecognizer::_tokenSet_16_data_[] = { 2UL, 4294962048UL, 4294900703UL, 4294967292UL, 32767UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// EOF "final" "abstract" "strictfp" SEMI LBRACK RBRACK "void" "boolean" 
// "byte" "char" "short" "int" "float" "long" "double" IDENT DOT STAR "private" 
// "public" "protected" "static" "transient" "native" "threadsafe" "synchronized" 
// "volatile" "class" "interface" LCURLY RCURLY COMMA LPAREN RPAREN SL_COMMENT 
// ML_COMMENT ASSIGN COLON "if" "else" "for" "while" "do" "break" "continue" 
// "return" "switch" "throw" "assert" "case" "default" "try" PLUS_ASSIGN 
// MINUS_ASSIGN STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN 
// SL_ASSIGN BAND_ASSIGN BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR 
// BAND NOT_EQUAL EQUAL LT_ GT LE GE "instanceof" SL SR BSR PLUS MINUS 
// DIV MOD INC DEC BNOT LNOT "this" "super" "true" "false" "null" "new" 
// NUM_INT CHAR_LITERAL STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_16(_tokenSet_16_data_,12);
const unsigned long JavaRecognizer::_tokenSet_17_data_[] = { 0UL, 4227834752UL, 2143UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" LBRACK "void" "boolean" "byte" "char" 
// "short" "int" "float" "long" "double" IDENT DOT "private" "public" "protected" 
// "static" "transient" "native" "threadsafe" "synchronized" "volatile" 
// "class" "interface" LPAREN 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_17(_tokenSet_17_data_,8);
const unsigned long JavaRecognizer::_tokenSet_18_data_[] = { 0UL, 50339840UL, 0UL, 0UL, 0UL, 0UL };
// LBRACK IDENT DOT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_18(_tokenSet_18_data_,6);
const unsigned long JavaRecognizer::_tokenSet_19_data_[] = { 0UL, 10240UL, 33280UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI LBRACK COMMA ASSIGN 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_19(_tokenSet_19_data_,8);
const unsigned long JavaRecognizer::_tokenSet_20_data_[] = { 0UL, 4194274176UL, 25055UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" SEMI "void" "boolean" "byte" "char" "short" 
// "int" "float" "long" "double" IDENT "private" "public" "protected" "static" 
// "transient" "native" "threadsafe" "synchronized" "volatile" "class" 
// "interface" LCURLY RCURLY SL_COMMENT ML_COMMENT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_20(_tokenSet_20_data_,8);
const unsigned long JavaRecognizer::_tokenSet_21_data_[] = { 0UL, 4194274176UL, 2683570335UL, 1610612736UL, 32766UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" SEMI "void" "boolean" "byte" "char" "short" 
// "int" "float" "long" "double" IDENT "private" "public" "protected" "static" 
// "transient" "native" "threadsafe" "synchronized" "volatile" "class" 
// LCURLY LPAREN "if" "for" "while" "do" "break" "continue" "return" "switch" 
// "throw" "assert" "try" PLUS MINUS INC DEC BNOT LNOT "this" "super" "true" 
// "false" "null" "new" NUM_INT CHAR_LITERAL STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_21(_tokenSet_21_data_,12);
const unsigned long JavaRecognizer::_tokenSet_22_data_[] = { 0UL, 4194274176UL, 4294732255UL, 1610612739UL, 32766UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" SEMI "void" "boolean" "byte" "char" "short" 
// "int" "float" "long" "double" IDENT "private" "public" "protected" "static" 
// "transient" "native" "threadsafe" "synchronized" "volatile" "class" 
// "interface" LCURLY RCURLY LPAREN SL_COMMENT ML_COMMENT "if" "else" "for" 
// "while" "do" "break" "continue" "return" "switch" "throw" "assert" "case" 
// "default" "try" "finally" "catch" PLUS MINUS INC DEC BNOT LNOT "this" 
// "super" "true" "false" "null" "new" NUM_INT CHAR_LITERAL STRING_LITERAL 
// NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_22(_tokenSet_22_data_,12);
const unsigned long JavaRecognizer::_tokenSet_23_data_[] = { 0UL, 0UL, 4096UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// RPAREN 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_23(_tokenSet_23_data_,8);
const unsigned long JavaRecognizer::_tokenSet_24_data_[] = { 0UL, 2048UL, 65664UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI LCURLY "throws" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_24(_tokenSet_24_data_,8);
const unsigned long JavaRecognizer::_tokenSet_25_data_[] = { 0UL, 2048UL, 128UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI LCURLY 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_25(_tokenSet_25_data_,8);
const unsigned long JavaRecognizer::_tokenSet_26_data_[] = { 0UL, 2048UL, 512UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI COMMA 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_26(_tokenSet_26_data_,8);
const unsigned long JavaRecognizer::_tokenSet_27_data_[] = { 0UL, 2048UL, 33280UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI COMMA ASSIGN 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_27(_tokenSet_27_data_,8);
const unsigned long JavaRecognizer::_tokenSet_28_data_[] = { 0UL, 2048UL, 768UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RCURLY COMMA 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_28(_tokenSet_28_data_,8);
const unsigned long JavaRecognizer::_tokenSet_29_data_[] = { 0UL, 33521664UL, 2176UL, 1610612736UL, 32766UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "void" "boolean" "byte" "char" "short" "int" "float" "long" "double" 
// IDENT LCURLY LPAREN PLUS MINUS INC DEC BNOT LNOT "this" "super" "true" 
// "false" "null" "new" NUM_INT CHAR_LITERAL STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_29(_tokenSet_29_data_,12);
const unsigned long JavaRecognizer::_tokenSet_30_data_[] = { 0UL, 100689920UL, 170752UL, 4294967292UL, 7UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI LBRACK RBRACK DOT STAR RCURLY COMMA LPAREN RPAREN ASSIGN COLON 
// PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN 
// BSR_ASSIGN SL_ASSIGN BAND_ASSIGN BXOR_ASSIGN BOR_ASSIGN QUESTION LOR 
// LAND BOR BXOR BAND NOT_EQUAL EQUAL LT_ GT LE GE "instanceof" SL SR BSR 
// PLUS MINUS DIV MOD INC DEC 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_30(_tokenSet_30_data_,12);
const unsigned long JavaRecognizer::_tokenSet_31_data_[] = { 0UL, 18432UL, 135936UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN COLON 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_31(_tokenSet_31_data_,8);
const unsigned long JavaRecognizer::_tokenSet_32_data_[] = { 0UL, 0UL, 4608UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// COMMA RPAREN 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_32(_tokenSet_32_data_,8);
const unsigned long JavaRecognizer::_tokenSet_33_data_[] = { 0UL, 33521664UL, 0UL, 0UL, 0UL, 0UL };
// "void" "boolean" "byte" "char" "short" "int" "float" "long" "double" 
// IDENT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_33(_tokenSet_33_data_,6);
const unsigned long JavaRecognizer::_tokenSet_34_data_[] = { 0UL, 4194274176UL, 4294707615UL, 1610612736UL, 32766UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" SEMI "void" "boolean" "byte" "char" "short" 
// "int" "float" "long" "double" IDENT "private" "public" "protected" "static" 
// "transient" "native" "threadsafe" "synchronized" "volatile" "class" 
// LCURLY RCURLY LPAREN "if" "else" "for" "while" "do" "break" "continue" 
// "return" "switch" "throw" "assert" "case" "default" "try" PLUS MINUS 
// INC DEC BNOT LNOT "this" "super" "true" "false" "null" "new" NUM_INT 
// CHAR_LITERAL STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_34(_tokenSet_34_data_,12);
const unsigned long JavaRecognizer::_tokenSet_35_data_[] = { 0UL, 4294945664UL, 4294896095UL, 4294967295UL, 32767UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" SEMI LBRACK "void" "boolean" "byte" "char" 
// "short" "int" "float" "long" "double" IDENT DOT STAR "private" "public" 
// "protected" "static" "transient" "native" "threadsafe" "synchronized" 
// "volatile" "class" "interface" LCURLY RCURLY LPAREN SL_COMMENT ML_COMMENT 
// ASSIGN COLON "if" "else" "for" "while" "do" "break" "continue" "return" 
// "switch" "throw" "assert" "case" "default" "try" "finally" "catch" PLUS_ASSIGN 
// MINUS_ASSIGN STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN 
// SL_ASSIGN BAND_ASSIGN BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR 
// BAND NOT_EQUAL EQUAL LT_ GT LE GE "instanceof" SL SR BSR PLUS MINUS 
// DIV MOD INC DEC BNOT LNOT "this" "super" "true" "false" "null" "new" 
// NUM_INT CHAR_LITERAL STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_35(_tokenSet_35_data_,12);
const unsigned long JavaRecognizer::_tokenSet_36_data_[] = { 0UL, 4194272128UL, 15UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" "void" "boolean" "byte" "char" "short" 
// "int" "float" "long" "double" IDENT "private" "public" "protected" "static" 
// "transient" "native" "threadsafe" "synchronized" "volatile" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_36(_tokenSet_36_data_,8);
const unsigned long JavaRecognizer::_tokenSet_37_data_[] = { 0UL, 4227834752UL, 15UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" LBRACK "void" "boolean" "byte" "char" 
// "short" "int" "float" "long" "double" IDENT DOT "private" "public" "protected" 
// "static" "transient" "native" "threadsafe" "synchronized" "volatile" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_37(_tokenSet_37_data_,8);
const unsigned long JavaRecognizer::_tokenSet_38_data_[] = { 0UL, 33521664UL, 2048UL, 1610612736UL, 32766UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "void" "boolean" "byte" "char" "short" "int" "float" "long" "double" 
// IDENT LPAREN PLUS MINUS INC DEC BNOT LNOT "this" "super" "true" "false" 
// "null" "new" NUM_INT CHAR_LITERAL STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_38(_tokenSet_38_data_,12);
const unsigned long JavaRecognizer::_tokenSet_39_data_[] = { 0UL, 134195200UL, 34816UL, 4294967292UL, 32767UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI LBRACK "void" "boolean" "byte" "char" "short" "int" "float" "long" 
// "double" IDENT DOT STAR LPAREN ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN 
// DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN BXOR_ASSIGN 
// BOR_ASSIGN QUESTION LOR LAND BOR BXOR BAND NOT_EQUAL EQUAL LT_ GT LE 
// GE "instanceof" SL SR BSR PLUS MINUS DIV MOD INC DEC BNOT LNOT "this" 
// "super" "true" "false" "null" "new" NUM_INT CHAR_LITERAL STRING_LITERAL 
// NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_39(_tokenSet_39_data_,12);
const unsigned long JavaRecognizer::_tokenSet_40_data_[] = { 0UL, 134195200UL, 35328UL, 4294967292UL, 32767UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI LBRACK "void" "boolean" "byte" "char" "short" "int" "float" "long" 
// "double" IDENT DOT STAR COMMA LPAREN ASSIGN PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR BAND NOT_EQUAL EQUAL 
// LT_ GT LE GE "instanceof" SL SR BSR PLUS MINUS DIV MOD INC DEC BNOT 
// LNOT "this" "super" "true" "false" "null" "new" NUM_INT CHAR_LITERAL 
// STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_40(_tokenSet_40_data_,12);
const unsigned long JavaRecognizer::_tokenSet_41_data_[] = { 0UL, 33521664UL, 133120UL, 1610612736UL, 32766UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "void" "boolean" "byte" "char" "short" "int" "float" "long" "double" 
// IDENT LPAREN COLON PLUS MINUS INC DEC BNOT LNOT "this" "super" "true" 
// "false" "null" "new" NUM_INT CHAR_LITERAL STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_41(_tokenSet_41_data_,12);
const unsigned long JavaRecognizer::_tokenSet_42_data_[] = { 0UL, 0UL, 1610612992UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// RCURLY "case" "default" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_42(_tokenSet_42_data_,8);
const unsigned long JavaRecognizer::_tokenSet_43_data_[] = { 0UL, 4194274176UL, 4294183327UL, 1610612736UL, 32766UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" SEMI "void" "boolean" "byte" "char" "short" 
// "int" "float" "long" "double" IDENT "private" "public" "protected" "static" 
// "transient" "native" "threadsafe" "synchronized" "volatile" "class" 
// LCURLY RCURLY LPAREN "if" "for" "while" "do" "break" "continue" "return" 
// "switch" "throw" "assert" "case" "default" "try" PLUS MINUS INC DEC 
// BNOT LNOT "this" "super" "true" "false" "null" "new" NUM_INT CHAR_LITERAL 
// STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_43(_tokenSet_43_data_,12);
const unsigned long JavaRecognizer::_tokenSet_44_data_[] = { 0UL, 2048UL, 4096UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RPAREN 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_44(_tokenSet_44_data_,8);
const unsigned long JavaRecognizer::_tokenSet_45_data_[] = { 0UL, 4194274176UL, 4294707615UL, 1610612739UL, 32766UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "final" "abstract" "strictfp" SEMI "void" "boolean" "byte" "char" "short" 
// "int" "float" "long" "double" IDENT "private" "public" "protected" "static" 
// "transient" "native" "threadsafe" "synchronized" "volatile" "class" 
// LCURLY RCURLY LPAREN "if" "else" "for" "while" "do" "break" "continue" 
// "return" "switch" "throw" "assert" "case" "default" "try" "finally" 
// "catch" PLUS MINUS INC DEC BNOT LNOT "this" "super" "true" "false" "null" 
// "new" NUM_INT CHAR_LITERAL STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_45(_tokenSet_45_data_,12);
const unsigned long JavaRecognizer::_tokenSet_46_data_[] = { 0UL, 18432UL, 168704UL, 8188UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_46(_tokenSet_46_data_,8);
const unsigned long JavaRecognizer::_tokenSet_47_data_[] = { 0UL, 18432UL, 168704UL, 16380UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_47(_tokenSet_47_data_,8);
const unsigned long JavaRecognizer::_tokenSet_48_data_[] = { 0UL, 18432UL, 168704UL, 32764UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_48(_tokenSet_48_data_,8);
const unsigned long JavaRecognizer::_tokenSet_49_data_[] = { 0UL, 18432UL, 168704UL, 65532UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_49(_tokenSet_49_data_,8);
const unsigned long JavaRecognizer::_tokenSet_50_data_[] = { 0UL, 18432UL, 168704UL, 131068UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_50(_tokenSet_50_data_,8);
const unsigned long JavaRecognizer::_tokenSet_51_data_[] = { 0UL, 18432UL, 168704UL, 262140UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_51(_tokenSet_51_data_,8);
const unsigned long JavaRecognizer::_tokenSet_52_data_[] = { 0UL, 18432UL, 168704UL, 524284UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR BAND 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_52(_tokenSet_52_data_,8);
const unsigned long JavaRecognizer::_tokenSet_53_data_[] = { 0UL, 18432UL, 168704UL, 2097148UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR BAND NOT_EQUAL EQUAL 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_53(_tokenSet_53_data_,8);
const unsigned long JavaRecognizer::_tokenSet_54_data_[] = { 0UL, 18432UL, 168704UL, 67108860UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR BAND NOT_EQUAL EQUAL 
// LT_ GT LE GE "instanceof" 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_54(_tokenSet_54_data_,8);
const unsigned long JavaRecognizer::_tokenSet_55_data_[] = { 0UL, 18432UL, 168704UL, 536870908UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR BAND NOT_EQUAL EQUAL 
// LT_ GT LE GE "instanceof" SL SR BSR 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_55(_tokenSet_55_data_,8);
const unsigned long JavaRecognizer::_tokenSet_56_data_[] = { 0UL, 67108864UL, 0UL, 2147483648UL, 1UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// STAR DIV MOD 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_56(_tokenSet_56_data_,12);
const unsigned long JavaRecognizer::_tokenSet_57_data_[] = { 0UL, 18432UL, 168704UL, 2147483644UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR BAND NOT_EQUAL EQUAL 
// LT_ GT LE GE "instanceof" SL SR BSR PLUS MINUS 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_57(_tokenSet_57_data_,8);
const unsigned long JavaRecognizer::_tokenSet_58_data_[] = { 0UL, 67127296UL, 168704UL, 4294967292UL, 1UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI RBRACK STAR RCURLY COMMA RPAREN ASSIGN COLON PLUS_ASSIGN MINUS_ASSIGN 
// STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN BSR_ASSIGN SL_ASSIGN BAND_ASSIGN 
// BXOR_ASSIGN BOR_ASSIGN QUESTION LOR LAND BOR BXOR BAND NOT_EQUAL EQUAL 
// LT_ GT LE GE "instanceof" SL SR BSR PLUS MINUS DIV MOD 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_58(_tokenSet_58_data_,12);
const unsigned long JavaRecognizer::_tokenSet_59_data_[] = { 0UL, 33521664UL, 2048UL, 0UL, 32736UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// "void" "boolean" "byte" "char" "short" "int" "float" "long" "double" 
// IDENT LPAREN "this" "super" "true" "false" "null" "new" NUM_INT CHAR_LITERAL 
// STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_59(_tokenSet_59_data_,12);
const unsigned long JavaRecognizer::_tokenSet_60_data_[] = { 0UL, 134211584UL, 170752UL, 4294967292UL, 32767UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI LBRACK RBRACK "void" "boolean" "byte" "char" "short" "int" "float" 
// "long" "double" IDENT DOT STAR RCURLY COMMA LPAREN RPAREN ASSIGN COLON 
// PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN 
// BSR_ASSIGN SL_ASSIGN BAND_ASSIGN BXOR_ASSIGN BOR_ASSIGN QUESTION LOR 
// LAND BOR BXOR BAND NOT_EQUAL EQUAL LT_ GT LE GE "instanceof" SL SR BSR 
// PLUS MINUS DIV MOD INC DEC BNOT LNOT "this" "super" "true" "false" "null" 
// "new" NUM_INT CHAR_LITERAL STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_60(_tokenSet_60_data_,12);
const unsigned long JavaRecognizer::_tokenSet_61_data_[] = { 0UL, 33538048UL, 2048UL, 1610612736UL, 32766UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// RBRACK "void" "boolean" "byte" "char" "short" "int" "float" "long" "double" 
// IDENT LPAREN PLUS MINUS INC DEC BNOT LNOT "this" "super" "true" "false" 
// "null" "new" NUM_INT CHAR_LITERAL STRING_LITERAL NUM_FLOAT 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_61(_tokenSet_61_data_,12);
const unsigned long JavaRecognizer::_tokenSet_62_data_[] = { 0UL, 100689920UL, 170880UL, 4294967292UL, 7UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// SEMI LBRACK RBRACK DOT STAR LCURLY RCURLY COMMA LPAREN RPAREN ASSIGN 
// COLON PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN SR_ASSIGN 
// BSR_ASSIGN SL_ASSIGN BAND_ASSIGN BXOR_ASSIGN BOR_ASSIGN QUESTION LOR 
// LAND BOR BXOR BAND NOT_EQUAL EQUAL LT_ GT LE GE "instanceof" SL SR BSR 
// PLUS MINUS DIV MOD INC DEC 
const ANTLR_USE_NAMESPACE(antlr)BitSet JavaRecognizer::_tokenSet_62(_tokenSet_62_data_,12);


