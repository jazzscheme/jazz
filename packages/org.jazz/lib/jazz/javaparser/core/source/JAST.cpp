//==============
//	JazzScheme
//==============
//
///	Abstract Syntax Trees
//	
//	The contents of this file are subject to the Mozilla Public License Version
//	1.1 (the "License"); you may not use this file except in compliance with
//	the License. You may obtain a copy of the License at
//	http://www.mozilla.org/MPL/
//
//	Software distributed under the License is distributed on an "AS IS" basis,
//	WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
//	for the specific language governing rights and limitations under the
//	License.
//
//	The Original Code is JazzScheme.
//
//	The Initial Developer of the Original Code is Guillaume Cartier.
//	Portions created by the Initial Developer are Copyright (C) 1996-2007
//	the Initial Developer. All Rights Reserved.
//
//	Contributor(s):
//
//	Alternatively, the contents of this file may be used under the terms of
//	the GNU General Public License Version 2 or later (the "GPL"), in which
//	case the provisions of the GPL are applicable instead of those above. If
//	you wish to allow use of your version of this file only under the terms of
//	the GPL, and not to allow others to use your version of this file under the
//	terms of the MPL, indicate your decision by deleting the provisions above
//	and replace them with the notice and other provisions required by the GPL.
//	If you do not delete the provisions above, a recipient may use your version
//	of this file under the terms of any one of the MPL or the GPL.
//
//	See www.jazzscheme.org for details.



#include "Jazz.h"
#include "JBoot.h"
#include "JAST.h"
#include "JAnsiString.h"



#include "JavaTokenTypes.hpp"



//#include "antlr/CommonASTWithHiddenTokens.hpp"



//-------
///	AST
//-------



DEFINE_CLASS(L"AST", AST, Object)



METHOD jObject CAST::allocate(jType model, jObjectPtr rest)
{
	return newAST(castClass(model), NULL);
}



CONSTRUCTOR JAST::JAST(cObject model, RefAST a) : JObject(model)
{
	INIT(ast, a)
	
	header.set_initialized();
}



EXTERN tBool is_ast_locatable(RefAST ast)
{
	tInt	type;

	type = ast->getType();

	return
		type != JavaTokenTypes::CLASS_DEF &&
		type != JavaTokenTypes::CTOR_DEF &&
		type != JavaTokenTypes::ELIST &&
		type != JavaTokenTypes::EXPR &&
		type != JavaTokenTypes::EXTENDS_CLAUSE &&
		type != JavaTokenTypes::IMPLEMENTS_CLAUSE &&
		type != JavaTokenTypes::INTERFACE_DEF &&
		type != JavaTokenTypes::METHOD_DEF &&
		type != JavaTokenTypes::MODIFIERS &&
		type != JavaTokenTypes::OBJBLOCK &&
		type != JavaTokenTypes::PARAMETER_DEF &&
		type != JavaTokenTypes::PARAMETERS &&
		type != JavaTokenTypes::LITERAL_double &&
		type != JavaTokenTypes::LITERAL_int &&
		type != JavaTokenTypes::LITERAL_long &&
		type != JavaTokenTypes::STATIC_INIT &&
		type != JavaTokenTypes::TYPE &&
		type != JavaTokenTypes::VARIABLE_DEF;
}



//------------
///	External
//------------



JAZZAPI tBOOL JAZZCALL JzIsASTNull(jObject ast)
{
	return castAST(ast)->astGet() == NULL;
}



JAZZAPI tInt JAZZCALL JzASTKind(jObject ast)
{
	return castAST(ast)->astGet()->getType();
}



JAZZAPI tBOOL JAZZCALL JzASTLocatable(jObject ast)
{
	return is_ast_locatable(castAST(ast)->astGet());
}



JAZZAPI tInt JAZZCALL JzASTLine(jObject ast)
{
	return castAST(ast)->astGet()->getLine();
}



JAZZAPI tInt JAZZCALL JzASTColumn(jObject ast)
{
	return castAST(ast)->astGet()->getColumn();
}



JAZZAPI jObject JAZZCALL JzASTText(jObject ast)
{
	return newAnsiString(getAnsiStringClass(), (tAnsiString) castAST(ast)->astGet()->getText().c_str());
}



JAZZAPI jObject JAZZCALL JzASTFirstChild(jObject ast)
{
	jAST	node;
	RefAST	child;

	node = castAST(ast);
	child = node->astGet()->getFirstChild();
	if (child)
		return newAST(node->get_class(), child);
	else
		return Nil;
}



JAZZAPI jObject JAZZCALL JzASTNextSibling(jObject ast)
{
	jAST	node;
	RefAST	sibling;

	node = castAST(ast);
	sibling = node->astGet()->getNextSibling();
	if (sibling)
		return newAST(node->get_class(), sibling);
	else
		return Nil;
}



JAZZAPI jObject JAZZCALL JzASTHiddenAfter(jObject ast)
{
//	getHiddenAfter();
	return Nil;
}
