//==============
//	JazzScheme
//==============
//
///	AST Header
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



#pragma once
#ifndef JAST_H_JAZZ
#define JAST_H_JAZZ



#include "antlr/AST.hpp"
//#include "antlr/CommonASTWithHiddenTokens.hpp"



using namespace std;
using namespace antlr;



/**
 * This class implements Antlr abstract syntax trees.
 */



typedef class JAST* jAST;



CLASS(AST, Object)
	public:
			    jObject	allocate			(jType, jObjectPtr);
END_CLASS(AST)



OBJECT(AST, Object)
	protected:
		SLOT_C(	RefAST,	ast)
				
	public:
						JAST		(cObject, RefAST);
END_OBJECT(AST)



NEW1(AST, RefAST)



tBool	is_ast_locatable	(RefAST);



#endif
