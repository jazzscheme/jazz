//==============
//	JazzScheme
//==============
//
///	Java Parser
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
#include "JError.h"
#include "JRuntime.h"
#include "JAnsiString.h"
#include "JFormat.h"
#include "JValues.h"
#include "JPathname.h"
#include "JFile.h"



#include <iostream>
#include <fstream>
#include <sstream>

#include "JavaLexer.hpp"
#include "JavaRecognizer.hpp"
#include "JavaTokenTypes.hpp"

#include "antlr/TokenStreamHiddenTokenFilter.hpp"
#include "antlr/CommonASTWithHiddenTokens.hpp"
#include "antlr/CommonHiddenStreamToken.hpp"



STATIC void parse_error(jObject message, jObject source, tInt line, tInt column)
{
	static jObject PARSE_ERROR	= intern(L"Parse-Error");
	static jObject CELL			= intern(L"Cell");
	static jObject KMESSAGE		= intern(L":message");
	static jObject KSOURCE		= intern(L":source");
	static jObject KSTART		= intern(L":start");

	signal(make_object(PARSE_ERROR, KMESSAGE, message, KSOURCE, source, KSTART, make_object(CELL, put_tInt(line), put_tInt(column))));
} ES



//---------
///	Class
//---------



LOCALCLASS JavaRecognizerClass : public JavaRecognizer
{
	private:
		jObject	source;

	public:
		JavaRecognizerClass(ANTLR_USE_NAMESPACE(antlr)TokenStream& lexer, jObject source);

		void reportError(const RecognitionException&);
};



CONSTRUCTOR JavaRecognizerClass::JavaRecognizerClass(ANTLR_USE_NAMESPACE(antlr)TokenStream& lexer, jObject src) : JavaRecognizer(lexer), source(src)
{
}



METHOD void JavaRecognizerClass::reportError(const RecognitionException& ex)
{
	parse_error(put_tString((tAnsiString) ex.toString().c_str()), source, ex.getLine(), ex.getColumn());
}



//----------
///	Parser
//----------



STATIC jObject parse_java(istream* s, tAnsiString filename, cObject model, jObject source)
{
	JavaLexer lexer(*s);
	lexer.setFilename(filename);
	lexer.setTokenObjectFactory(&CommonHiddenStreamToken::factory);
	
	TokenStreamHiddenTokenFilter filter(lexer);
	filter.hide(JavaTokenTypes::SL_COMMENT);
	filter.hide(JavaTokenTypes::ML_COMMENT);

	JavaRecognizerClass parser(filter, source);
	parser.setASTNodeFactory(&CommonASTWithHiddenTokens::factory);
	parser.setFilename(filename);

	RefAST ast = NULL;

	try
	{
		parser.compilationUnit();
		ast = parser.getAST();
	}
    catch (RecognitionException re)
	{
		re;
    }
	catch (exception&)
	{
	}

	if (! ast)
		error(L"Unable to parse java file: {t}", source);

	return newAST(model, ast);
} ES



STATIC jObject parse_java_file(jObject file, cObject model, jObject source)
{
	tAnsiChar	filename[MAX_PATH];

	WideCharToMultiByte(CP_ACP, 0, fetch_tString(parse_pathname(file)), -1, filename, MAX_PATH, NULL, NULL);

	ifstream s(filename);

	return parse_java(&s, filename, model, source);
} ES



STATIC jObject parse_java_string(jAnsiString string, cObject model, jObject source)
{
	istringstream	s(string->contentGet());

	return parse_java(&s, "", model, source);
} ES



//----------
///	Walker
//----------



LOCALCLASS ASTWalker : public JavaTokenTypes
{
	protected:
				tInt	type;
				jQueue	definitions;
				tBool	inCode;
				tBool	includeCode;

	public:
						ASTWalker						(tBool);

				void	walk							(const RefAST &);
				void	walk_package					(const RefAST &);
				void	walk_import						(const RefAST &);
				void	walk_class						(const RefAST &);
				void	walk_interface					(const RefAST &);
				void	walk_static_init				(const RefAST &);
				void	walk_instance_init				(const RefAST &);
				void	walk_constructor				(const RefAST &);
				void	walk_method						(const RefAST &);
				void	walk_variable					(const RefAST &);
				void	walk_parameter					(const RefAST &);
				void	walk_code						(const RefAST &);
				void	walk_identifier					(const RefAST &);
				void	walk_descendants				(const RefAST &);
				void	walk_children					(const RefAST &);
				void	walk_siblings					(const RefAST &);

		virtual	void	on_definition					(jObject)	= 0;
		virtual	void	on_package						(const RefAST &)	= 0;
		virtual	void	on_import						(const RefAST &)	= 0;
		virtual	void	on_class						(const RefAST &)	= 0;
		virtual	void	on_interface					(const RefAST &)	= 0;
		virtual	void	on_static_init					(const RefAST &)	= 0;
		virtual	void	on_instance_init				(const RefAST &)	= 0;
		virtual	void	on_constructor					(const RefAST &)	= 0;
		virtual	void	on_method						(const RefAST &)	= 0;
		virtual	void	on_variable						(const RefAST &)	= 0;
		virtual	void	on_parameter					(const RefAST &)	= 0;
		virtual	void	on_identifier					(const RefAST &)	= 0;

				jObject	ast_text						(const RefAST &);
				RefAST	ast_identifier					(const RefAST &);
				RefAST	ast_content						(const RefAST &);
				jObject	ast_content_text				(const RefAST &);
				tBool	ast_locatable					(const RefAST &);
				tInt	ast_line						(const RefAST &);
				tInt	ast_column						(const RefAST &);

				jObject	types_list						(const RefAST &);
				jObject	parameters_list					(const RefAST &);
				jObject	qualified_list					(const RefAST &, jObject = Nil);

				jObject	class_extends					(const RefAST &);
				jObject	class_implements				(const RefAST &);

				jObject	get_modifiers					(const RefAST &);
				jObject	definition_modifiers			(const RefAST &);
				jObject	definition_type					(const RefAST &);
				jObject	definition_parameters			(const RefAST &);
				RefAST	definition_body					(const RefAST &);

				jObject	new_package_definition			(const RefAST &, jObject, jObject);
				jObject	new_import_definition			(const RefAST &, jObject, jObject);
				jObject	new_class_definition			(const RefAST &, jObject);
				jObject	new_interface_definition		(const RefAST &, jObject);
				jObject	new_static_init_definition		(const RefAST &, jObject, jObject);
				jObject	new_instance_init_definition	(const RefAST &, jObject, jObject);
				jObject	new_constructor_definition		(const RefAST &, jObject, jObject);
				jObject	new_method_definition			(const RefAST &, jObject, jObject);
				jObject	new_variable_definition			(const RefAST &, jObject, jObject);
				jObject	new_parameter_definition		(const RefAST &, jObject, jObject);
				jObject	new_code_definition				(const RefAST &, jObject);
};



CONSTRUCTOR ASTWalker::ASTWalker(tBool ic) : type(-1), definitions(NULL), inCode(false), includeCode(ic)
{
}



METHOD void ASTWalker::walk(const RefAST & ast)
{
	if (ast)
	{
		switch (ast->getType())
		{
			case PACKAGE_DEF:
				walk_package(ast);
				break;

			case IMPORT:
				walk_import(ast);
				break;

			case CLASS_DEF:
				walk_class(ast);
				break;

			case INTERFACE_DEF:
				walk_interface(ast);
				break;

			case STATIC_INIT:
				walk_static_init(ast);
				break;

			case INSTANCE_INIT:
				walk_instance_init(ast);
				break;

			case CTOR_DEF:
				walk_constructor(ast);
				break;

			case METHOD_DEF:
				walk_method(ast);
				break;

			case VARIABLE_DEF:
				walk_variable(ast);
				break;

//			case PARAMETER_DEF:
//				walk_parameter(ast);
//				break;

			case IDENT:
				walk_identifier(ast);
				break;

			default:
				walk_code(ast);
				break;
		}
	}
}



METHOD void ASTWalker::walk_package(const RefAST & ast)
{
	tInt	preserved_type;
	jObject	definition;
	jQueue	preserved_definitions;
	tBool	preserved_inCode;
	JQueue	queue(getQueueClass());

	preserved_type = type;
	preserved_definitions = definitions;
	preserved_inCode = inCode;
	type = PACKAGE_DEF;
	definitions = &queue;
	inCode = true;
	on_package(ast);
	walk(definition_body(ast));
	type = preserved_type;
	definitions = preserved_definitions;
	inCode = preserved_inCode;

	if (includeCode)
		definition = new_package_definition(ast, queue.queue_list(), Nil);
	else
		definition = new_package_definition(ast, Nil, queue.queue_list());
	on_definition(definition);
	
	walk_siblings(ast);
}


METHOD void ASTWalker::walk_import(const RefAST & ast)
{
	tInt	preserved_type;
	jObject	definition;
	jQueue	preserved_definitions;
	tBool	preserved_inCode;
	JQueue	queue(getQueueClass());

	preserved_type = type;
	preserved_definitions = definitions;
	preserved_inCode = inCode;
	type = IMPORT;
	definitions = &queue;
	inCode = true;
	on_import(ast);
	walk(definition_body(ast));
	type = preserved_type;
	definitions = preserved_definitions;
	inCode = preserved_inCode;

	if (includeCode)
		definition = new_import_definition(ast, queue.queue_list(), Nil);
	else
		definition = new_import_definition(ast, Nil, queue.queue_list());
	on_definition(definition);
	
	walk_siblings(ast);
}



METHOD void ASTWalker::walk_class(const RefAST & ast)
{
	tInt	preserved_type;
	jObject	definition;
	jQueue	preserved_definitions;
	JQueue	queue(getQueueClass());

	preserved_type = type;
	preserved_definitions = definitions;
	type = CLASS_DEF;
	definitions = &queue;
	on_class(ast);
	walk_children(ast);
	type = preserved_type;
	definitions = preserved_definitions;
	
	definition = new_class_definition(ast, queue.queue_list());
	on_definition(definition);
	
	walk_siblings(ast);
}



METHOD void ASTWalker::walk_interface(const RefAST & ast)
{
	tInt	preserved_type;
	jObject	definition;
	jQueue	preserved_definitions;
	JQueue	queue(getQueueClass());

	preserved_type = type;
	preserved_definitions = definitions;
	type = INTERFACE_DEF;
	definitions = &queue;
	on_interface(ast);
	walk_children(ast);
	type = preserved_type;
	definitions = preserved_definitions;

	definition = new_interface_definition(ast, queue.queue_list());
	on_definition(definition);
	
	walk_siblings(ast);
}



METHOD void ASTWalker::walk_static_init(const RefAST & ast)
{
	tInt	preserved_type;
	jObject	definition;
	jQueue	preserved_definitions;
	tBool	preserved_inCode;
	JQueue	queue(getQueueClass());

	preserved_type = type;
	preserved_definitions = definitions;
	preserved_inCode = inCode;
	type = STATIC_INIT;
	definitions = &queue;
	inCode = true;
	on_static_init(ast);
	walk(definition_body(ast));
	type = preserved_type;
	definitions = preserved_definitions;
	inCode = preserved_inCode;

	if (includeCode)
		definition = new_static_init_definition(ast, queue.queue_list(), Nil);
	else
		definition = new_static_init_definition(ast, Nil, queue.queue_list());
	on_definition(definition);
	
	walk_siblings(ast);
}



METHOD void ASTWalker::walk_instance_init(const RefAST & ast)
{
	tInt	preserved_type;
	jObject	definition;
	jQueue	preserved_definitions;
	tBool	preserved_inCode;
	JQueue	queue(getQueueClass());

	preserved_type = type;
	preserved_definitions = definitions;
	preserved_inCode = inCode;
	type = STATIC_INIT;
	definitions = &queue;
	inCode = true;
	on_instance_init(ast);
	walk(definition_body(ast));
	type = preserved_type;
	definitions = preserved_definitions;
	inCode = preserved_inCode;

	if (includeCode)
		definition = new_instance_init_definition(ast, queue.queue_list(), Nil);
	else
		definition = new_instance_init_definition(ast, Nil, queue.queue_list());
	on_definition(definition);
	
	walk_siblings(ast);
}



METHOD void ASTWalker::walk_constructor(const RefAST & ast)
{
	tInt	preserved_type;
	jObject	definition;
	jQueue	preserved_definitions;
	tBool	preserved_inCode;
	JQueue	queue(getQueueClass());

	preserved_type = type;
	preserved_definitions = definitions;
	preserved_inCode = inCode;
	type = CTOR_DEF;
	definitions = &queue;
	inCode = true;
	on_constructor(ast);
	walk(definition_body(ast));
	type = preserved_type;
	definitions = preserved_definitions;
	inCode = preserved_inCode;

	if (includeCode)
		definition = new_constructor_definition(ast, queue.queue_list(), Nil);
	else
		definition = new_constructor_definition(ast, Nil, queue.queue_list());
	on_definition(definition);
	
	walk_siblings(ast);
}



METHOD void ASTWalker::walk_method(const RefAST & ast)
{
	tInt	preserved_type;
	jObject	definition;
	jQueue	preserved_definitions;
	tBool	preserved_inCode;
	JQueue	queue(getQueueClass());

	preserved_type = type;
	preserved_definitions = definitions;
	preserved_inCode = inCode;
	type = METHOD_DEF;
	definitions = &queue;
	inCode = true;
	on_method(ast);
	walk(definition_body(ast));
	type = preserved_type;
	definitions = preserved_definitions;
	inCode = preserved_inCode;

	if (includeCode)
		definition = new_method_definition(ast, queue.queue_list(), Nil);
	else
		definition = new_method_definition(ast, Nil, queue.queue_list());
	on_definition(definition);
	
	walk_siblings(ast);
}



METHOD void ASTWalker::walk_variable(const RefAST & ast)
{
	if (type == CLASS_DEF || type == INTERFACE_DEF)
	{
		tInt	preserved_type;
		jObject	definition;
		jQueue	preserved_definitions;
		tBool	preserved_inCode;
		JQueue	queue(getQueueClass());

		preserved_type = type;
		preserved_definitions = definitions;
		preserved_inCode = inCode;
		type = VARIABLE_DEF;
		definitions = &queue;
		inCode = true;
		on_variable(ast);
		walk(definition_body(ast));
		type = preserved_type;
		definitions = preserved_definitions;
		inCode = preserved_inCode;

		if (includeCode)
			definition = new_variable_definition(ast, queue.queue_list(), Nil);
		else
			definition = new_variable_definition(ast, Nil, queue.queue_list());
		on_definition(definition);
		
		walk_siblings(ast);
	}
	else
		walk_descendants(ast);
}



METHOD void ASTWalker::walk_parameter(const RefAST & ast)
{
	if (type == CTOR_DEF || type == METHOD_DEF)
	{
		tInt	preserved_type;
		jObject	definition;
		jQueue	preserved_definitions;
		tBool	preserved_inCode;
		JQueue	queue(getQueueClass());

		preserved_type = type;
		preserved_definitions = definitions;
		preserved_inCode = inCode;
		type = PARAMETER_DEF;
		definitions = &queue;
		inCode = true;
		on_parameter(ast);
		walk(definition_body(ast));
		type = preserved_type;
		definitions = preserved_definitions;
		inCode = preserved_inCode;

		if (includeCode)
			definition = new_parameter_definition(ast, queue.queue_list(), Nil);
		else
			definition = new_parameter_definition(ast, Nil, queue.queue_list());
		on_definition(definition);
		
		walk_siblings(ast);
	}
	else
		walk_descendants(ast);
}



METHOD void ASTWalker::walk_code(const RefAST & ast)
{
	if (inCode && includeCode)
	{
		tInt	preserved_type;
		jObject	definition;
		jQueue	preserved_definitions;
		JQueue	queue(getQueueClass());

		preserved_type = type;
		preserved_definitions = definitions;
		type = ast->getType();
		definitions = &queue;
		walk_children(ast);
		type = preserved_type;
		definitions = preserved_definitions;

		definition = new_code_definition(ast, queue.queue_list());
		on_definition(definition);
		
		walk_siblings(ast);
	}
	else
		walk_descendants(ast);
}



METHOD void ASTWalker::walk_identifier(const RefAST & ast)
{
	if (includeCode)
		walk_code(ast);
	else
	{
		on_identifier(ast);
		walk_descendants(ast);
	}
}



METHOD void ASTWalker::walk_descendants(const RefAST & ast)
{
	walk_children(ast);
	walk_siblings(ast);
}



METHOD void ASTWalker::walk_children(const RefAST & ast)
{
	RefAST	child;

	child = ast->getFirstChild();
	
	if (child)
		walk(child);
}



METHOD void ASTWalker::walk_siblings(const RefAST & ast)
{
	RefAST	sibling;

	sibling = ast->getNextSibling();
	
	if (sibling)
		walk(sibling);
}



METHOD jObject ASTWalker::ast_text(const RefAST & ast)
{
	return put_tString((tAnsiString) ast->getText().c_str());
}



METHOD RefAST ASTWalker::ast_identifier(const RefAST & ast)
{
	switch (ast->getType())
	{
		case PACKAGE_DEF:
			return ast->getFirstChild();

		case IMPORT:
			return ast->getFirstChild();

		case CLASS_DEF:
			return ast->getFirstChild()->getNextSibling();

		case INTERFACE_DEF:
			return ast->getFirstChild()->getNextSibling();

		case CTOR_DEF:
			return ast->getFirstChild()->getNextSibling();

		case METHOD_DEF:
			return ast->getFirstChild()->getNextSibling()->getNextSibling();

		case VARIABLE_DEF:
			return ast->getFirstChild()->getNextSibling()->getNextSibling();

		case PARAMETER_DEF:
			return ast->getFirstChild()->getNextSibling()->getNextSibling();
	}

	return NULL;
}



METHOD RefAST ASTWalker::ast_content(const RefAST & ast)
{
	switch (ast->getType())
	{
		case DOT:
			return ast_content(ast->getFirstChild()->getNextSibling());

		case TYPE:
		case ARRAY_DECLARATOR:
			return ast_content(ast->getFirstChild());
		
		case IDENT:
		default:
			return ast;
	}
}



METHOD jObject ASTWalker::ast_content_text(const RefAST & ast)
{
	switch (ast->getType())
	{
		case IDENT:
			return ast_text(ast);
		
		case DOT:
			return ast_content_text(ast->getFirstChild()->getNextSibling());

		case TYPE:
			return ast_content_text(ast->getFirstChild());
			
		case ARRAY_DECLARATOR:
			return format_string(L"{a}[]", ast_content_text(ast->getFirstChild()));
		
		default:
			return ast_text(ast);
	}
}



METHOD tBool ASTWalker::ast_locatable(const RefAST & ast)
{
	return is_ast_locatable(ast);
}



METHOD tInt ASTWalker::ast_line(const RefAST & ast)
{
	return ast_content(ast)->getLine();
}


METHOD tInt ASTWalker::ast_column(const RefAST & ast)
{
	return ast_content(ast)->getColumn();
}



METHOD jObject ASTWalker::types_list(const RefAST & ast)
{
	JQueue	queue(getQueueClass());
	RefAST	type;

	type = ast->getFirstChild();
	while (type)
	{
		queue.enqueue(qualified_list(type));
		type = type->getNextSibling();
	}

	return queue.queue_list();
}



METHOD jObject ASTWalker::parameters_list(const RefAST & ast)
{
	JQueue	queue(getQueueClass());
	RefAST	parameter;

	parameter = ast->getFirstChild();
	while (parameter)
	{
		queue.enqueue(ast_content_text(parameter->getFirstChild()->getNextSibling()));
		parameter = parameter->getNextSibling();
	}

	return queue.queue_list();
}



METHOD jObject ASTWalker::qualified_list(const RefAST & ast, jObject tail)
{
	switch (ast->getType())
	{
		case IDENT:
			return cons(ast_content_text(ast), tail);

		case DOT:
			return qualified_list(ast->getFirstChild(),
					cons(ast_content_text(ast), tail));

		default:
			return Nil;
	}
}



METHOD jObject ASTWalker::class_extends(const RefAST & ast)
{
	return types_list(ast->getFirstChild()->getNextSibling()->getNextSibling());
}



METHOD jObject ASTWalker::class_implements(const RefAST & ast)
{
	return types_list(ast->getFirstChild()->getNextSibling()->getNextSibling()->getNextSibling());
}



METHOD jObject ASTWalker::get_modifiers(const RefAST & ast)
{
	static jObject ABSTRACT_		= intern(L"abstract");
	static jObject FINAL_			= intern(L"final");
	static jObject NATIVE_			= intern(L"native");
	static jObject PRIVATE_			= intern(L"private");
	static jObject PROTECTED_		= intern(L"protected");
	static jObject PUBLIC_			= intern(L"public");
	static jObject STATIC_			= intern(L"static");
	static jObject SYNCHRONIZED_	= intern(L"synchronized");
	static jObject THREADSAFE_		= intern(L"threadsafe");
	static jObject TRANSIENT_		= intern(L"transient");
	static jObject VOLATILE_		= intern(L"volatile");

	RefAST	scan, modifier;
	JQueue	queue(getQueueClass());

	scan = ast->getFirstChild();
	while (scan)
	{
		switch (scan->getType())
		{
			case ABSTRACT:				queue.enqueue(ABSTRACT_);		break;
			case FINAL:					queue.enqueue(FINAL_);			break;
			case LITERAL_native:		queue.enqueue(NATIVE_);			break;
			case LITERAL_private:		queue.enqueue(PRIVATE_);		break;
			case LITERAL_protected:		queue.enqueue(PROTECTED_);		break;
			case LITERAL_public:		queue.enqueue(PUBLIC_);			break;
			case LITERAL_static:		queue.enqueue(STATIC_);			break;
			case LITERAL_synchronized:	queue.enqueue(SYNCHRONIZED_);	break;
			case LITERAL_threadsafe:	queue.enqueue(THREADSAFE_);		break;
			case LITERAL_transient:		queue.enqueue(TRANSIENT_);		break;
			case LITERAL_volatile:		queue.enqueue(VOLATILE_);		break;
		}
		scan = scan->getNextSibling();
	}

	return queue.queue_list();
}



METHOD jObject ASTWalker::definition_modifiers(const RefAST & ast)
{
	switch (ast->getType())
	{
		case CLASS_DEF:
			return get_modifiers(ast->getFirstChild());

		case INTERFACE_DEF:
			return get_modifiers(ast->getFirstChild());

		case CTOR_DEF:
			return get_modifiers(ast->getFirstChild());

		case METHOD_DEF:
			return get_modifiers(ast->getFirstChild());

		case VARIABLE_DEF:
			return get_modifiers(ast->getFirstChild());

		case PARAMETER_DEF:
			return get_modifiers(ast->getFirstChild());

		default:
			return Nil;
	}
}



METHOD jObject ASTWalker::definition_type(const RefAST & ast)
{
	switch (ast->getType())
	{
		case CTOR_DEF:
			return Nil;

		case METHOD_DEF:
			return ast_content_text(ast->getFirstChild()->getNextSibling());

		case VARIABLE_DEF:
			return ast_content_text(ast->getFirstChild()->getNextSibling());

		case PARAMETER_DEF:
			return ast_content_text(ast->getFirstChild()->getNextSibling());
	}

	return NULL;
}



METHOD jObject ASTWalker::definition_parameters(const RefAST & ast)
{
	switch (ast->getType())
	{
		case CTOR_DEF:
			return parameters_list(ast->getFirstChild()->getNextSibling()->getNextSibling());

		case METHOD_DEF:
			return parameters_list(ast->getFirstChild()->getNextSibling()->getNextSibling()->getNextSibling());

		case VARIABLE_DEF:
			return Nil;
	}

	return NULL;
}



METHOD RefAST ASTWalker::definition_body(const RefAST & ast)
{
	switch (ast->getType())
	{
		case PACKAGE_DEF:
			return ast->getFirstChild();

		case IMPORT:
			return ast->getFirstChild();

		case STATIC_INIT:
			return ast->getFirstChild();

		case INSTANCE_INIT:
			return ast->getFirstChild();

		case CTOR_DEF:
			return ast->getFirstChild()->getNextSibling()->getNextSibling()->getNextSibling();

		case METHOD_DEF:
			return ast->getFirstChild()->getNextSibling()->getNextSibling()->getNextSibling()->getNextSibling();

		case VARIABLE_DEF:
			return ast->getFirstChild()->getNextSibling()->getNextSibling()->getNextSibling();
	}

	return NULL;
}



METHOD jObject ASTWalker::new_package_definition(const RefAST & ast, jObject definitions, jObject references)
{
	static jObject PACKAGE_ENTRY = intern(L"Package-Entry");

	RefAST	ident;
	RefAST	content;

	ident = ast_identifier(ast);
	content = ast_content(ident);

	return make_object(PACKAGE_ENTRY, Nil, qualified_list(ident), ast_content_text(ident), put_tInt(ast_line(content) - 1), put_tInt(ast_column(content) - 1), definitions, references);
}



METHOD jObject ASTWalker::new_import_definition(const RefAST & ast, jObject definitions, jObject references)
{
	static jObject IMPORT_ENTRY = intern(L"Import-Entry");

	RefAST	ident;
	RefAST	content;

	ident = ast_identifier(ast);
	content = ast_content(ident);

	return make_object(IMPORT_ENTRY, Nil, qualified_list(ident), ast_content_text(ident), put_tInt(ast_line(content) - 1), put_tInt(ast_column(content) - 1), definitions, references);
}



METHOD jObject ASTWalker::new_class_definition(const RefAST & ast, jObject definitions)
{
	static jObject CLASS_ENTRY = intern(L"Class-Entry");

	RefAST	ident;

	ident = ast_identifier(ast);

	return make_object(CLASS_ENTRY, definition_modifiers(ast), Nil, ast_text(ident), class_extends(ast), class_implements(ast), put_tInt(ast_line(ident) - 1), put_tInt(ast_column(ident) - 1), definitions);
}



METHOD jObject ASTWalker::new_interface_definition(const RefAST & ast, jObject definitions)
{
	static jObject INTERFACE_ENTRY = intern(L"Interface-Entry");

	RefAST	ident;

	ident = ast_identifier(ast);

	return make_object(INTERFACE_ENTRY, definition_modifiers(ast), Nil, ast_text(ident), class_extends(ast), put_tInt(ast_line(ident) - 1), put_tInt(ast_column(ident) - 1), definitions);
}



METHOD jObject ASTWalker::new_static_init_definition(const RefAST & ast, jObject definitions, jObject references)
{
	static jObject STATIC_INIT_ENTRY = intern(L"Static-Init-Entry");

	return make_object(STATIC_INIT_ENTRY, definition_modifiers(ast), Nil, Nil, put_tInt(ast->getFirstChild()->getLine() - 1), put_tInt(ast->getFirstChild()->getColumn() - 1), definitions, references);
}



METHOD jObject ASTWalker::new_instance_init_definition(const RefAST & ast, jObject definitions, jObject references)
{
	static jObject INSTANCE_INIT_ENTRY = intern(L"Instance-Init-Entry");

	return make_object(INSTANCE_INIT_ENTRY, definition_modifiers(ast), Nil, Nil, put_tInt(ast->getFirstChild()->getLine() - 1), put_tInt(ast->getFirstChild()->getColumn() - 1), definitions, references);
}



METHOD jObject ASTWalker::new_constructor_definition(const RefAST & ast, jObject definitions, jObject references)
{
	static jObject CONSTRUCTOR_ENTRY = intern(L"Constructor-Entry");

	RefAST	ident;
	tBool	is_primitive;

	ident = ast_identifier(ast);
	is_primitive = ast_content(ident)->getType()!=IDENT;

	return make_object(CONSTRUCTOR_ENTRY, definition_modifiers(ast), ast_text(ident), definition_type(ast), put_tBool(is_primitive), definition_parameters(ast), Nil, put_tInt(ast_line(ident) - 1), put_tInt(ast_column(ident) - 1), definitions, references);
}



METHOD jObject ASTWalker::new_method_definition(const RefAST & ast, jObject definitions, jObject references)
{
	static jObject METHOD_ENTRY = intern(L"Method-Entry");

	RefAST	ident;
	tBool	is_primitive;

	ident = ast_identifier(ast);
	is_primitive = ast_content(ident)->getType()!=IDENT;

	return make_object(METHOD_ENTRY, definition_modifiers(ast), ast_text(ident), definition_type(ast), put_tBool(is_primitive), definition_parameters(ast), Nil, put_tInt(ast_line(ident) - 1), put_tInt(ast_column(ident) - 1), definitions, references);
}



METHOD jObject ASTWalker::new_variable_definition(const RefAST & ast, jObject definitions, jObject references)
{
	static jObject VARIABLE_ENTRY = intern(L"Variable-Entry");

	RefAST	ident;
	tBool	is_primitive;

	ident = ast_identifier(ast);
	is_primitive = ast_content(ident)->getType()!=IDENT;

	return make_object(VARIABLE_ENTRY, definition_modifiers(ast), ast_text(ident), definition_type(ast), put_tBool(is_primitive), put_tInt(ast_line(ident) - 1), put_tInt(ast_column(ident) - 1), definitions, references);
}



METHOD jObject ASTWalker::new_parameter_definition(const RefAST & ast, jObject definitions, jObject references)
{
	static jObject PARAMETER_ENTRY = intern(L"Parameter-Entry");

	RefAST	ident;
	tBool	is_primitive;

	ident = ast_identifier(ast);
	is_primitive = ast_content(ident)->getType()!=IDENT;

	return make_object(PARAMETER_ENTRY, definition_modifiers(ast), ast_text(ident), definition_type(ast), put_tBool(is_primitive), put_tInt(ast_line(ident) - 1), put_tInt(ast_column(ident) - 1), definitions, references);
}



METHOD jObject ASTWalker::new_code_definition(const RefAST & ast, jObject definitions)
{
	static jObject CODE_ENTRY = intern(L"Code-Entry");

	tInt	type;
	jObject	line, column;

	type = ast->getType();

	if (ast_locatable(ast))
	{
		line = put_tInt(ast_line(ast) - 1);
		column = put_tInt(ast_column(ast) - 1);
	}
	else
	{
		line = Nil;
		column = Nil;
	}

	return make_object(CODE_ENTRY, ast_text(ast), put_tInt(type), line, column, definitions);
}



//---------------
///	Definitions
//---------------



LOCALCLASS DefinitionsFinder : public ASTWalker
{
	protected:
		tBool	includeReferences;
		jQueue	queue;

	public:
				DefinitionsFinder	(tBool, tBool, jQueue);

		void	on_definition		(jObject);
		void	on_import			(const RefAST &);
		void	on_package			(const RefAST &);
		void	on_class			(const RefAST &);
		void	on_interface		(const RefAST &);
		void	on_static_init		(const RefAST &);
		void	on_instance_init	(const RefAST &);
		void	on_constructor		(const RefAST &);
		void	on_method			(const RefAST &);
		void	on_variable			(const RefAST &);
		void	on_parameter		(const RefAST &);
		void	on_identifier		(const RefAST &);

		void	add_definition		(jObject);
		tBool	has_reference		(tString);
};



CONSTRUCTOR DefinitionsFinder::DefinitionsFinder(tBool ir, tBool ic, jQueue q) : includeReferences(ir), queue(q), ASTWalker(ic)
{
}



METHOD void DefinitionsFinder::on_definition(jObject definition)
{
	if (! inCode)
	{
		if (! definitions)
			add_definition(definition);
		else
			definitions->enqueue(definition);
	}
}



METHOD void DefinitionsFinder::on_import(const RefAST & ast)
{
}



METHOD void DefinitionsFinder::on_package(const RefAST & ast)
{
}



METHOD void DefinitionsFinder::on_class(const RefAST & ast)
{
}



METHOD void DefinitionsFinder::on_interface(const RefAST & ast)
{
}



METHOD void DefinitionsFinder::on_static_init(const RefAST & ast)
{
}



METHOD void DefinitionsFinder::on_instance_init(const RefAST & ast)
{
}



METHOD void DefinitionsFinder::on_constructor(const RefAST & ast)
{
}



METHOD void DefinitionsFinder::on_method(const RefAST & ast)
{
}



METHOD void DefinitionsFinder::on_variable(const RefAST & ast)
{
}



METHOD void DefinitionsFinder::on_parameter(const RefAST & ast)
{
}



METHOD void DefinitionsFinder::on_identifier(const RefAST & ast)
{
	if (type != CLASS_DEF && type != INTERFACE_DEF && includeReferences && definitions)
	{
		jObject	ident;
		tString	string;

		ident = ast_text(ast);
		string = fetch_tString(ident);
		if (type == STATIC_INIT || ! has_reference(string))
		{
			definitions->enqueue(ident);
		}
	}
}



METHOD void DefinitionsFinder::add_definition(jObject definition)
{
	queue->enqueue(definition);
}



METHOD tBool DefinitionsFinder::has_reference(tString string)
{
	jObject	list;

	list = definitions->queue_list();
	while (list != Nil)
	{
		if (wcscmp(string, fetch_tString(car(list))) == 0)
			return true;

		list = cdr(list);
	}

	return false;
}



//------------
///	External
//------------



JAZZAPI jObject JAZZCALL JzParseJava(jObject object, cObject model, jObjectPtr rest)
{
	OPT(jObject, source, object, rest)

	if (is_file(object))
		return parse_java_file(object, model, source);
	else if (isAnsiString(object))
		return parse_java_string(castAnsiString(object), model, source);
	else
	{
		error(L"{t} is not java parsable", object);
		return NOOBJECT;
	}
}



STATIC jObject java_definitions(jObject ast, tBool includeReferences, tBool includeCode)
{
	JQueue				queue(getQueueClass());
	DefinitionsFinder	finder(includeReferences, includeCode, &queue);

	finder.walk(castAST(ast)->astGet());

	return queue.queue_list();
} ES



JAZZAPI jObject JAZZCALL JzJavaDefinitions(jObject ast, jObjectPtr rest)
{
	KEY(L":include-references?"	,tBool,		includeReferences,	true,	rest)
	KEY(L":include-code?"		,tBool,		includeCode,		false,	rest)

	return java_definitions(ast, includeReferences, includeCode);
}
