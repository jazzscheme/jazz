/* -----------------------------------------------------------------------------
 * See the LICENSE file for information on copyright, usage and redistribution
 * of SWIG, and the README file for authors - http://www.swig.org/release.html.
 *
 * jazzscheme.cxx
 *
 * jazzscheme language module for SWIG.
 * ----------------------------------------------------------------------------- */



#include "swigmod.h"
#include "cparse.h"
#include <ctype.h>



class JAZZSCHEME:public Language {
public:
	String *module;
	String *f_jazz;
	
	virtual void main(int argc, char *argv[]);
	virtual int top(Node *n);
	virtual int functionWrapper(Node *n);
	virtual int variableWrapper(Node *n);
	virtual int constantWrapper(Node *n);
	virtual int classDeclaration(Node *n);
	virtual int enumDeclaration(Node *n);
	virtual int typedefHandler(Node *n);
	
	// c++ specific
	virtual int constructorHandler(Node *n);
	virtual int destructorHandler(Node *n);
	virtual int memberfunctionHandler(Node *n);
	virtual int membervariableHandler(Node *n);
	virtual int classHandler(Node *n);

private:
	void emit_function(Node *n, String *name);
	void emit_method(Node *n);
	void emit_getter(Node *n);
	void emit_setter(Node *n);
	void emit_class(Node *n);
	void emit_struct_union(Node *n, bool un);
	void emit_export(Node *n, String *name);
	void emit_inline(Node *n, String *name);
	
	String *lispy_name(char *name);
	String *lispify_name(Node *n, String *ty, const char *flag, bool kw = false);
	String *convert_literal(String *num_param, String *type, bool try_to_split = true);
	String *infix_to_prefix(String *val, char split_op, const String *op, String *type);
	String *strip_parens(String *string);
	String *trim(String *string);
};



void JAZZSCHEME::main(int argc, char *argv[]) {
	int i;
	
	SWIG_library_directory("jazzscheme");
	SWIG_config_file("jazzscheme.swg");
	for (i = 1; i < argc; i++) {
		if (!Strcmp(argv[i], "-help")) {
			Printf(stdout, "jazzscheme Options (available with -jazzscheme)\n");
		}
	}
	f_jazz = NewString("");
	
	allow_overloading();
}



int JAZZSCHEME::top(Node *n) {
	File *f_null = NewString("");
	module = Getattr(n, "name");
	
	String *output_filename = NewString("");
	Printf(output_filename, "%s%s.fusion", SWIG_output_directory(), module);
	
	File *f_output = NewFile(output_filename, "w");
	NewFile(output_filename, "w");
	if (!f_output) {
		FileErrorDisplay(output_filename);
		SWIG_exit(EXIT_FAILURE);
	}
	
	Swig_register_filebyname("header", f_null);
	Swig_register_filebyname("wrapper", f_null);
	Swig_register_filebyname("runtime", f_null);
	Swig_register_filebyname("lisphead", f_null);
	Swig_register_filebyname("swiglisp", f_null);
	
	Language::top(n);
	Printf(f_output, ";;;");
	Printf(f_output, "\n;;;; %s", module);
	Printf(f_output, "\n;;;\n\n\n");
	Printf(f_output, "(c-package %s", module);
	Printf(f_output, "%s)\n", f_jazz);
	
	Printf(stderr, "All done!\n");
	Close(f_output);
	Delete(f_output);
	Delete(f_jazz);
	Delete(f_null);
	
	return SWIG_OK;
}



int JAZZSCHEME::classHandler(Node *n) {
	String *name = Getattr(n, "sym:name");
	String *kind = Getattr(n, "kind");
	
	if (Strcmp(kind, "struct") == 0) {
		emit_struct_union(n, false);
		return SWIG_OK;
	} else if (Strcmp(kind, "union") == 0) {
		emit_struct_union(n, true);
		return SWIG_OK;
	} else if (Strcmp(kind, "class") == 0) {
		emit_class(n);
		Language::classHandler(n);
		Printf(f_jazz, ")");
	} else {
		Printf(stderr, "Don't know how to deal with %s kind of class yet.\n", kind);
		Printf(stderr, " (name: %s)\n", name);
		SWIG_exit(EXIT_FAILURE);
		return SWIG_OK;
	}
	
	return SWIG_OK;
}



int JAZZSCHEME::constructorHandler(Node *n) {
	// Let SWIG generate a global forwarding function.
	return Language::constructorHandler(n);
}



int JAZZSCHEME::destructorHandler(Node *n) {
	// Let SWIG generate a global forwarding function.
	return Language::destructorHandler(n);
}



void JAZZSCHEME::emit_method(Node *n) {
	String *args_placeholder = NewStringf("");
	String *args_call = NewStringf("");
	
	ParmList *pl = Getattr(n, "parms");
	int argnum = 0;
	Node *parent = parentNode(n);

	for (Parm *p = pl; p; p = nextSibling(p), argnum++) {
	
		String *argname = Getattr(p, "name");
		String *ffitype = Swig_typemap_lookup_new("lispclass", p, "", 0);
		
		int tempargname = 0;
		
		if (!argname) {
			argname = NewStringf("arg%d", argnum);
			tempargname = 1;
		} else if (Strcmp(argname, "t") == 0 || Strcmp(argname, "T") == 0) {
			argname = NewStringf("t-arg%d", argnum);
			tempargname = 1;
		}
		if (Len(ffitype) > 0)
			Printf(args_placeholder, " (%s %s)", argname, ffitype);
		else
			Printf(args_placeholder, " %s", argname, ffitype);
		
		if (Strcmp(ffitype, lispify_name(parent, lispy_name(Char(Getattr(parent, "sym:name"))), "'classname")) == 0)
			Printf(args_call, " %s", argname);
		else
			Printf(args_call, " %s", argname);
		
		Delete(ffitype);
		
		if (tempargname)
			Delete(argname);
	}
	
	String *method_name = Getattr(n, "name");
	
	Printf(f_jazz, "\n\n\n    (c-method (%s (obj %s)%s)\n      (%s obj%s))",
		lispify_name(n, lispy_name(Char(method_name)), "'method"),
		lispify_name(parent, lispy_name(Char(Getattr(parent, "sym:name"))), "'class"), args_placeholder,
		lispify_name(n, Getattr(n, "sym:name"), "'function"), args_call);

}



void JAZZSCHEME::emit_setter(Node *n) {
	Node *p = parentNode(n);
	Printf(f_jazz, "\n\n\n    (c-method (set-%s arg0 (obj %s))\n      (%s obj arg0))",
		lispify_name(n, Getattr(n, "name"), "'method"),
		lispify_name(p, lispy_name(Char(Getattr(p, "sym:name"))), "'class"), lispify_name(n, Getattr(n, "sym:name"), "'function"));
}



void JAZZSCHEME::emit_getter(Node *n) {
	Node *p = parentNode(n);
	Printf(f_jazz, "\n\n\n    (c-method (get-%s (obj %s))\n      (%s obj))",
		lispify_name(n, Getattr(n, "name"), "'method"),
		lispify_name(p, lispy_name(Char(Getattr(p, "sym:name"))), "'class"), lispify_name(n, Getattr(n, "sym:name"), "'function"));
}



int JAZZSCHEME::memberfunctionHandler(Node *n) {
	// Let SWIG generate a global forwarding function.
	Setattr(n, "jazzscheme:memberfunction", "1");
	return Language::memberfunctionHandler(n);
}



int JAZZSCHEME::membervariableHandler(Node *n) {
	// Let SWIG generate a get/set function pair.
	Setattr(n, "jazzscheme:membervariable", "1");
	return Language::membervariableHandler(n);
}



int JAZZSCHEME::functionWrapper(Node *n) {
	ParmList *parms = Getattr(n, "parms");
	String *iname = Getattr(n, "sym:name");
	Wrapper *wrap = NewWrapper();
	
	String *raw_return_type = Swig_typemap_lookup_new("ctype", n, "", 0);
	SwigType *return_type = Swig_cparse_type(raw_return_type);
	SwigType *resolved = SwigType_typedef_resolve_all(return_type);
	int is_void_return = (Cmp(resolved, "void") == 0);
	Delete(resolved);
	
	if (!is_void_return) {
		String *lresult_init = NewStringf("lresult = (%s)0", raw_return_type);
		Wrapper_add_localv(wrap, "lresult", raw_return_type, lresult_init, NIL);
		Delete(lresult_init);
	}
	
	String *overname = 0;
	if (Getattr(n, "sym:overloaded")) {
		overname = Getattr(n, "sym:overname");
	} else {
		if (!addSymbol(iname, n))
		return SWIG_ERROR;
	}

	String *wname = Swig_name_wrapper(iname);
	if (overname) {
		StringAppend(wname, overname);
	}
	// Emit all of the local variables for holding arguments.
	emit_args(Getattr(n, "type"), parms, wrap);
	
	// Attach the standard typemaps 
	Swig_typemap_attach_parms("ctype", parms, wrap);
	emit_attach_parmmaps(parms, wrap);
	
	int num_arguments = emit_num_arguments(parms);
	String *name_and_parms = NewStringf("%s (", wname);
	int i;
	Parm *p;
	int gencomma = 0;

	for (i = 0, p = parms; i < num_arguments; i++) {
		while (checkAttribute(p, "tmap:in:numinputs", "0")) {
			p = Getattr(p, "tmap:in:next");
		}
		
		SwigType *c_parm_type = Swig_cparse_type(Getattr(p, "tmap:ctype"));
		String *arg = NewStringf("l%s", Getattr(p, "lname"));
		
		// Emit parameter declaration
		if (gencomma)
			Printf(name_and_parms, ", ");
		String *parm_decl = SwigType_str(c_parm_type, arg);
		Printf(name_and_parms, "%s", parm_decl);
		Delete(parm_decl);
		gencomma = 1;
		
		// Emit parameter conversion code
		String *parm_code = Getattr(p, "tmap:in");
		{
			Replaceall(parm_code, "$input", arg);
			Setattr(p, "emit:input", arg);
			Printf(wrap->code, "%s\n", parm_code);
			p = Getattr(p, "tmap:in:next");
		}
		
		Delete(arg);
	}
	Printf(name_and_parms, ")");

	// Emit the function definition
	String *signature = SwigType_str(return_type, name_and_parms);
	Printf(wrap->def, "EXPORT %s {", signature);
	Printf(wrap->code, "  try {\n");
	emit_action(n, wrap);
	if (!is_void_return) {
		String *result_convert = Swig_typemap_lookup_new("out", n, "result", 0);
		Replaceall(result_convert, "$result", "lresult");
		Printf(wrap->code, "%s\n", result_convert);
		Printf(wrap->code, "    return lresult;\n");
		Delete(result_convert);
	}

	Printf(wrap->code, "  } catch (...) {\n");
	if (!is_void_return)
		Printf(wrap->code, "    return (%s)0;\n", raw_return_type);
	Printf(wrap->code, "  }\n");
	Printf(wrap->code, "}\n");
	
	if (CPlusPlus) {
		emit_function(n, wname);
		if (Getattr(n, "jazzscheme:memberfunction"))
			emit_method(n);
		else if (Getattr(n, "jazzscheme:membervariable")) {
			if (Getattr(n, "memberget"))
				emit_getter(n);
			else if (Getattr(n, "memberset"))
				emit_setter(n);
		}
	} else
		emit_function(n, iname);
	
	Delete(wname);
	
	return SWIG_OK;
}



void JAZZSCHEME::emit_function(Node *n, String *name) {
	String *func_name = Getattr(n, "sym:name");
	
	ParmList *pl = Getattr(n, "parms");
	
	int argnum = 0;
	
	func_name = lispify_name(n, func_name, "'function");
	
	emit_inline(n, func_name);
	
	Printf(f_jazz, "\n\n\n    (c-external (\"%s\" %s)", name, func_name);
	String *ffitype = Swig_typemap_lookup_new("cout", n, ":pointer", 0);
	
	Printf(f_jazz, " %s", ffitype);
	Delete(ffitype);

	for (Parm *p = pl; p; p = nextSibling(p), argnum++) {
	
		if (SwigType_isvarargs(Getattr(p, "type"))) {
			Printf(f_jazz, "\n  %s", NewString("&rest"));
			continue;
		}
		
		String *argname = Getattr(p, "name");
		
		ffitype = Swig_typemap_lookup_new("cin", p, "", 0);
		
		int tempargname = 0;
		if (!argname) {
		
			argname = NewStringf("arg%d", argnum);
			tempargname = 1;
		} else if (Strcmp(argname, "t") == 0 || Strcmp(argname, "T") == 0) {
			argname = NewStringf("t_arg%d", argnum);
			tempargname = 1;
		}
		
		Printf(f_jazz, "\n      (%s %s)", argname, ffitype);
		
		Delete(ffitype);
		
		if (tempargname)
			Delete(argname);
	}
	Printf(f_jazz, ")");		/* finish arg list */
	
	emit_export(n, func_name);
}



int JAZZSCHEME::constantWrapper(Node *n) {
	String *type = Getattr(n, "type");
	String *converted_value = convert_literal(Getattr(n, "value"), type);
	String *name = lispify_name(n, Getattr(n, "sym:name"), "'constant");
	
	if (Strcmp(name, "t") == 0 || Strcmp(name, "T") == 0)
		name = NewStringf("t_var");
	
	Printf(f_jazz, "\n\n(constant %s %s)", name, converted_value);
	Delete(converted_value);
	
	emit_export(n, name);
	return SWIG_OK;
}



int JAZZSCHEME::variableWrapper(Node *n) {
	String *var_name = Getattr(n, "sym:name");
	String *lisp_type = Swig_typemap_lookup_new("cin", n, "", 0);
	String *lisp_name = lispify_name(n, var_name, "'variable");
	
	if (Strcmp(lisp_name, "t") == 0 || Strcmp(lisp_name, "T") == 0)
		lisp_name = NewStringf("t_var");
	
	Printf(f_jazz, "\n\n    (c-definition (\"%s\" %s)\n      %s)", var_name, lisp_name, lisp_type);
	
	Delete(lisp_type);
	
	emit_export(n, lisp_name);
	return SWIG_OK;
}



int JAZZSCHEME::typedefHandler(Node *n) {
	if (strncmp(Char(Getattr(n, "type")), "enum", 4)) {
		String *lisp_name = lispify_name(n, Getattr(n, "name"), "'typename");
		Printf(f_jazz, "\n\n(jazzscheme:defctype %s %s)", lisp_name, Swig_typemap_lookup_new("cin", n, "", 0));
		emit_export(n, lisp_name);
	}
	return Language::typedefHandler(n);
}



int JAZZSCHEME::classDeclaration(Node *n) {
	return Language::classDeclaration(n);
}



int JAZZSCHEME::enumDeclaration(Node *n) {
	String *name = Getattr(n, "sym:name");
	bool slot_name_keywords;
	String *lisp_name = 0;
	if (name && Len(name) != 0) {
		lisp_name = lispify_name(n, name, "'enumname");
		if (GetFlag(n, "feature:bitfield")) {
			Printf(f_jazz, "\n(bitfield %s", lisp_name);
		} else {
			Printf(f_jazz, "\n(enumeration %s", lisp_name);
		}
		slot_name_keywords = true;
		
		// Registering the enum name to the cin and cout typemaps
		Parm *pattern = NewParm(name, NULL);
		Swig_typemap_register("cin", pattern, lisp_name, NULL, NULL);
		Swig_typemap_register("cout", pattern, lisp_name, NULL, NULL);
		Delete(pattern);
		
		// Registering with the kind, i.e., enum
		pattern = NewParm(NewStringf("enum %s", name), NULL);
		Swig_typemap_register("cin", pattern, lisp_name, NULL, NULL);
		Swig_typemap_register("cout", pattern, lisp_name, NULL, NULL);
		Delete(pattern);
	} else {
		Printf(f_jazz, "\n(defanonenum %s", name);
		slot_name_keywords = false;
	}
	
	for (Node *c = firstChild(n); c; c = nextSibling(c)) {
	
		String *slot_name = lispify_name(c, Getattr(c, "name"), "'enumvalue", slot_name_keywords);
		String *value = Getattr(c, "enumvalue");
		
		if (!value || GetFlag(n, "feature:bitfield:ignore_values"))
			Printf(f_jazz, "\n%s", slot_name);
		else {
			String *type = Getattr(c, "type");
			String *converted_value = convert_literal(value, type);
			Printf(f_jazz, "\n(%s %s)", slot_name, converted_value);
			Delete(converted_value);
		}
		Delete(value);
	}
	
	Printf(f_jazz, ")");
	
	// No need to export keywords
	if (lisp_name && Len(lisp_name) != 0) {
		emit_export(n, lisp_name);
	} else {
		for (Node *c = firstChild(n); c; c = nextSibling(c))
		emit_export(c, lispify_name(c, Getattr(c, "name"), "'enumvalue"));
	}
	
	return SWIG_OK;
}



void JAZZSCHEME::emit_class(Node *n) {
	String *name = Getattr(n, "sym:name");
	String *lisp_name = lispify_name(n, lispy_name(Char(name)), "'classname");
	
	String *bases = Getattr(n, "bases");
	String *supers = NewString("(");
	if (bases) {
		int first = 1;
		for (Iterator i = First(bases); i.item; i = Next(i)) {
			if (!first)
				Printf(supers, " ");
			String *s = Getattr(i.item, "name");
			Printf(supers, "%s", lispify_name(i.item, s, "'classname"));
		}
	} else {
	}
	Printf(supers, ")");
	
	Printf(f_jazz, "\n\n");
	Printf(f_jazz, "\n  ;;;");
	Printf(f_jazz, "\n  ;;;; %s", lisp_name);
	Printf(f_jazz, "\n  ;;;\n\n");
	Printf(f_jazz, "\n  (c-class %s extends %s", lisp_name, supers);
	
	Parm *pattern = NewParm(Getattr(n, "name"), NULL);
	
	Swig_typemap_register("lispclass", pattern, lisp_name, NULL, NULL);
	SwigType_add_pointer(Getattr(pattern, "type"));
	Swig_typemap_register("lispclass", pattern, lisp_name, NULL, NULL);
	SwigType_add_qualifier(Getattr(pattern, "type"), "const");
	Swig_typemap_register("lispclass", pattern, lisp_name, NULL, NULL);
	SwigType_del_pointer(Getattr(pattern, "type"));
	SwigType_add_reference(Getattr(pattern, "type"));
	Swig_typemap_register("lispclass", pattern, lisp_name, NULL, NULL);
	
	Delete(pattern);
	
	// Walk children to generate type definition.
	String *slotdefs = NewString("   ");
	
	Node *c;
	for (c = firstChild(n); c; c = nextSibling(c)) {
		String *storage_type = Getattr(c, "storage");
		if ((!Strcmp(nodeType(c), "cdecl") && (!storage_type || Strcmp(storage_type, "typedef")))) {
			String *access = Getattr(c, "access");
			
			// hack. why would decl have a value of "variableHandler" and now "0"?
			String *childDecl = Getattr(c, "decl");
			if (!Strcmp(childDecl, "0"))
				childDecl = NewString("");
			
			SwigType *childType = NewStringf("%s%s", childDecl,
			Getattr(c, "type"));
			String *cname = (access && Strcmp(access, "public")) ? NewString("nil") : Copy(Getattr(c, "name"));
			
			if (!SwigType_isfunction(childType)) {
				String *ns = NewString("");
				Printf(slotdefs, "(#.(swig-insert-id \"%s\" %s :type :slot :class \"%s\") %s)", cname, ns, name, childType);	//compose_foreign_type(childType)
				Delete(ns);
				if (access && Strcmp(access, "public"))
				Printf(slotdefs, " ;; %s member", access);
				
				Printf(slotdefs, "\n   ");
			}
			Delete(childType);
			Delete(cname);
		}
	}
	
	Delete(supers);
}



void JAZZSCHEME::emit_struct_union(Node *n, bool un = false) {
	String *name = Getattr(n, "sym:name");
	String *kind = Getattr(n, "kind");
	
	if (Strcmp(kind, "struct") != 0 && Strcmp(kind, "union") != 0) {
		Printf(stderr, "Don't know how to deal with %s kind of class yet.\n", kind);
		Printf(stderr, " (name: %s)\n", name);
		SWIG_exit(EXIT_FAILURE);
	}
	String *lisp_name = lispify_name(n, name, "'classname");
	
	// Register the struct/union name to the cin and cout typemaps
	Parm *pattern = NewParm(name, NULL);
	Swig_typemap_register("cin", pattern, lisp_name, NULL, NULL);
	Swig_typemap_register("cout", pattern, lisp_name, NULL, NULL);
	Delete(pattern);
	
	// Registering with the kind, i.e., struct or union
	pattern = NewParm(NewStringf("%s %s", kind, name), NULL);
	Swig_typemap_register("cin", pattern, lisp_name, NULL, NULL);
	Swig_typemap_register("cout", pattern, lisp_name, NULL, NULL);
	Delete(pattern);
	
	if (un) {
		Printf(f_jazz, "\n\n\n(c-union %s", lisp_name);
	} else
		Printf(f_jazz, "\n\n\n(c-structure %s", lisp_name);

	for (Node *c = firstChild(n); c; c = nextSibling(c)) {
		if (Strcmp(nodeType(c), "cdecl")) {
		} else {
			SwigType *childType = NewStringf("%s%s", Getattr(c, "decl"),
			Getattr(c, "type"));
			
			Hash *typemap = Swig_typemap_search("cin", childType, "", 0);
			String *typespec = NewString("");
			if (typemap) {
				typespec = NewString(Getattr(typemap, "code"));
			}
			
			String *slot_name = lispify_name(c, Getattr(c, "sym:name"), "'slotname");
			if (Strcmp(slot_name, "t") == 0 || Strcmp(slot_name, "T") == 0)
				slot_name = NewStringf("t_var");
			
			Printf(f_jazz, "\n(%s %s)", slot_name, typespec);
			
			Delete(typespec);
		}
	}
	
	Printf(f_jazz, ")");
	
	emit_export(n, lisp_name);
	for (Node *c = firstChild(n); c; c = nextSibling(c)) {
		if (!Strcmp(nodeType(c), "cdecl")) {
			emit_export(c, lispify_name(c, Getattr(c, "sym:name"), "'slotname"));
		}
	}
}



void JAZZSCHEME::emit_export(Node *n, String *name) {
	if (GetInt(n, "feature:export"))
		Printf(f_jazz, "\n(cl:export '%s)\n", name);
}



void JAZZSCHEME::emit_inline(Node *n, String *name) {
	if (GetInt(n, "feature:inline"))
		Printf(f_jazz, "\n(cl:declaim (cl:inline %s))\n", name);
}



//
/// Utilities
//



String *JAZZSCHEME::lispify_name(Node *n, String *ty, const char *flag, bool kw) {
	String *intern_func = Getattr(n, "feature:intern_function");
	if (intern_func) {
		if (Strcmp(intern_func, "1") == 0)
			intern_func = NewStringf("swig-lispify");
		return NewStringf("#.(%s \"%s\" %s%s)", intern_func, ty, flag, kw ? " :keyword" : "");
	} else if (kw)
		return NewStringf(":%s", ty);
	else
		return ty;
}



/* returns new string w/ parens stripped */
String *JAZZSCHEME::strip_parens(String *string) {
	char *s = Char(string), *p;
	int len = Len(string);
	String *res;
	
	if (len == 0 || s[0] != '(' || s[len - 1] != ')') {
		return NewString(string);
	}
	
	p = (char *) malloc(len - 2 + 1);
	if (!p) {
		Printf(stderr, "Malloc failed\n");
		SWIG_exit(EXIT_FAILURE);
	}
	
	strncpy(p, s + 1, len - 1);
	p[len - 2] = 0;		/* null terminate */
	
	res = NewString(p);
	free(p);
	
	return res;
}



String *JAZZSCHEME::trim(String *str) {
	char *c = Char(str);
	while (*c != '\0' && isspace((int) *c))
		++c;
	String *result = NewString(c);
	Chop(result);
	return result;
}



String *JAZZSCHEME::infix_to_prefix(String *val, char split_op, const String *op, String *type) {
	List *ored = Split(val, split_op, -1);
	
	// try parsing the split results. if any part fails, kick out.
	bool part_failed = false;
	if (Len(ored) > 1) {
		String *result = NewStringf("(%s", op);
		for (Iterator i = First(ored); i.item; i = Next(i)) {
			String *converted = convert_literal(i.item, type);
			if (converted) {
				Printf(result, " %s", converted);
				Delete(converted);
			} else {
				part_failed = true;
				break;
			}
		}
		Printf(result, ")");
		Delete(ored);
		return part_failed ? 0 : result;
	} else {
		Delete(ored);
	}
	return 0;
}



/* To be called by code generating the lisp interface
   Will return a containing the literal based on type.
   Will return null if there are problems.

   try_to_split defaults to true (see stub above).
*/
String *JAZZSCHEME::convert_literal(String *literal, String *type, bool try_to_split) {
	String *num_param = Copy(literal);
	String *trimmed = trim(num_param);
	String *num = strip_parens(trimmed), *res = 0;
	Delete(trimmed);
	char *s = Char(num);
	
	// very basic parsing of infix expressions.
	if (try_to_split) {
		if ((res = infix_to_prefix(num, '|', "cl:logior", type)))
			return res;
		if ((res = infix_to_prefix(num, '&', "cl:logand", type)))
			return res;
		if ((res = infix_to_prefix(num, '^', "cl:logxor", type)))
			return res;
		if ((res = infix_to_prefix(num, '*', "cl:*", type)))
			return res;
		if ((res = infix_to_prefix(num, '/', "cl:/", type)))
			return res;
		if ((res = infix_to_prefix(num, '+', "cl:+", type)))
			return res;
		if ((res = infix_to_prefix(num, '-', "cl:-", type)))
			return res;
	}

	if (SwigType_type(type) == T_FLOAT || SwigType_type(type) == T_DOUBLE || SwigType_type(type) == T_LONGDOUBLE) {
		// Use CL syntax for float literals 
		
		// careful. may be a float identifier or float constant.
		char *num_start = Char(num);
		char *num_end = num_start + strlen(num_start) - 1;
		
		bool is_literal = isdigit(*num_start) || (*num_start == '.') || (*num_start == '+') || (*num_start == '-');
	
		String *lisp_exp = 0;
		if (is_literal) {
			if (*num_end == 'f' || *num_end == 'F') {
				lisp_exp = NewString("f");
			} else {
				lisp_exp = NewString("d");
			}
		
			if (*num_end == 'l' || *num_end == 'L' || *num_end == 'f' || *num_end == 'F') {
				*num_end = '\0';
				num_end--;
			}
			
			int exponents = Replaceall(num, "e", lisp_exp) + Replaceall(num, "E", lisp_exp);
			
			if (!exponents)
				Printf(num, "%s0", lisp_exp);
			
			if (exponents > 1 || (exponents + Replaceall(num, ".", ".") == 0)) {
				Delete(num);
				num = 0;
			}
		}
		return num;
	} else if (SwigType_type(type) == T_CHAR) {
		/* Use CL syntax for character literals */
		Delete(num);
		return NewStringf("#\\%c", s[2]);
	} else if (SwigType_type(type) == T_STRING) {
		/* Use CL syntax for string literals */
		Delete(num);
		return NewStringf("\"%s\"", num_param);
	} else if (SwigType_type(type) == T_INT) {
		Replaceall(num, "u", "");
		Replaceall(num, "U", "");
		Replaceall(num, "l", "");
		Replaceall(num, "L", "");
		
		int i, j;
		if (sscanf(s, "%d >> %d", &i, &j) == 2) {
		Delete(num);
		return NewStringf("(cl:ash %d -%d)", i, j);
	} else if (sscanf(s, "%d << %d", &i, &j) == 2) {
		Delete(num);
		return NewStringf("(cl:ash %d %d)", i, j);
	}
	}
	
	if (Len(num) >= 2 && s[0] == '0') {	/* octal or hex */
		Delete(num);
	if (s[1] == 'x')
		return NewStringf("#x%s", s + 2);
	else
		return NewStringf("#o%s", s + 1);
	}
	return num;
}



// less flexible as it does the conversion in C, the lispify name does the conversion in lisp
String *JAZZSCHEME::lispy_name(char *name) {
	bool helper = false;
	String *new_name = NewString("");
	for (unsigned int i = 0; i < strlen(name); i++) {
		if (name[i] == '_' || name[i] == '-') {
			Printf(new_name, "%c", '-');
			helper = false;
		} else if (name[i] >= 'A' && name[i] <= 'Z') {
			if (helper)
				Printf(new_name, "%c", '-');
			Printf(new_name, "%c", ('a' + (name[i] - 'A')));
			helper = false;
		} else {
			helper = true;
			Printf(new_name, "%c", name[i]);
		}
	}
	return new_name;
}



extern "C" Language *swig_jazzscheme(void) {
	return new JAZZSCHEME();
}
