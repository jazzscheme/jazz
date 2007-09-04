//==============
//	JazzScheme
//==============
//
///	JavaPrimitive Header
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
#ifndef JJavaPrimitive_H_JAZZ
#define JJavaPrimitive_H_JAZZ



#include "JPrimitive.h"

#include "jni.h"



/**
 * This class implements Java primitive objects that cannot exist as real Jazz objects.
 * It is used to map typeless Jazz objects to Java primitive typed objects.
 */



CLASS(JavaPrimitive, Primitive)
END_CLASS(JavaPrimitive)



OBJECT(JavaPrimitive, Primitive)
	public:
							JJavaPrimitive		(cObject, tString, jType);

				tInt		get_size			();
				void		print				(jPrinter, PrintDetail);
END_OBJECT(JavaPrimitive)



NEW2(JavaPrimitive, tString, jType)



#define JAVA_PRIMITIVE(type)								\
	typedef class J##type* j##type;							\
															\
	extern j##type The##type;								\
	extern j##type new##type();								\
	PRIMITIVE_GETTER(j##type, type)							\
															\
	class J##type : public JJavaPrimitive					\
	{														\
		public:												\
			J##type(cObject m, tString n, jType a)			\
				: JJavaPrimitive(m, n, a)					\
			{												\
			}



#define END_JAVA_PRIMITIVE(type)	\
	};



#define JAVA_PRIMITIVE_MAKER(c, n, t, b)					\
	c* The##t;												\
	c* new##t()												\
	{														\
		c*	type;											\
															\
		type = new c(getJavaPrimitiveClass(), n, b);		\
		type->finalize();									\
															\
		return type;										\
	}



#define DEFINE_JAVA_PRIMITIVE(name, type)								\
	JAVA_PRIMITIVE_MAKER(J##type, name, type, getJavaPrimitiveClass())	\
	static J##type* __dummy##type = get##type();



#endif
