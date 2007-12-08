//==============
//	JazzScheme
//==============
//
///	Java Primitive Types
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
#include "JJavaPrimitive.h"
#include "JJavaObject.h"
#include "JTyperef.h"
#include "JLong.h"
#include "JReal.h"

#include "jni.h"



DEFINE_CLASS(L"Java-Primitive", JavaPrimitive, Primitive)



CONSTRUCTOR JJavaPrimitive::JJavaPrimitive(cObject model, tString n, jType a) : JPrimitive(model, n, a)
{
}



METHOD tInt JJavaPrimitive::get_size()
{
	return sizeof(jvalue);
}



METHOD void JJavaPrimitive::print(jPrinter p, PrintDetail detail)
{
	p->start_unreadable(this, false);
	p->output_string(L"Java-Primitive ");
	get_symbol()->print(p, detail);
	p->end_unreadable(this);
}



JAZZAPI tBOOL JAZZCALL JzIsJavaPrimitive(jObject object)
{
	return isJavaPrimitive(object);
}



//------------
///	JavaVoid
//------------



JAVA_PRIMITIVE(JavaVoidPrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
END_JAVA_PRIMITIVE(JavaVoidPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javavoid", JavaVoidPrimitive)



METHOD jObject JJavaVoidPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jnienv()->CallVoidMethodA(object, methodID, args);
	check_java_exception();
	
	return Null;
}



METHOD jObject JJavaVoidPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jnienv()->CallStaticVoidMethodA(clazz, methodID, args);
	check_java_exception();
	
	return Null;
}



//------------
///	JavaBool
//------------



JAVA_PRIMITIVE(JavaBoolPrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaBoolPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javabool", JavaBoolPrimitive)



METHOD jObject JJavaBoolPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jboolean	result;
	
	result = jnienv()->CallBooleanMethodA(object, methodID, args);
	check_java_exception();
	
	return put_tBool(result != 0);
}



METHOD jObject JJavaBoolPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jboolean	result;
	
	result = jnienv()->CallStaticBooleanMethodA(clazz, methodID, args);
	check_java_exception();
	
	return put_tBool(result != 0);
}



METHOD void JJavaBoolPrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	(* address).z = fetch_tBool(object);
}



//------------
///	JavaByte
//------------



JAVA_PRIMITIVE(JavaBytePrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaBytePrimitive)



DEFINE_JAVA_PRIMITIVE(L"javabyte", JavaBytePrimitive)



METHOD jObject JJavaBytePrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jbyte	result;
	
	result = jnienv()->CallByteMethodA(object, methodID, args);
	check_java_exception();
	
	return put_tInt(result);
}



METHOD jObject JJavaBytePrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jbyte	result;
	
	result = jnienv()->CallStaticByteMethodA(clazz, methodID, args);
	check_java_exception();
	
	return put_tInt(result);
}



METHOD void JJavaBytePrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	(* address).b = fetch_tInt(object);
}



//------------
///	JavaChar
//------------



JAVA_PRIMITIVE(JavaCharPrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaCharPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javachar", JavaCharPrimitive)



METHOD jObject JJavaCharPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jchar	result;
	
	result = jnienv()->CallCharMethodA(object, methodID, args);
	check_java_exception();
	
	return put_tChar(result);
}



METHOD jObject JJavaCharPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jchar	result;
	
	result = jnienv()->CallStaticCharMethodA(clazz, methodID, args);
	check_java_exception();
	
	return put_tChar(result);
}



METHOD void JJavaCharPrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	(* address).c = fetch_tChar(object);
}



//-------------
///	JavaShort
//-------------



JAVA_PRIMITIVE(JavaShortPrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaShortPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javashort", JavaShortPrimitive)



METHOD jObject JJavaShortPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jshort	result;
	
	result = jnienv()->CallShortMethodA(object, methodID, args);
	check_java_exception();
	
	return put_tInt(result);
}



METHOD jObject JJavaShortPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jshort	result;
	
	result = jnienv()->CallStaticShortMethodA(clazz, methodID, args);
	check_java_exception();
	
	return put_tInt(result);
}



METHOD void JJavaShortPrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	(* address).s = fetch_tInt(object);
}



//-----------
///	JavaInt
//-----------



JAVA_PRIMITIVE(JavaIntPrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaIntPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javaint", JavaIntPrimitive)



METHOD jObject JJavaIntPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jint	result;
	
	result = jnienv()->CallIntMethodA(object, methodID, args);
	check_java_exception();
	
	return put_tInt(result);
}



METHOD jObject JJavaIntPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jint	result;
	
	result = jnienv()->CallStaticIntMethodA(clazz, methodID, args);
	check_java_exception();
	
	return put_tInt(result);
}



METHOD void JJavaIntPrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	(* address).i = fetch_tInt(object);
}



//------------
///	JavaLong
//------------



JAVA_PRIMITIVE(JavaLongPrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaLongPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javalong", JavaLongPrimitive)



METHOD jObject JJavaLongPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jlong	result;
	
	result = jnienv()->CallLongMethodA(object, methodID, args);
	check_java_exception();
	
	return put_tLong(result);
}



METHOD jObject JJavaLongPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jlong	result;
	
	result = jnienv()->CallStaticLongMethodA(clazz, methodID, args);
	check_java_exception();
	
	return put_tLong(result);
}



METHOD void JJavaLongPrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	(* address).j = fetch_tLong(object);
}



//-------------
///	JavaFloat
//-------------



JAVA_PRIMITIVE(JavaFloatPrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaFloatPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javafloat", JavaFloatPrimitive)



METHOD jObject JJavaFloatPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jfloat	result;
	
	result = jnienv()->CallFloatMethodA(object, methodID, args);
	check_java_exception();
	
	return put_tFloat(result);
}



METHOD jObject JJavaFloatPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jfloat	result;
	
	result = jnienv()->CallStaticFloatMethodA(clazz, methodID, args);
	check_java_exception();
	
	return put_tFloat(result);
}



METHOD void JJavaFloatPrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	(* address).f = fetch_tFloat(object);
}



//--------------
///	JavaDouble
//--------------



JAVA_PRIMITIVE(JavaDoublePrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaDoublePrimitive)



DEFINE_JAVA_PRIMITIVE(L"javadouble", JavaDoublePrimitive)



METHOD jObject JJavaDoublePrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jdouble	result;
	
	result = jnienv()->CallDoubleMethodA(object, methodID, args);
	check_java_exception();
	
	return put_tDouble(result);
}



METHOD jObject JJavaDoublePrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jdouble	result;
	
	result = jnienv()->CallStaticDoubleMethodA(clazz, methodID, args);
	check_java_exception();
	
	return put_tDouble(result);
}



METHOD void JJavaDoublePrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	(* address).d = fetch_tDouble(object);
}



//--------------
///	JavaString
//--------------



JAVA_PRIMITIVE(JavaStringPrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		jObject		get_array_elements	(jarray);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaStringPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javastring", JavaStringPrimitive)



METHOD jObject JJavaStringPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jstring string;

	string = (jstring) jnienv()->CallObjectMethodA(object, methodID, args);
	check_java_exception();

	if (string != NULL)
		return put_tString((tString) jnienv()->GetStringChars(string, 0));
	else
		return Nil;
}



METHOD jObject JJavaStringPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jstring string;

	string = (jstring) jnienv()->CallStaticObjectMethodA(clazz, methodID, args);
	check_java_exception();

	if (string != NULL)
		return put_tString((tString) jnienv()->GetStringChars(string, 0));
	else
		return Nil;
}



METHOD jObject JJavaStringPrimitive::get_array_elements(jarray a)
{
	JQueue	queue(getQueueClass());

	jobjectArray array = (jobjectArray) a;

	jsize size = jnienv()->GetArrayLength(array);

	for (int n = 0; n < size; n++)
	{
		jstring string = (jstring) jnienv()->GetObjectArrayElement(array, n);
		queue.enqueue(put_tString((tString) jnienv()->GetStringChars(string, 0)));
	}

	return queue.queue_list();
}



METHOD void JJavaStringPrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	if (object == Nil)
	{
		(* address).l = NULL;
	}
	else
	{
		jstring	result;
	
		result = jnienv()->NewString((const jchar *) fetch_tString(object), length(object));
		(* address).l = result;
	}
}



//-------------
///	JavaArray
//-------------



#ifndef SEALED
JAVA_PRIMITIVE(JavaArrayPrimitive)
	public:
		jType		get_array_type		(jTyperef);

		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaArrayPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javaarray", JavaArrayPrimitive)



METHOD jType JJavaArrayPrimitive::get_array_type(jTyperef typeref)
{
	return castTyperef(car(typeref->parametersGet()))->get_type();
}



METHOD jObject JJavaArrayPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jarray array;
	
	array = (jarray) jnienv()->CallObjectMethodA(object, methodID, args);
	check_java_exception();

	if (array != NULL)
		return get_array_type(castTyperef(typeref))->get_array_elements(array);
	else
		return Nil;
}



METHOD jObject JJavaArrayPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jarray array;
	
	array = (jarray) jnienv()->CallStaticObjectMethodA(clazz, methodID, args);
	check_java_exception();

	if (array != NULL)
		return get_array_type(castTyperef(typeref))->get_array_elements(array);
	else
		return Nil;
}



METHOD void JJavaArrayPrimitive::write_value(jObject typeref, jvalue* address, jObject list)
{
	if (list == Nil)
		(* address).l = NULL;
	else
	{
		tInt			len;
		jObject			string;
		jobjectArray	array;
		jstring			result;
		
		len = length(list);
		array = jnienv()->NewObjectArray(len, jnienv()->FindClass("java/lang/String"), NULL);
	
		for (int n = 0; n < len; n++)
		{
			string = car(list);
			result = jnienv()->NewString((const jchar *) fetch_tString(string), length(string));
			jnienv()->SetObjectArrayElement(array, n, result);
			list = cdr(list);
		}
		
		(* address).l = array;
	}
}
#endif



//--------------
///	JavaObject
//--------------



JAVA_PRIMITIVE(JavaObjectPrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaObjectPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javaobject", JavaObjectPrimitive)



METHOD jObject JJavaObjectPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jobject	result;
	
	result = jnienv()->CallObjectMethodA(object, methodID, args);
	check_java_exception();
	
	if (result != NULL)
		return newJavaObject(getJavaObjectClass(), result);
	else
		return Nil;
}



METHOD jObject JJavaObjectPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jobject	result;
	
	result = jnienv()->CallStaticObjectMethodA(clazz, methodID, args);
	check_java_exception();
	
	if (result != NULL)
		return newJavaObject(getJavaObjectClass(), result);
	else
		return Nil;
}



METHOD void JJavaObjectPrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	if (object == Nil)
		(* address).l = NULL;
	else
		(* address).l = castJavaObject(object)->java_objectGet();
}



//-------------
///	JavaClass
//-------------



JAVA_PRIMITIVE(JavaClassPrimitive)
	public:
		jObject		call_method			(jObject, jobject, jmethodID, const jvalue *);
		jObject		call_static_method	(jObject, jclass, jmethodID, const jvalue *);
		void		write_value			(jObject, jvalue*, jObject);
END_JAVA_PRIMITIVE(JavaClassPrimitive)



DEFINE_JAVA_PRIMITIVE(L"javaclass", JavaClassPrimitive)



METHOD jObject JJavaClassPrimitive::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jobject	result;
	
	result = jnienv()->CallObjectMethodA(object, methodID, args);
	check_java_exception();
	
	if (result != NULL)
		return newJavaObject(getJavaObjectClass(), result);
	else
		return Nil;
}



METHOD jObject JJavaClassPrimitive::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jobject	result;
	
	result = jnienv()->CallStaticObjectMethodA(clazz, methodID, args);
	check_java_exception();
	
	if (result != NULL)
		return newJavaObject(getJavaObjectClass(), result);
	else
		return Nil;
}



METHOD void JJavaClassPrimitive::write_value(jObject typeref, jvalue* address, jObject object)
{
	if (object == Nil)
		(* address).l = NULL;
	else
		(* address).l = castJavaObject(object)->java_objectGet();
}
