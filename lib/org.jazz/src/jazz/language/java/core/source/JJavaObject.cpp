//==============
//	JazzScheme
//==============
//
///	Java Objects
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
//	Portions created by the Initial Developer are Copyright (C) 1996-2008
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
#include "JJavaObject.h"
#include "JStubable.h"
#include "JAnsiString.h"



DEFINE_CLASS(L"Java-Object", JavaObject, Object)



METHOD jObject CJavaObject::allocate(jType model, jObjectPtr)
{
	return newJavaObject(castClass(model), NULL);
}



METHOD jObject CJavaObject::call_method(jObject typeref, jobject object, jmethodID methodID, const jvalue *args)
{
	jobject	result;

	result = jnienv()->CallObjectMethodA(object, methodID, args);
	check_java_exception();

	if (result != NULL)
		return newJavaObject(this, result);
	else
		return Nil;
}



METHOD jObject CJavaObject::call_static_method(jObject typeref, jclass clazz, jmethodID methodID, const jvalue *args)
{
	jobject	result;

	result = jnienv()->CallStaticObjectMethodA(clazz, methodID, args);
	check_java_exception();

	if (result != NULL)
		return newJavaObject(this, result);
	else
		return Nil;
}



METHOD jObject CJavaObject::get_array_elements(jarray a)
{
	JQueue	queue(getQueueClass());

	jobjectArray array = (jobjectArray) a;

	jsize size = jnienv()->GetArrayLength(array);

	for (int n = 0; n < size; n++)
		queue.enqueue(newJavaObject(this, jnienv()->GetObjectArrayElement(array, n)));

	return queue.queue_list();
}



METHOD void CJavaObject::write_value(jObject typeref, jvalue* address, jObject object)
{
	(* address).l = castJavaObject(object)->java_objectGet();
}



STATIC jAnsiString get_classname(jUnit unit)
{
	DEFINITION_META(Java, period2slash_notation_ansi, L"Java", L"period->slash-notation-ansi")
	
	return castAnsiString(call(NOOBJECT, Java, period2slash_notation_ansi, get_name(unit->get_symbol())));
} ES



METHOD jclass CJavaObject::find_class()
{
	jObject	classname;
	jclass	java_class;

	classname = get_classname(this);
    java_class = jnienv()->FindClass(fetch_tAnsiString(classname));
	check_java_exception();
	
	// This is probably not necessary because of the check above (to verify)
	if (! java_class)
		error(L"Unable to find java class {a}", classname);

	return java_class;
}



CONSTRUCTOR JJavaObject::JJavaObject(cObject model, jobject object) : JObject(model)
{
	INIT(java_object, object)
}



DESTRUCTOR JJavaObject::~JJavaObject()
{
}



//------------
///	External
//------------



JAZZAPI long JAZZCALL JzGetJavaClass(cObject model)
{
	return (long) castJavaObjectClass(model)->find_class();
}



JAZZAPI long JAZZCALL JzGetJavaObject(jObject jazz)
{
	return (long) castJavaObject(jazz)->java_objectGet();
}



JAZZAPI void JAZZCALL JzSetJavaObject(jObject jazz, long java)
{
	castJavaObject(jazz)->java_objectSet((jobject) java);
}
