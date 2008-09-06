//==============
//	JazzScheme
//==============
//
///	Java Virtual Machine
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
#include "JJVM.h"
#include "JJavaObject.h"
#include "JAnsiString.h"
#include "JVector.h"
#include "JParameter.h"
#include "JException.h"



DEFINE_CLASS(L"JVM", JVM, Object)



JNIEXPORT jint JNICALL Java_org_jazzscheme_java_Runtime_callJazz(JNIEnv *, jobject, jint)
{
	debugger(L"Called from Java!!!");
	return 5;
}



STATIC jString JVMPath = NULL; ES
STATIC jAnsiString JVMOption = NULL; ES
STATIC jJVM JVM = NULL; ES



ROOT(JVMSetup)
	if (JVMPath)
		JVMPath->mark(NoContainer);
	if (JVMOption)
		JVMOption->mark(NoContainer);
	if (JVM)
		JVM->mark(NoContainer);
END_ROOT(JVMSetup)



METHOD jObject CJVM::allocate(jType model, jObjectPtr rest)
{
	return newJVM(castClass(model));
}



CONSTRUCTOR JJVM::JJVM(cObject model) : JObject(model)
{
	typedef jint (JNICALL *CreateJavaVM)(JavaVM** pvm, void** env, void* args);
	typedef jint (JNICALL *GetDefaultJavaVMInitArgs)(void* args);
	
	HINSTANCE library;

	CreateJavaVM JNI_CreateJavaVM;
	GetDefaultJavaVMInitArgs JNI_GetDefaultJavaVMInitArgs;

	if (! JVMPath)
		error(L"Setup information unavailable to create JVM");

	if ((library = LoadLibrary(fetch_tString(JVMPath))) == 0)
		error(L"Unable to load jvm.dll");
	
	JNI_CreateJavaVM = (CreateJavaVM) GetProcAddress(library, "JNI_CreateJavaVM");
	if (JNI_CreateJavaVM == 0)
		error(L"Unable to find JNI_CreateJavaVM entry point in jvm.dll");
	
	JNI_GetDefaultJavaVMInitArgs = (GetDefaultJavaVMInitArgs) GetProcAddress(library, "JNI_GetDefaultJavaVMInitArgs");
	if (JNI_GetDefaultJavaVMInitArgs == 0)
		error(L"Unable to find JNI_GetDefaultJavaVMInitArgs entry point in jvm.dll");
	
	jint err;
	
	JavaVM* vm;
	JNIEnv* env;

	JavaVMInitArgs args;
	JavaVMOption options[1];
	
    memset(&args, 0, sizeof(args));
	args.version = JNI_VERSION_1_2;
	args.nOptions = 1;
	options[0].optionString = fetch_tAnsiString(JVMOption);
	args.options = options;
	args.ignoreUnrecognized = JNI_FALSE;

    err = JNI_CreateJavaVM(&vm, (void**)&env, &args);

	if (err != 0)
		error(L"Unable to create JavaVM");
	
	javavm = vm;
	GetActiveThread()->jnienvSet(env);

	jclass clazz = env->FindClass("org/jazzscheme/java/Runtime");
	JNINativeMethod methods[1];
	methods[0].name = "callJazz";
	methods[0].signature = "(I)I";
	methods[0].fnPtr = (void*) Java_org_jazzscheme_java_Runtime_callJazz;
	env->RegisterNatives(clazz, methods, 1);
}



METHOD void JJVM::destroy_jvm()
{
	// Doesn't really work as Java wait's for the main thread to exit before actually unloading...
	if (javavm)
	{
		javavm->DestroyJavaVM();
		javavm = NULL;
	}
	JVM = NULL;
}



EXTERN jJVM jvm()
{
	if (! JVM)
		JVM = newJVM(getJVMClass());

	return JVM;
}



EXTERN JavaVM* javavm()
{
	return jvm()->javavmGet();
}



EXTERN JNIEnv* jnienv()
{
	jThread	thread;
	JNIEnv*	env;

	thread = GetActiveThread();
	env = thread->jnienvGet();
	if (env)
		return env;
	else
	{
		JavaVM*	vm;

		vm = javavm();

		vm->AttachCurrentThread((void**) &env, NULL);

		thread->jnienvSet(env);

		return env;
	}
}



EXTERN void detach_jni_thread()
{
	JavaVM*	vm;

	vm = javavm();

	vm->DetachCurrentThread();
}



EXTERN void check_java_exception()
{
	static jObject JAVA_EXCEPTION	= intern(L"Java-Exception");
	static jObject THROWABLE		= intern(L"java.lang.Throwable");

	jthrowable	throwable;
	
	throwable = jnienv()->ExceptionOccurred();

	if (throwable != NULL)
	{
		jnienv()->ExceptionClear();
		signal(make_object(JAVA_EXCEPTION, newJavaObject(castClass(load_unit(THROWABLE)), throwable)));
	}
}



EXTERN void clear_java_exception()
{
	jnienv()->ExceptionClear();
}



//------------
///	External
//------------



JAZZAPI void JAZZCALL JzJVMSetup(jObject jvm_path, jObject jvm_option)
{
	JVMPath = castString(jvm_path);
	JVMOption = castAnsiString(jvm_option);
}



JAZZAPI void JAZZCALL JzJVMDestroy()
{
	jvm()->destroy_jvm();
}



JAZZAPI long JAZZCALL JzJVMFindClass(tAnsiString name)
{
    long	clazz;
	
	clazz = (long) jnienv()->FindClass(name);
	check_java_exception();

	return clazz;
}



JAZZAPI tInt JAZZCALL JzJVMNewObject(long cls, long methodid, jObject parameter_types, jObjectPtr ptr)
{
#ifndef SEALED
	jObject		object;
	jParameter	parameter;
	jTyperef	typeref;
	jType		type;
	jObjectPtr	parameters;
	jvalue		arguments[32];
	int			i, n;
	long		result;

//	int n = 0;
//	while (*ptr)
//		args[n++].i = fetch_tInt(*ptr++);
	
	parameters = castVector(parameter_types)->contentGet();
	n = 0;
	for (i = length(parameter_types); i > 0; i--)
	{
		parameter = castParameter(*parameters);
		typeref = parameter->typerefGet();
		type = typeref->get_type();
		parameters++;

		object = *ptr++;

		type->write_value(typeref, &arguments[n], object);
		n++;
	}

    result = (long) jnienv()->NewObjectA((jclass) cls, (jmethodID) methodid, arguments);
    check_java_exception();
    
    return result;
#else
	return 0;
#endif
}



JAZZAPI long JAZZCALL JzJVMGetMethodID(long cls, tAnsiString name, tAnsiString sig)
{
	long	result;
	
    result = (long) jnienv()->GetMethodID((jclass) cls, name, sig);
    check_java_exception();
    
    return result;
}
