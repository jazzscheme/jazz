//==============
//	JazzScheme
//==============
//
///	Sample C++ Code
//



#include "Jazz.h"
#include "JSample.h"



CONSTRUCTOR JSample::JSample(cObject model) : JObject(model)
{
#ifndef SEALED
	INIT(size, 0)
#endif
}



METHOD jObject JSample::get_size()
{
	return size;
}



EXTERN int sample()
{
	sample_test(size);
}



EXTERN void sample_test(int size)
{
	// some test
}
