//==============
//	JazzScheme
//==============
//
///	DDE
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



EXTERN HDDEDATA CALLBACK DdeCallback(UINT uType, UINT uFmt, HCONV hconv, HSZ hsz1, HSZ hsz2, HDDEDATA hdata, DWORD dwData1, DWORD dwData2)
{
	static jObject _DDE_SERVER	= intern(L"DDE-Server");
	static jObject _CALLBACK	= intern(L"callback");

	static jDefinition	definition;
	static jObject		singleton;

	if (! definition)
	{
		definition = castDefinition(find_unit_field(load_unit(_DDE_SERVER)->get_class(), _CALLBACK));
		singleton = load_unit(_DDE_SERVER);
	}

	return (HDDEDATA) fetch_tInt(call(NOOBJECT, singleton, definition, put_tInt(uType), put_tInt((tInt) hsz1), put_tInt((tInt) hsz2), put_tInt((tInt) hdata)));
}



JAZZAPI tInt JAZZCALL JzGetDdeCallback()
{
	return (tInt) DdeCallback;
}
