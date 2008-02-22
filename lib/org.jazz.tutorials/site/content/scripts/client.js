//==============
//  JazzScheme
//==============
//
///	Client Side JavaScript
//
//	Filename: client.js
//	Creators: Guillaume Cartier
//


//--------
/// Form
//--------


function submitForm(form, url) {
	document[form].action = url;
	document[form].submit();
}
