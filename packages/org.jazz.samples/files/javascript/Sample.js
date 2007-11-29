//
//==============
//	JazzScheme
//==============
//
///	Sample JavaScript
//
//	Filename: Sample.js
//	Creators: Guillaume Cartier
//


//-------------
/// Framework
//-------------


function displayURL(url) {
	window.open(url, getNewWindowName()).focus();
}


function getNewWindowName() {
	return new Date().getTime();
}


//--------
/// Form
//--------


function submitForm(form, url) {
	document[form].action = url;
	document[form].submit();
}
