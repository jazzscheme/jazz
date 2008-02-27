//==============
//  JazzScheme
//==============
//
///	C4 JavaScript
//
//	Filename: c4.js
//	Creators: Guillaume Cartier
//


//--------
/// Form
//--------


function playMove(move, form, url) {
	document['Form'].move.value = move;
	submitForm(form, url);
}
