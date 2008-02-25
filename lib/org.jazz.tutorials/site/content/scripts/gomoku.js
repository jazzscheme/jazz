//==============
//  JazzScheme
//==============
//
///	Gomoku JavaScript
//
//	Filename: gomoku.js
//	Creators: Guillaume Cartier
//


//--------
/// Form
//--------


function playMove(move, form, url) {
	document['Form'].move.value = move;
	submitForm(form, url);
}
