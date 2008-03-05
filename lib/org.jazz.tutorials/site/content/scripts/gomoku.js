//==============
//  JazzScheme
//==============
//
///	Gomoku JavaScript
//
//	Filename: gomoku.js
//	Creators: Guillaume Cartier
//


function playMove(move, form, url) {
	document['Form'].move.value = move;
	submitForm(form, url);
}


function gotoMove(rank, form, url) {
	document['Form'].goto.value = rank;
	submitForm(form, url);
}
