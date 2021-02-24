// SAR Mon Mar 12 15:16:29 GMT 2012: This has now been changed to use + and -
// to expand and collapse the hidden </div>s. The + and - are also now always
// visible. 

function showHide(shID) {
    if (document.getElementById(shID)) {
	if (document.getElementById(shID+'-show').innerHTML != '-') {
	    document.getElementById(shID+'-show').innerHTML = '-';
            document.getElementById(shID).style.display = 'block';

	    // This must be called twice in order for the jump to anchor tags
	    // to work properly in SAFARI. 
	    // ALSO: Must be .href and not .hash for SAFARI

	    // window.location.hash=shID;
	    window.location.href='#'+shID.substring(0, shID.length - 4);
	    window.location.href='#'+shID.substring(0, shID.length - 4);
	}
	else {
	    document.getElementById(shID+'-show').innerHTML = '+';
            document.getElementById(shID).style.display = 'none';

	    // See SAFARI comment above
	    window.location.href='#'+shID.substring(0, shID.length - 4);
	    window.location.href='#'+shID.substring(0, shID.length - 4);
	}
    }
}

function jumpShow(shID) {
    if (document.getElementById(shID)) {
	if (document.getElementById(shID+'-show').innerHTML != '-') {
	    document.getElementById(shID+'-show').innerHTML = '-';
	    document.getElementById(shID).style.display = 'block';

 	    window.location.href='#'+shID.substring(0, shID.length - 4);
	    window.location.href='#'+shID.substring(0, shID.length - 4);
	}
	else {
	    window.location.href='#'+shID.substring(0, shID.length - 4);
	    window.location.href='#'+shID.substring(0, shID.length - 4);
	}
    }
}