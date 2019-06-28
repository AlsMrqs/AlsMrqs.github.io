function escape(s) {
	return '<script>console.log("'+s+'");</script>';
}

function check_lvl0(pass) {
	if (pass == "a senha") {
		alert("Correct Password!\nGo to lvl [ 1 ]!");
		window.location.href="chall_1.html";
	} else {
		alert("Invalid Password");
	}
}

function check_lvl1(pass) {
	if (pass == "n00b") {
		alert("Correct Password!\nLVL 2 is under construction!");
	} else {
		alert("Invalid Password!!");
	}
}