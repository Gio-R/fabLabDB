document.getElementById("submit_button").onclick = function() {
    var xhr = new XMLHttpRequest();
    var fd = new FormData(document.getElementById("login_form"));
    xhr.onreadystatechange = function() { 
        if (xhr.readyState == 4 && xhr.status != 200) {
            var error = JSON.parse(xhr.responseText).error.message;
            var text = document.createTextNode("Login fallito");
            document.getElementById("container").appendChild(text);
        }
    }
    xhr.open("POST", window.location.href.toString() + "/login");
    xhr.send(fd);
}