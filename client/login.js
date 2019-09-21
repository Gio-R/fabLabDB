window.onload = function() { 
    document.getElementById('submit_button').onclick = function() {
        var form = new FormData(document.getElementById('login_form'));
        var post = {
            method: 'POST',
            body: form
        };
        fetch(window.location.href.toString() + 'login', post).then(function(response) {
            if (!response.redirected && response.ok) {
                response.json().then(jsonResponse => {
                    if (jsonResponse["response"]["code"] == "401") {
                        var error = document.createElement("P");
                        error.innerHTML = jsonResponse["response"]["message"];
                        error.id = "login_error";
                        document.getElementById('login_form')
                                .appendChild(error);
                    }
                });
            } else if (response.redirected == true) {
                window.location.replace(response.url);
            }
        });
    }
}