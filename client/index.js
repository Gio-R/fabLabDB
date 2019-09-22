window.onload = function() {
    document.getElementById("insert_person").onclick = () => insertPerson();
}

function insertPerson() {
    var inputDiv = document.getElementById("user_input");
    inputDiv.innerHTML = "";
    var form = document.createElement("form");
    form.method = "POST";
    form.enctype = "multipart/form-data";
    form.classList.add("input_form");
    var cfInput = createTextInput("cf", "Codice fiscale");
    var nameInput = createTextInput("name", "Nome");
    var surnameInput = createTextInput("surname", "Cognome");
    var button = document.createElement("button");
    button.type = "button";
    button.innerHTML = "Inserisci";
    button.onclick = sendInsertData(form, "add_person");
    form.appendChild(cfInput);
    form.appendChild(nameInput);
    form.appendChild(surnameInput);
    form.appendChild(button);
    inputDiv.appendChild(form);
}

function sendInsertData(form, route) {
    var fd = new FormData(form);
    var post = {
        method: "POST",
        body: form
    };
    fetch(window.location.href.toString() + route, post).then(function(response) {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                }
            });
        }
    });
}

function createTextInput(name, placeholder) {
    var input = document.createElement("input");
    input.type = "text";
    input.name = name;
    input.placeholder = placeholder;
    return input;
}

function setJsonData(url, selectId, setter) {
    var html_code = "";
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() { 
        if (xhr.readyState == 4 && xhr.status == 200)
            setter(selectId, JSON.parse(xhr.responseText));
    }
    xhr.open("GET", url, true);
    xhr.send();
}

function setSelectList(selectId, options) {
    var selectElement = document.getElementById(selectId);
    while (selectElement.options.length) {
        selectElement.remove(0);
    }
    for (var i = 0; i < options.length; i++) {
        var opt = options[i];
        var str = opt._personNome.concat(" ", opt._personCognome, " ", opt._personCf);
        selectElement.options.add(new Option(str, i));
    }
}

function setAllPeopl(selectId) {
    setJsonData(window.location.href + "people", selectId, setSelectList);
}