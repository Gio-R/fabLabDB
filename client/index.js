window.onload = function() {
    document.getElementById("insert_person").onclick = () => insertPerson();
    document.getElementById("show_people").onclick = () => showPeople();
}

function insertPerson() {
    clearPage();
    var form = document.getElementById("input_form");
    form.classList.remove("hidden");
    var cfInput = createTextInput("cf", "Codice fiscale");
    var nameInput = createTextInput("name", "Nome");
    var surnameInput = createTextInput("surname", "Cognome");
    var button = document.createElement("button");
    button.type = "button";
    button.innerHTML = "Inserisci";
    form.appendChild(cfInput);
    form.appendChild(nameInput);
    form.appendChild(surnameInput);
    form.appendChild(button);
    button.onclick = () => sendInsertData("input_form", "insert_person");
}

function showPeople() {
    clearPage();
    var form = document.getElementById("filters_form");
    form.classList.remove("hidden");
    // TODO: apply filters
    createFiltersCheckBoxes(form, ["partners", "Soci"], ["cutters", "Operatori intagliatrice"], ["printers", "Operatori stampante"]);
    var resultDiv = document.getElementById("result");
    var resultList = document.getElementById("result_list");
    resultList.classList.remove("hidden");
    fetch(window.location.protocol + "//" + window.location.host.toString() + "/" + "people").then(function(response) {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    resultDiv.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponse)) {
                    for (const index in jsonResponse) {
                        var listElem = document.createElement("li");
                        var person = jsonResponse[index];
                        listElem.classList.add("result_elem");
                        listElem.innerHTML = person._personNome + " " + person._personCognome + " -- " + person._personCf;
                        resultList.appendChild(listElem);
                    }
                }
            });
        }
    });        
}

function clearPage() {
    var hideableElements = document.getElementsByClassName("hideable");
    for (var i = 0; i < hideableElements.length; i++) {
        hideableElements[i].classList.add("hidden");
        while (hideableElements[i].firstChild) {
            hideableElements[i].removeChild(hideableElements[i].firstChild);
        }
    }
}

function createFiltersCheckBoxes(form, ...checkBoxes) {
    checkBoxes.forEach(box => {
        var input = document.createElement("input");
        var label = document.createElement("label");
        input.type = "radio";
        input.name = "radio_" + form.id;
        input.id = box[0];
        label.for = input.id;
        label.innerHTML = box[1];
        form.appendChild(input);
        form.appendChild(label);
        form.appendChild(document.createElement("br"));
    });
}

function sendInsertData(formId, route) {
    var form = document.getElementById(formId);
    var fd = new FormData(form);
    var post = {
        method: "POST",
        body: fd
    };
    var errors = form.getElementsByClassName("error");
    while (errors.length > 0) {
        errors.item(0).parentNode.removeChild(errors.item(0));
    }
    var oks = form.getElementsByClassName("ok");
    while (oks.length > 0) {
        oks.item(0).parentNode.removeChild(oks.item(0));
    }
    fetch(window.location.protocol + "//" + window.location.host.toString() + "/" + route, post).then(function(response) {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] == "200") {
                    var ok = document.createElement("P");
                    ok.innerHTML = jsonResponse["response"]["message"];
                    ok.classList.add("ok");
                    form.appendChild(ok);
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

function getJsonData(route) {
    fetch(window.location.href.toString() + route).then(function(response) {
        if (response.ok) {
            response.json().then(jsonResponse => {return jsonResponse});
        }
    });
}

function checkIfJsonIsError(json) {
    return json.hasOwnProperty("response");
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