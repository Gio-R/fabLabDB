window.onload = function() {
    document.getElementById("insert_person").onclick = () => insertPerson();
    document.getElementById("show_people").onclick = () => showPeople();
}

/* creates the form and sends the data to insert a new person in the database */
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

/* creates the list to show the people in the database, with the appropriate filters */
function showPeople() {
    clearPage();
    var form = document.getElementById("filters_form");
    form.classList.remove("hidden");
    createFiltersCheckBoxes(form, "people_type", ["all", "Tutti", "people"], ["partners", "Soci", "partners"], ["cutters", "Operatori intagliatrice", "cutterOperators"], ["printers", "Operatori stampante", "printerOperators"]);
    setTable("people");
}

/* sets the content of the "result_area" element with the results from the given route */
function setTable(route) {
    var resultDiv = document.getElementById("result_area");
    fetch(window.location.protocol + "//" + window.location.host.toString() + "/" + route).then(function(response) {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    showClearElem("result_area");
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    resultDiv.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponse)) {
                    showClearElem("result_area");
                    var resultTable = createTable("result_table", "headers", ["cf", "Codice fiscale"], ["name", "Nome"], ["surname", "Cognome"], ["expense", "Spesa totale"]);
                    for (const index in jsonResponse) {
                        var listElem = document.createElement("tr");
                        var person = jsonResponse[index];
                        listElem.classList.add("result_elem");
                        var nameCell = document.createElement("td");
                        nameCell.innerHTML = person._personNome;
                        var surnameCell = document.createElement("td");
                        surnameCell.innerHTML = person._personCognome;
                        var cfCell = document.createElement("td");
                        cfCell.innerHTML = person._personCf;
                        var expenseCell = document.createElement("td");
                        expenseCell.innerHTML = person._personSpesaTotale + " €";
                        listElem.append(cfCell, nameCell, surnameCell, expenseCell);
                        resultTable.appendChild(listElem);
                    }
                    resultDiv.appendChild(resultTable);
                }
            });
        }
    });
}

/* clears the page */
function clearPage() {
    var hideableElements = document.getElementsByClassName("hideable");
    for (var i = 0; i < hideableElements.length; i++) {
        hideableElements[i].classList.add("hidden");
        while (hideableElements[i].firstChild) {
            hideableElements[i].removeChild(hideableElements[i].firstChild);
        }
    }
}

/* makes visible the element with elemId, as if it was created anew */
function showClearElem(elemId) {
    var elem = document.getElementById(elemId);
    while(elem.firstChild) {
        elem.removeChild(elem.firstChild);
    }
    elem.classList.remove("hidden");
}

/* creates a variable number of checkboxes into the form, with [name, label, route] properties */
function createFiltersCheckBoxes(form, name, ...checkBoxes) {
    checkBoxes.forEach(box => {
        var input = document.createElement("input");
        var label = document.createElement("label");
        input.type = "radio";
        input.name = name;
        input.id = box[0];
        input.onclick = () => { if (input.checked) { setTable(box[2]); } };
        label.for = input.id;
        label.innerHTML = box[1];
        form.appendChild(input);
        form.appendChild(label);
        form.appendChild(document.createElement("br"));
    });
}

/* sends to the said route the content of the given form, giving a visual notice of the result of the operation */
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

/* creates a text input, with the given name and placeholder */
function createTextInput(name, placeholder) {
    var input = document.createElement("input");
    input.type = "text";
    input.name = name;
    input.placeholder = placeholder;
    return input;
}

/* creates a table with the given id, id of the headers and columns [headerId, name] */
function createTable(tableId, headersId, ...headers) {
    var resultTable = document.createElement("table");
    resultTable.id = tableId;
    var header = document.createElement("tr")
    header.id = headersId;
    resultTable.appendChild(header);
    headers.forEach(h => {
        var th = document.createElement("th");
        th.id = h[0];
        th.innerHTML = h[1];
        header.appendChild(th);
    });
    return resultTable;
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