window.onload = function() {
    /* people */
    document.getElementById("insert_person").onclick = () => insertPerson();
    document.getElementById("show_people").onclick = () => showPeople();
    document.getElementById("modify_person").onclick = () => modifyPerson();
    /* cuts */
    /* prints */
    /* materials */
    /* plastics */
    document.getElementById("insert_plastic").onclick = () => insertPlastic();
    document.getElementById("show_plastics").onclick = () => showPlastics();
    /* admins */
}

/*
----------------------------------------------------------------------------------------
PEOPLE FUNCTIONS
----------------------------------------------------------------------------------------
*/

/* creates the form and sends the data to insert a new person in the database */
function insertPerson() {
    clearPage();
    var form = document.getElementById("input_form");
    showClearElem(form.id);
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
    button.onclick = () => sendFormData("input_form", "insert_person");
}

/* creates the list to show the people in the database, with the appropriate filters */
function showPeople() {
    clearPage();
    var form = document.getElementById("filters_form");
    form.classList.remove("hidden");
    document.getElementById("filters_form").classList.remove("hidden");
    createFiltersRadioButtons(form, "people_type", ["all", "Tutti", "people"], ["partners", "Soci", "partners"], ["cutters", "Operatori intagliatrice", "cutterOperators"], ["printers", "Operatori stampante", "printerOperators"]);
    var resultTable = createTable("result_table", "headers", ["cf", "Codice fiscale"], ["name", "Nome"], ["surname", "Cognome"], ["expense", "Spesa totale"]);
    setTable("people", resultTable, (jsonResponse, table) => {
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
            table.appendChild(listElem);
        }
    });
}

/* show the appropriate forms to modify a person */
function modifyPerson() {
    clearPage();
    var form = document.getElementById("input_form");
    showClearElem(form.id);
    fetch(window.location.protocol + "//" + window.location.host.toString() + "/people").then(response => {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponse)) {
                    var list = document.createElement("select");
                    list.name = "cf";
                    list.id = "cf_select";
                    form.appendChild(list);
                    for (const index in jsonResponse) {
                        var elem = document.createElement("option");
                        elem.value = jsonResponse[index]._personCf;
                        elem.innerHTML = jsonResponse[index]._personNome + " " + jsonResponse[index]._personCognome + " -- " + jsonResponse[index]._personCf;
                        list.appendChild(elem);
                    }
                    list.onchange = () => changeCheckedBoxes();
                    changeCheckedBoxes();
                    form.appendChild(document.createElement("br"));
                    createFiltersCheckBoxes(form, ["partner", "true", "Socio"], ["cutter", "true", "Operatore intagliatrice"], ["printer", "true", "Operatore stampante"]);
                    createHiddenFiltersCheckBoxes(form, ["partner", "false", "Socio"], ["cutter", "false", "Operatore intagliatrice"], ["printer", "false", "Operatore stampante"]);
                    var okButton = document.createElement("button");
                    okButton.type = "button";
                    okButton.innerHTML = "Modifica";
                    okButton.onclick = () => {
                        disableCheckBoxes(form);
                        sendFormData("input_form", "modify_person")
                    };
                    form.appendChild(okButton);
                }
            });
        }
    });
}

/*
----------------------------------------------------------------------------------------
CUTS FUNCTIONS
----------------------------------------------------------------------------------------
*/

/*
----------------------------------------------------------------------------------------
PRINTS FUNCTIONS
----------------------------------------------------------------------------------------
*/

/*
----------------------------------------------------------------------------------------
MATERIALS FUNCTIONS
----------------------------------------------------------------------------------------
*/

/*
----------------------------------------------------------------------------------------
PLASTICS FUNCTIONS
----------------------------------------------------------------------------------------
*/

/* creates the form and sends the data to insert a new plastic in the database */
function insertPlastic() {
    clearPage();
    var form = document.getElementById("input_form");
    showClearElem(form.id);
    var plasticCode = createTextInput("code", "Codice plastica (3 caratteri)");
    var nameInput = createTextInput("name", "Nome");
    var descriptionInput = createTextInput("description", "Descrizione");
    var button = document.createElement("button");
    button.type = "button";
    button.innerHTML = "Inserisci";
    form.appendChild(plasticCode);
    form.appendChild(nameInput);
    form.appendChild(descriptionInput);
    form.appendChild(button);
    button.onclick = () => sendFormData("input_form", "insert_plastic");
}

/* creates the form and sends the data to insert a new filament in the database */
function insertFilament() {

}

/* shows the filaments, giving the possibility to select the plastic */
function showFilaments() {

}

/* shows the plastics in the database */
function showPlastics() {
    clearPage();
    var form = document.getElementById("filters_form");
    form.classList.remove("hidden");
    document.getElementById("filters_form").classList.add("hidden");
    var resultTable = createTable("result_table", "headers", ["code", "Codice"], ["name", "Nome"], ["description", "Descrizione"]);
    setTable("plastics", resultTable, (jsonResponse, table) => {
        for (const index in jsonResponse) {
            var listElem = document.createElement("tr");
            var plastic = jsonResponse[index];
            listElem.classList.add("result_elem");
            var codeCell = document.createElement("td");
            codeCell.innerHTML = plastic._plasticCodicePlastica;
            var nameCell = document.createElement("td");
            nameCell.innerHTML = plastic._plasticNome;
            var descrCell = document.createElement("td");
            descrCell.innerHTML = plastic._plasticDescrizione;
            listElem.append(codeCell, nameCell, descrCell);
            table.appendChild(listElem);
        }
    });
}

/*
----------------------------------------------------------------------------------------
ADMIN FUNCTIONS
----------------------------------------------------------------------------------------
*/

/*
----------------------------------------------------------------------------------------
GENERAL FUNCTIONS
----------------------------------------------------------------------------------------
*/

/* fetches data from the given route, and executes the given action */
function fetchData(route, action) {
    fetch(window.location.protocol + "//" + window.location.host.toString() + "/" + route).then(action);
}

/* sets the content of the "result_area" element with the results from the given route inserted in the given table by the tableSetter */
function setTable(route, table, tableSetter) {
    var resultDiv = document.getElementById("result_area");
    fetchData(route, function(response) {
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
                    tableSetter(jsonResponse, table);
                    resultDiv.appendChild(table);
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

/* creates a variable number of radio buttons into the form, with [name, label, route] properties */
function createFiltersRadioButtons(form, name, ...radioButtons) {
    radioButtons.forEach(button => {
        var input = document.createElement("input");
        var label = document.createElement("label");
        input.type = "radio";
        input.name = name;
        input.id = button[0];
        input.onclick = () => { if (input.checked) { setTable(button[2]); } };
        label.for = input.id;
        label.innerHTML = button[1];
        form.appendChild(input);
        form.appendChild(label);
        form.appendChild(document.createElement("br"));
    });
}

/* creates a variable number of checkBoxes into the form, with [name, value, label] properties */
function createFiltersCheckBoxes(form, ...checkBoxes) {
    checkBoxes.forEach(box => {
        var input = document.createElement("input");
        var label = document.createElement("label");
        input.type = "checkbox";
        input.name = box[0];
        input.value = box[1];
        input.id = box[0];
        label.for = input.id;
        label.innerHTML = box[2];
        form.appendChild(input);
        form.appendChild(label);
        form.appendChild(document.createElement("br"));
    });
}

/* creates a variable number of checkBoxes into the form, with [name, value, label] properties, that have the "hidden" class */
function createHiddenFiltersCheckBoxes(form, ...checkBoxes) {
    checkBoxes.forEach(box => {
        var input = document.createElement("input");
        input.type = "checkbox";
        input.name = box[0];
        input.value = box[1];
        input.checked = true;
        input.id = box[0] + "_hidden";
        input.classList.add("hidden");
        form.appendChild(input);
    });
}

/* change the checked boxes based on the value of the "cf_select" element */
function changeCheckedBoxes() {
    var list = document.getElementById("cf_select");
    fetchData("people", response => {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (!checkIfJsonIsError(jsonResponse)) {
                    for (const index in jsonResponse) {
                        if (list.value == jsonResponse[index]._personCf) {
                            document.getElementById("partner").checked = jsonResponse[index]._personSocio;
                            document.getElementById("cutter").checked = jsonResponse[index]._personOperatoreIntagliatrice;
                            document.getElementById("printer").checked = jsonResponse[index]._personOperatoreStampante;
                        }
                    }
                }
            });
        }
    });
}

/* disables the invisibles checkboxes if the visible ones are checked */
function disableCheckBoxes(form) {
    var checkBoxes = document.getElementsByTagName("input");
    for (const index in checkBoxes) {
        var box = checkBoxes.item(index);
        if (box.type == "checkbox" && box.classList.contains("hidden")) {
            var shownId = box.id.split("_")[0];
            var shownBox = document.getElementById(shownId);
            if (shownBox.checked) {
                box.disabled = true;
            }
        }
    }
}

/* sends to the said route the content of the given form, giving a visual notice of the result of the operation */
function sendFormData(formId, route) {
    var form = document.getElementById(formId);
    var fd = new FormData(form);
    var post = {
        method: "POST",
        body: fd
    };
    console.log(form);
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

function checkIfJsonIsError(json) {
    return json.hasOwnProperty("response");
}