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
                    setTableFromData(jsonResponse, table, tableSetter);
                }
            });
        }
    });
}

/* sets the content of the "result_area" element with the given data, inserted in the given table by the tableSetter */
function setTableFromData(data, table, tableSetter) {
    var resultDiv = document.getElementById("result_area");
    document.getElementById("result_area").classList.remove("hidden");
    tableSetter(data, table);
    resultDiv.appendChild(table);
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
    var groupHideableElements = document.getElementsByClassName("group_hideable");
    for (var i = 0; i < groupHideableElements.length; i++) {
        groupHideableElements[i].classList.add("hidden");
    }
}

/* makes visible the element with elemId, as if it was created anew */
function showClearElem(elemId) {
    var elem = document.getElementById(elemId);
    if (elem != null) {
        while(elem.firstChild) {
            elem.removeChild(elem.firstChild);
        }
        elem.classList.remove("hidden");
        while (elem.parentElement) {
            elem.parentElement.classList.remove("hidden");
            elem = elem.parentElement;
        }
    }
}

/* creates a variable number of radio buttons into the form, for the table with the given tableSetter, with [name, label, route] properties */
function createFiltersRadioButtons(form, table, tableSetter, name, ...radioButtons) {
    radioButtons.forEach(button => {
        var input = document.createElement("input");
        var label = document.createElement("label");
        input.type = "radio";
        input.name = name;
        input.id = button[0];
        input.onclick = () => { if (input.checked) { setTable(button[2], table, tableSetter); } };
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

/* sends to the said route the content of the given form, doing something with the results */
function useChosenData(formId, route, action) {
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
                if (checkIfJsonIsError(jsonResponse)) {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else {
                    action(jsonResponse);
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
    var head = document.createElement("thead");
    var body = document.createElement("tbody");
    body.id = "table_body";
    var header = document.createElement("tr");
    head.appendChild(header);
    header.id = headersId;
    resultTable.appendChild(head);
    headers.forEach(h => {
        var th = document.createElement("th");
        th.id = h[0];
        th.innerHTML = h[1];
        header.appendChild(th);
    });
    resultTable.appendChild(body);
    return resultTable;
}

function checkIfJsonIsError(json) {
    return json.hasOwnProperty("response");
}

/* function to create the form to assign an A element to a B element, using the given route */
function assignAToB(route, ARoute, BRoute, AName, BName, getACode, getAString, getBCode, getBString) {
    clearPage();
    var form = document.getElementById("input_form");
    fetchData(ARoute, responseA => {
        if (responseA.ok) {
            responseA.json().then(jsonResponseA => {
                if (checkIfJsonIsError(jsonResponseA) && jsonResponseA["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponseA["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponseA)) {
                    fetchData(BRoute, responseB => {
                        if (responseB.ok) {
                            responseB.json().then(jsonResponseB => {
                                if (checkIfJsonIsError(jsonResponseB) && jsonResponseB["response"]["code"] != "200") {
                                    var error = document.createElement("P");
                                    error.innerHTML = jsonResponseB["response"]["message"];
                                    error.classList.add("error");
                                    form.appendChild(error);
                                } else if (!checkIfJsonIsError(jsonResponseB)) {
                                    showClearElem(form.id);
                                    var AList = document.createElement("select");
                                    AList.name = AName;
                                    AList.id = AName + "_select";
                                    form.appendChild(AList);
                                    form.appendChild(document.createElement("br"));
                                    for (const index in jsonResponseA) {
                                        var elem = document.createElement("option");
                                        elem.value = getACode(jsonResponseA[index]);
                                        elem.innerHTML = getAString(jsonResponseA[index]);
                                        AList.appendChild(elem);
                                    }
                                    var BList = document.createElement("select");
                                    BList.name = BName;
                                    BList.id = BName + "_select";
                                    form.appendChild(BList);
                                    form.appendChild(document.createElement("br"));
                                    for (const index in jsonResponseB) {
                                        var elem = document.createElement("option");
                                        elem.value = getBCode(jsonResponseB[index]);
                                        elem.innerHTML = getBString(jsonResponseB[index]);
                                        BList.appendChild(elem);
                                    }
                                    var okButton = document.createElement("button");
                                    okButton.type = "button";
                                    okButton.innerHTML = "Assegna";
                                    okButton.onclick = () => sendFormData("input_form", route);
                                    form.appendChild(okButton);
                                }
                            });
                        }
                    });
                }
            });
        }
    });
}

/* function to insert a new work into the given route (either a print or a cut) */
function insertWork(route) {
    clearPage();
    var form = document.getElementById("input_form");
    fetchData("people", response => {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponse)) {
                    showClearElem(form.id);
                    var peopleList = document.createElement("select");
                    peopleList.name = "client";
                    peopleList.id = "client_select";
                    form.appendChild(peopleList);
                    form.appendChild(document.createElement("br"));
                    for (const index in jsonResponse) {
                        var elem = document.createElement("option");
                        elem.value = jsonResponse[index]._personCf;
                        elem.innerHTML = jsonResponse[index]._personNome + " " + jsonResponse[index]._personCognome + " -- " + jsonResponse[index]._personCf;
                        peopleList.appendChild(elem);
                    }
                    var dateInput = document.createElement("input");
                    dateInput.type = "date";
                    dateInput.name = "date";
                    dateInput.placeholder = "gg/mm/aaaa";
                    var descrInput = createTextInput("descr", "Descrizione");
                    var button = document.createElement("button");
                    button.type = "button";
                    button.innerHTML = "Inserisci";
                    form.appendChild(dateInput);
                    form.appendChild(descrInput);
                    form.appendChild(button);
                    button.onclick = () => sendFormData("input_form", route);
                }
            });
        }
    });
}

/* function to complete a work into the given route (either a print or a cut) */
function completeWork(dataRoute, workType, workCode, workToString, insertRoute) {
    clearPage();
    var form = document.getElementById("input_form");
    fetchData(dataRoute, response => {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponse)) {
                    showClearElem(form.id);
                    var list = document.createElement("select");
                    list.name = workType;
                    list.id = workType + "_select";
                    form.appendChild(list);
                    for (const index in jsonResponse) {
                        var elem = document.createElement("option");
                        elem.value = workCode(jsonResponse[index]);
                        elem.innerHTML = workToString(jsonResponse[index]);
                        list.appendChild(elem);
                    }
                    form.appendChild(document.createElement("br"));
                    var totalInput = createTextInput("total", "Costo totale (usare . per i decimali)");
                    var materialInput = createTextInput("materials", "Costo materiali (usare . per i decimali)");
                    var timeInput = createTextInput("time", "Tempo di completamento in ore (usare . per i decimali)");
                    var dateInput = document.createElement("input");
                    dateInput.type = "date";
                    dateInput.name = "date";
                    dateInput.placeholder = "gg/mm/aaaa";
                    var okButton = document.createElement("button");
                    okButton.type = "button";
                    okButton.innerHTML = "Modifica";
                    okButton.onclick = () => sendFormData("input_form", insertRoute);
                    form.append(totalInput, materialInput, timeInput, dateInput, document.createElement("br"), okButton);
                }
            });
        }
    });
}

function showNotYetImplemented(areaId) {
    clearPage();
    var p = document.createElement("p");
    p.classList.add("alert");
    p.innerHTML = "Questa funzione sarà implementata a breve!";
    showClearElem(areaId);
    document.getElementById(areaId).appendChild(p);
}