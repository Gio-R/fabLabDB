window.onload = function() {
    /* people */
    document.getElementById("insert_person").onclick = () => insertPerson();
    document.getElementById("show_people").onclick = () => showPeople();
    document.getElementById("modify_person").onclick = () => modifyPerson();
    /* cuts */
    document.getElementById("insert_cut").onclick = () => insertCut();
    document.getElementById("assign_cut").onclick = () => assignCut();
    document.getElementById("complete_cut").onclick = () => completeCut();
    document.getElementById("assign_processing").onclick = () => assignProcessing();
    document.getElementById("show_cuts").onclick = () => showCuts();
    document.getElementById("insert_processing").onclick = () => insertProcessing();
    document.getElementById("show_processings").onclick = () => showProcessings();
    /* prints */
    document.getElementById("insert_print").onclick = () => insertPrint();
    document.getElementById("assign_print").onclick = () => assignPrint();
    document.getElementById("complete_print").onclick = () => completePrint();
    document.getElementById("assign_filament").onclick = () => assignFilament();
    document.getElementById("assign_printer").onclick = () => assignPrinter();
    document.getElementById("show_prints").onclick = () => showPrints();
    /* materials */
    document.getElementById("insert_material").onclick = () => insertMaterial();
    document.getElementById("insert_class").onclick = () => insertClass();
    document.getElementById("show_materials").onclick = () => showMaterials();
    document.getElementById("show_classes").onclick = () => showMaterialsClasses();
    /* plastics */
    document.getElementById("insert_plastic").onclick = () => insertPlastic();
    document.getElementById("insert_filament").onclick = () => insertFilament();
    document.getElementById("show_plastics").onclick = () => showPlastics();
    document.getElementById("show_filaments").onclick = () => showFilaments();
    /* admins */
    document.getElementById("insert_printer").onclick = () => insertPrinter();
    document.getElementById("insert_type").onclick = () => insertType();
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
    var resultTable = createTable("result_table", "headers", ["cf", "Codice fiscale"], ["name", "Nome"], ["surname", "Cognome"], ["expense", "Spesa totale"]);
    document.getElementById("result_area").appendChild(resultTable);
    var setter = (jsonResponse, table) => {
        var body = table.getElementsByTagName("tbody")[0];
        showClearElem(body.id);
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
            body.appendChild(listElem);
        }
    };
    createFiltersRadioButtons(form, resultTable, setter, "people_type", ["all", "Tutti", "people"], ["partners", "Soci", "partners"], ["cutters", "Operatori intagliatrice", "cutterOperators"], ["printers", "Operatori stampante", "printerOperators"]);
    setTable("people", resultTable, setter);
}

/* show the appropriate forms to modify a person */
function modifyPerson() {
    clearPage();
    var form = document.getElementById("input_form");
    showClearElem(form.id);
    fetchData("people", response => {
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

/* creates the form and insert a new cut */
function insertCut() {
    insertWork("insert_cut");
}

/* assigns a cut to an operator*/
function assignCut() {
    assignAToB("assign_cut_operator", "cuts", "cutterOperators", "cut", "operator", 
                cut => cut._cutCodiceIntaglio, cut => (cut._cutCodiceIntaglio + " -- " + cut._cutDataRichiesta + " -- " + cut._cutCfRichiedente),
                operator => operator._personCf, operator => (operator._personNome + " " + operator._personCognome + " -- " + operator._personCf));
}

/* completes a cut*/
function completeCut() {
    completeWork("cuts", "cut", cut => cut._cutCodiceIntaglio, 
                    cut => (cut._cutCodiceIntaglio + " -- " + cut._cutDataRichiesta + " -- " + cut._cutCfRichiedente),
                    "modify_cut");
}

/* */
function assignProcessing() {
}

/* shows all the cuts in the database*/
function showCuts() {
    clearPage();
    var form = document.getElementById("filters_form");
    form.classList.remove("hidden");
    var resultTable = createTable("result_table_works", "headers", ["code", "Cod. intaglio"], ["request_day", "Data richiesta"], 
                            ["complete_day", "Data consegna"], ["client", "CF richiedente"]);
    document.getElementById("result_area").appendChild(resultTable);
    var setter = (jsonResponse, table) => {
        var body = table.getElementsByTagName("tbody")[0];
        showClearElem(body.id);
        for (const index in jsonResponse) {
            var listElem = document.createElement("tr");
            var cut = jsonResponse[index];
            listElem.classList.add("result_elem");
            var codeCell = document.createElement("td");
            codeCell.innerHTML = cut._cutCodiceIntaglio;
            var requestCell = document.createElement("td");
            requestCell.innerHTML = cut._cutDataRichiesta;
            var completeCell = document.createElement("td");
            completeCell.innerHTML = cut._cutDataConsegna;
            var clientCell = document.createElement("td");
            clientCell.innerHTML = cut._cutCfRichiedente;
            var descrDiv = document.createElement("div");
            descrDiv.classList.add("complete_description");
            var descrPar = document.createElement("p");
            descrPar.innerHTML = "Codice intaglio: " + cut._cutCodiceIntaglio 
                               + "</br> Data richiesta: " + cut._cutDataRichiesta
                               + "</br> Codice fiscale richiedente: " + cut._cutCfRichiedente
                               + "</br> Data consegna: " + (cut._cutDataConsegna == null ? "" : cut._cutDataConsegna)
                               + "</br> Costo totale: " + (cut._cutCostoTotale == null ? "" : cut._cutCostoTotale)
                               + "</br> Costo materiali: " + (cut._cutCostoMateriali == null ? "" : cut._cutCostoMateriali)
                               + "</br> Tempo esecuzione: " + (cut._cutTempo == null ? "" : cut._cutTempo)
                               + "</br> Codice fiscale incaricato: " + (cut._cutCfIncaricato == null ? "" : cut._cutCfIncaricato)
                               + "</br> Descrizione: " + (cut._cutDescrizione == null ? "" : cut._cutDescrizione);
            descrDiv.appendChild(descrPar);
            codeCell.appendChild(descrDiv);
            listElem.append(codeCell, requestCell, completeCell, clientCell);
            body.appendChild(listElem);
        }
    };
    createFiltersRadioButtons(form, resultTable, setter, "cut_type", ["all", "Tutte", "cuts"], ["complete", "Complete", "complete_cuts"], ["incomplete", "Incomplete", "incomplete_cuts"]);
    setTable("cuts", resultTable, setter);
}

/* creates the form and inserts a new processing */
function insertProcessing() {
    clearPage();
    var form = document.getElementById("input_form");
    fetchData("types", responseTypes => {
        if (responseTypes.ok) {
            responseTypes.json().then(jsonResponseTypes => {
                if (checkIfJsonIsError(jsonResponseTypes) && jsonResponseTypes["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponseTypes["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponseTypes)) {
                    fetchData("materials", responseMaterials => {
                        if (responseMaterials.ok) {
                            responseMaterials.json().then(jsonResponseMaterials => {
                                if (checkIfJsonIsError(jsonResponseMaterials) && jsonResponseMaterials["response"]["code"] != "200") {
                                    var error = document.createElement("P");
                                    error.innerHTML = jsonResponseMaterials["response"]["message"];
                                    error.classList.add("error");
                                    form.appendChild(error);
                                } else if (!checkIfJsonIsError(jsonResponseMaterials)) {
                                    showClearElem(form.id);
                                    var typesList = document.createElement("select");
                                    typesList.name = "type";
                                    typesList.id = "type_select";
                                    form.appendChild(typesList);
                                    form.appendChild(document.createElement("br"));
                                    for (const index in jsonResponseTypes) {
                                        var elem = document.createElement("option");
                                        elem.value = jsonResponseTypes[index]._typeCodiceTipo;
                                        elem.innerHTML = jsonResponseTypes[index]._typeCodiceTipo + " " + jsonResponseTypes[index]._typeNome;
                                        typesList.appendChild(elem);
                                    }
                                    var materialsTypes = document.createElement("select");
                                    materialsTypes.name = "material";
                                    materialsTypes.id = "material_select";
                                    form.appendChild(materialsTypes);
                                    form.appendChild(document.createElement("br"));
                                    for (const index in jsonResponseMaterials) {
                                        var elem = document.createElement("option");
                                        elem.value = jsonResponseMaterials[index]._materialCodiceMateriale;
                                        elem.innerHTML = jsonResponseMaterials[index]._materialNome + " " + jsonResponseMaterials[index]._materialSpessore;
                                        materialsTypes.appendChild(elem);
                                    }
                                    var maxPInput = createTextInput("max_potency", "Potenza massima");
                                    var minPInput = createTextInput("min_potency", "Potenze minima");
                                    var speedInput = createTextInput("speed", "Velocità");
                                    var descrInput = createTextInput("description", "Descrizione");
                                    var button = document.createElement("button");
                                    button.type = "button";
                                    button.innerHTML = "Inserisci";
                                    form.append(maxPInput, minPInput, speedInput, descrInput);
                                    form.appendChild(button);
                                    button.onclick = () => sendFormData("input_form", "insert_processing");
                                }
                            });
                        }
                    });
                }
            });
        }
    });
}

/* */
function showProcessings() {
    clearPage();
    var form = document.getElementById("filters_form");
    form.classList.remove("hidden");
    fetchData("materials", response => {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponse)) {
                    var list = document.createElement("select");
                    list.name = "material";
                    list.id = "material_select";
                    var elem = document.createElement("option");
                    elem.value = "all";
                    elem.innerHTML = "Tutti le lavorazioni";
                    list.appendChild(elem);
                    form.appendChild(list);
                    for (const index in jsonResponse) {
                        elem = document.createElement("option");
                        elem.value = jsonResponse[index]._materialCodiceMateriale
                        elem.innerHTML = jsonResponse[index]._materialCodiceClasse + " " + jsonResponse[index]._materialNome + " " + jsonResponse[index]._materialSpessore;
                        list.appendChild(elem);
                    }
                    var resultTable = createTable("result_table", "headers", ["code", "Codice lavorazione"], ["description", "Descrizione"]);
                    document.getElementById("result_area").appendChild(resultTable);
                    var setter = (jsonResponse, table) => {
                        var body = table.getElementsByTagName("tbody")[0];
                        showClearElem(body.id);
                        for (const index in jsonResponse) {
                            var listElem = document.createElement("tr");
                            var processing = jsonResponse[index];
                            listElem.classList.add("result_elem");
                            var codeCell = document.createElement("td");
                            codeCell.innerHTML = processing._processingCodiceLavorazione;
                            var descrCell = document.createElement("td");
                            descrCell.innerHTML = processing._processingDescrizione;
                            var descrDiv = document.createElement("div");
                            descrDiv.classList.add("complete_description");
                            var descrPar = document.createElement("p");
                            descrPar.innerHTML = "Codice lavorazione: " + processing._processingCodiceLavorazione
                                            + "</br> Tipo: " + processing._processingCodiceTipo
                                            + "</br> Materiale: " + processing._processingCodiceMateriale
                                            + "</br> Potenza massima: " + processing._processingPotenzaMassima
                                            + "</br> Potenza minima: " + processing._processingPotenzaMinima
                                            + "</br> Velocità: " + processing._processingVelocita
                                            + "</br> Descrizione: " + processing._processingDescrizione;
                            descrDiv.appendChild(descrPar);
                            codeCell.appendChild(descrDiv);
                            listElem.append(codeCell, descrCell);
                            body.appendChild(listElem);
                        }
                    };
                    var changer = () => {
                        if (list.value == "all") {
                            setTable("processings", resultTable, setter);
                        } else {
                            useChosenData("filters_form", "select_processings_by_material", data => {
                                setTableFromData(data, resultTable, setter);
                            });
                        }
                    };
                    changer();
                    list.onchange = () => changer();
                }
            });
        }
    });
}


/*
----------------------------------------------------------------------------------------
PRINTS FUNCTIONS
----------------------------------------------------------------------------------------
*/

/* creates the form and sends the data to insert a new print */
function insertPrint() {
    insertWork("insert_print");
}

/* assigns a print to an operator */
function assignPrint() {
    assignAToB("assign_print_operator", "prints", "printerOperators", "print", "operator", 
                print => print._printCodiceStampa, print => (print._printCodiceStampa + " -- " + print._printDataRichiesta + " -- " + print._printCfRichiedente),
                operator => operator._personCf, operator => (operator._personNome + " " + operator._personCognome + " -- " + operator._personCf));
}

/* completese a print */
function completePrint() {
    completeWork("prints", "print", print => print._printCodiceStampa, 
                    print => (print._printCodiceStampa + " -- " + print._printDataRichiesta + " -- " + print._printCfRichiedente),
                    "modify_print");
}

/* assigns a filament to a print */
function assignFilament() {
    assignAToB("assign_print_filament", "prints", "filaments", "print", "filament", print => print._printCodiceStampa, 
                print => (print._printCodiceStampa + " -- " + print._printDataRichiesta + " -- " + print._printCfRichiedente),
                filament => filament._filamentCodiceFilamento, 
                filament => (filament._filamentMarca + " " + filament._filamentColore));
}

/* assigns a printer to a print */
function assignPrinter() {
    assignAToB("assign_print_printer", "printers", "prints", "printer", "print", printer => printer._printerCodiceStampante,
                printer => (printer._printerMarca + " " + printer._printerModello), print => print._printCodiceStampa, 
                print => (print._printCodiceStampa + " -- " + print._printDataRichiesta + " -- " + print._printCfRichiedente));
}

/* shows the prints */
function showPrints() {
    clearPage();
    var form = document.getElementById("filters_form");
    form.classList.remove("hidden");
    var resultTable = createTable("result_table_works", "headers", ["code", "Cod. stampa"], ["request_day", "Data richiesta"], 
                            ["complete_day", "Data consegna"], ["client", "CF richiedente"]);
    document.getElementById("result_area").appendChild(resultTable);
    var setter = (jsonResponse, table) => {
        var body = table.getElementsByTagName("tbody")[0];
        showClearElem(body.id);
        for (const index in jsonResponse) {
            var listElem = document.createElement("tr");
            var print = jsonResponse[index];
            listElem.classList.add("result_elem");
            var codeCell = document.createElement("td");
            codeCell.innerHTML = print._printCodiceStampa;
            var requestCell = document.createElement("td");
            requestCell.innerHTML = print._printDataRichiesta;
            var completeCell = document.createElement("td");
            completeCell.innerHTML = print._printDataConsegna;
            var clientCell = document.createElement("td");
            clientCell.innerHTML = print._printCfRichiedente;
            var descrDiv = document.createElement("div");
            descrDiv.classList.add("complete_description");
            var descrPar = document.createElement("p");
            descrPar.innerHTML = "Codice stampa: " + print._printCodiceStampa 
                               + "</br> Data richiesta: " + print._printDataRichiesta
                               + "</br> Codice fiscale richiedente: " + print._printCfRichiedente
                               + "</br> Data consegna: " + (print._printDataConsegna == null ? "" : print._printDataConsegna)
                               + "</br> Costo totale: " + (print._printCostoTotale == null ? "" : print._printCostoTotale)
                               + "</br> Costo materiali: " + (print._printCostoMateriali == null ? "" : print._printCostoMateriali)
                               + "</br> Tempo esecuzione: " + (print._printTempo == null ? "" : print._printTempo)
                               + "</br> Codice fiscale incaricato: " + (print._printCfIncaricato == null ? "" : print._printCfIncaricato)
                               + "</br> Codice stampante: " + (print._printCodiceStampante == null ? "" : print._printCodiceStampante)
                               + "</br> Descrizione: " + (print._printDescrizione == null ? "" : print._printDescrizione);
            descrDiv.appendChild(descrPar);
            codeCell.appendChild(descrDiv);
            listElem.append(codeCell, requestCell, completeCell, clientCell);
            body.appendChild(listElem);
        }
    };
    createFiltersRadioButtons(form, resultTable, setter, "print_type", ["all", "Tutte", "prints"], ["complete", "Complete", "complete_prints"], ["incomplete", "Incomplete", "incomplete_prints"]);
    setTable("prints", resultTable, setter);
}

/*
----------------------------------------------------------------------------------------
MATERIALS FUNCTIONS
----------------------------------------------------------------------------------------
*/

/* creates the form and sends the data to insert a new material into the database */
function insertMaterial() {
    clearPage();
    var form = document.getElementById("input_form");
    showClearElem(form.id);
    fetchData("materials_classes", response => {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponse)) {
                    var list = document.createElement("select");
                    list.name = "class";
                    list.id = "materials_classes_select";
                    form.appendChild(list);
                    for (const index in jsonResponse) {
                        var elem = document.createElement("option");
                        elem.value = jsonResponse[index]._materialsclassCodiceClasse;
                        elem.innerHTML = jsonResponse[index]._materialsclassCodiceClasse + " -- " + jsonResponse[index]._materialsclassNome;
                        list.appendChild(elem);
                    }
                    var materialCode = createTextInput("code", "Codice materiale (2 caratteri)");
                    var nameInput = createTextInput("name", "Nome");
                    var widthInput = createTextInput("width", "Spessore");
                    var descrInput = createTextInput("description", "Descrizione");
                    var okButton = document.createElement("button");
                    okButton.type = "button";
                    okButton.innerHTML = "Inserisci";
                    okButton.onclick = () => sendFormData("input_form", "insert_material");
                    form.append(materialCode, nameInput, widthInput, descrInput, okButton);
                }
            });
        }
    });
}

/* creates the form and sends the data to insert a new materials class into the database */
function insertClass() {
    clearPage();
    var form = document.getElementById("input_form");
    showClearElem(form.id);
    var codeInput = createTextInput("code", "Codice classe (2 caratteri)");
    var nameInput = createTextInput("name", "Nome");
    var button = document.createElement("button");
    button.type = "button";
    button.innerHTML = "Inserisci";
    form.appendChild(codeInput);
    form.appendChild(nameInput);
    form.appendChild(button);
    button.onclick = () => sendFormData("input_form", "insert_class");
}

/* shows the available materials */
function showMaterials() {
    clearPage();
    var form = document.getElementById("filters_form");
    form.classList.remove("hidden");
    fetchData("materials_classes", response => {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponse)) {
                    var list = document.createElement("select");
                    list.name = "class_code";
                    list.id = "class_select";
                    var elem = document.createElement("option");
                    elem.value = "all";
                    elem.innerHTML = "Tutti i materiali";
                    list.appendChild(elem);
                    form.appendChild(list);
                    for (const index in jsonResponse) {
                        elem = document.createElement("option");
                        elem.value = jsonResponse[index]._materialsclassCodiceClasse
                        elem.innerHTML = jsonResponse[index]._materialsclassCodiceClasse + " -- " + jsonResponse[index]._materialsclassNome;
                        list.appendChild(elem);
                    }
                    var resultTable = createTable("result_table", "headers", ["code", "Codice"], ["class", "Classe"], ["name", "Nome"], ["width", "Spessore"], ["description", "Descrizione"]);
                    var setter = (jsonResponse, table) => {
                        var body = table.getElementsByTagName("tbody")[0];
                        showClearElem(body.id);
                        for (const index in jsonResponse) {
                            var listElem = document.createElement("tr");
                            var material = jsonResponse[index];
                            listElem.classList.add("result_elem");
                            var codeCell = document.createElement("td");
                            codeCell.innerHTML = material._materialCodiceMateriale;
                            var classCell = document.createElement("td");
                            classCell.innerHTML = material._materialCodiceClasse;
                            var nameCell = document.createElement("td");
                            nameCell.innerHTML = material._materialNome;
                            var widthCell = document.createElement("td");
                            widthCell.innerHTML = material._materialSpessore;
                            var descrCell = document.createElement("td");
                            descrCell.innerHTML = material._materialDescrizione;
                            listElem.append(codeCell, classCell, nameCell, widthCell, descrCell);
                            body.appendChild(listElem);
                        }
                    };
                    var changer = () => {
                        if (list.value == "all") {
                            setTable("materials", resultTable, setter);
                        } else {
                            useChosenData("filters_form", "select_materials", data => {
                                setTableFromData(data, resultTable, setter);
                            });
                        }
                    };
                    changer();
                    list.onchange = () => changer();
                }
            });
        }
    });
}

/* shows the available materials classes */
function showMaterialsClasses() {
    clearPage();
    document.getElementById("filters_form").classList.add("hidden");
    var resultTable = createTable("result_table", "headers", ["code", "Codice"], ["name", "Nome"]);
    setTable("materials_classes", resultTable, (jsonResponse, table) => {
        for (const index in jsonResponse) {
            var listElem = document.createElement("tr");
            var mClass = jsonResponse[index];
            listElem.classList.add("result_elem");
            var codeCell = document.createElement("td");
            codeCell.innerHTML = mClass._materialsclassCodiceClasse;
            var nameCell = document.createElement("td");
            nameCell.innerHTML = mClass._materialsclassNome;
            listElem.append(codeCell, nameCell);
            table.appendChild(listElem);
        }
    });
}

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
    clearPage();
    var form = document.getElementById("input_form");
    showClearElem(form.id);
    fetchData("plastics", response => {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponse)) {
                    var list = document.createElement("select");
                    list.name = "plastic";
                    list.id = "plastic_select";
                    form.appendChild(list);
                    for (const index in jsonResponse) {
                        var elem = document.createElement("option");
                        elem.value = jsonResponse[index]._plasticCodicePlastica;
                        elem.innerHTML = jsonResponse[index]._plasticCodicePlastica + " -- " + jsonResponse[index]._plasticNome;
                        list.appendChild(elem);
                    }
                    var filamentCode = createTextInput("code", "Codice filamento (4 caratteri)");
                    var brandInput = createTextInput("brand", "Marca");
                    var colorInput = createTextInput("color", "Colore");
                    var okButton = document.createElement("button");
                    okButton.type = "button";
                    okButton.innerHTML = "Inserisci";
                    okButton.onclick = () => sendFormData("input_form", "insert_filament");
                    form.append(filamentCode, brandInput, colorInput, okButton);
                }
            });
        }
    });
}

/* shows the filaments, giving the possibility to select the plastic */
function showFilaments() {
    clearPage();
    var form = document.getElementById("filters_form");
    form.classList.remove("hidden");
    fetchData("plastics", response => {
        if (response.ok) {
            response.json().then(jsonResponse => {
                if (checkIfJsonIsError(jsonResponse) && jsonResponse["response"]["code"] != "200") {
                    var error = document.createElement("P");
                    error.innerHTML = jsonResponse["response"]["message"];
                    error.classList.add("error");
                    form.appendChild(error);
                } else if (!checkIfJsonIsError(jsonResponse)) {
                    var list = document.createElement("select");
                    list.name = "plastic_code";
                    list.id = "plastic_select";
                    var elem = document.createElement("option");
                    elem.value = "all";
                    elem.innerHTML = "Tutte le plastiche";
                    list.appendChild(elem);
                    form.appendChild(list);
                    for (const index in jsonResponse) {
                        elem = document.createElement("option");
                        elem.value = jsonResponse[index]._plasticCodicePlastica;
                        elem.innerHTML = jsonResponse[index]._plasticCodicePlastica + " -- " + jsonResponse[index]._plasticNome;
                        list.appendChild(elem);
                    }
                    var resultTable = createTable("result_table", "headers", ["code", "Codice"], ["plastic", "Plastica"], ["brand", "Marca"], ["color", "Colore"]);
                    var setter = (jsonResponse, table) => {
                        var body = table.getElementsByTagName("tbody")[0];
                        showClearElem(body.id);
                        for (const index in jsonResponse) {
                            var listElem = document.createElement("tr");
                            var filament = jsonResponse[index];
                            listElem.classList.add("result_elem");
                            var codeCell = document.createElement("td");
                            codeCell.innerHTML = filament._filamentCodiceFilamento;
                            var plasticCell = document.createElement("td");
                            plasticCell.innerHTML = filament._filamentCodicePlastica;
                            var brandCell = document.createElement("td");
                            brandCell.innerHTML = filament._filamentMarca;
                            var colorCell = document.createElement("td");
                            colorCell.innerHTML = filament._filamentColore;
                            listElem.append(codeCell, plasticCell, brandCell, colorCell);
                            body.appendChild(listElem);
                        }
                    };
                    var changer = () => {
                        if (list.value == "all") {
                            setTable("filaments", resultTable, setter);
                        } else {
                            useChosenData("filters_form", "select_filaments", data => {
                                setTableFromData(data, resultTable, setter);
                            });
                        }
                    };
                    changer();
                    list.onchange = () => changer();
                }
            });
        }
    });
}

/* shows the plastics in the database */
function showPlastics() {
    clearPage();
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

/* creates the form and sends the data to insert a new type of processing in the database */
function insertType() {
    clearPage();
    var form = document.getElementById("input_form");
    showClearElem(form.id);
    var codeInput = createTextInput("code", "Codice tipo di lavorazione (3 caratteri)");
    var nameInput = createTextInput("name", "Nome tipo di lavorazione");
    var descrInput = createTextInput("description", "Descrizione");
    var button = document.createElement("button");
    button.type = "button";
    button.innerHTML = "Inserisci";
    form.appendChild(codeInput);
    form.appendChild(nameInput);
    form.appendChild(descrInput);
    form.appendChild(button);
    button.onclick = () => sendFormData("input_form", "insert_type");
}

/* creates the form and insert a new printer in the database */
function insertPrinter() {
    clearPage();
    var form = document.getElementById("input_form");
    showClearElem(form.id);
    var codeInput = createTextInput("code", "Codice stampante (3 caratteri)");
    var brandInput = createTextInput("brand", "Marca");
    var modelInput = createTextInput("model", "Modello");
    var descrInput = createTextInput("description", "Descrizione");
    var button = document.createElement("button");
    button.type = "button";
    button.innerHTML = "Inserisci";
    form.appendChild(codeInput);
    form.appendChild(brandInput);
    form.appendChild(modelInput);
    form.appendChild(descrInput);
    form.appendChild(button);
    button.onclick = () => sendFormData("input_form", "insert_printer");
}

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
}

/* makes visible the element with elemId, as if it was created anew */
function showClearElem(elemId) {
    var elem = document.getElementById(elemId);
    if (elem != null) {
        while(elem.firstChild) {
            elem.removeChild(elem.firstChild);
        }
        elem.classList.remove("hidden");
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