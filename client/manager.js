window.onload = () => {
    document.getElementById("insert_printer").onclick = () => insertPrinter();
    document.getElementById("insert_type").onclick = () => insertType();
    document.getElementById("insert_admin").onclick = () => insertAdmin();
}

/*
----------------------------------------------------------------------------------------
ADMIN FUNCTIONS
----------------------------------------------------------------------------------------
*/

/* creates the form and sends the data to insert a new type of processing in the database */
function insertType() {
    /* c. */clearPage();
    var form = document.getElementById("input_form");
    /* c. */showClearElem(form.id);
    var codeInput = /* c. */createTextInput("code", "Codice tipo di lavorazione (3 caratteri)");
    var nameInput = /* c. */createTextInput("name", "Nome tipo di lavorazione");
    var descrInput = /* c. */createTextInput("description", "Descrizione");
    var button = document.createElement("button");
    button.type = "button";
    button.innerHTML = "Inserisci";
    form.appendChild(codeInput);
    form.appendChild(nameInput);
    form.appendChild(descrInput);
    form.appendChild(button);
    button.onclick = () => /* c. */sendFormData("input_form", "insert_type");
}

/* creates the form and insert a new printer in the database */
function insertPrinter() {
    /* c. */clearPage();
    var form = document.getElementById("input_form");
    /* c. */showClearElem(form.id);
    var codeInput = /* c. */createTextInput("code", "Codice stampante (3 caratteri)");
    var brandInput = /* c. */createTextInput("brand", "Marca");
    var modelInput = /* c. */createTextInput("model", "Modello");
    var descrInput = /* c. */createTextInput("description", "Descrizione");
    var button = document.createElement("button");
    button.type = "button";
    button.innerHTML = "Inserisci";
    form.appendChild(codeInput);
    form.appendChild(brandInput);
    form.appendChild(modelInput);
    form.appendChild(descrInput);
    form.appendChild(button);
    button.onclick = () => /* c. */sendFormData("input_form", "insert_printer");
}

/* creates the form and insert a new admin in the database */
function insertAdmin() {
    /* c. */clearPage();
    var form = document.getElementById("input_form");
    /* c. */showClearElem(form.id);
    var usernameInput = /* c. */createTextInput("username", "Username");
    var passwordInput = document.createElement("input");
    passwordInput.type = "password";
    passwordInput.name = "password";
    passwordInput.placeholder = "Password";
    var button = document.createElement("button");
    button.type = "button";
    button.innerHTML = "Inserisci";
    form.appendChild(usernameInput);
    form.appendChild(passwordInput);
    form.appendChild(button);
    button.onclick = () => /* c. */sendFormData("input_form", "insert_user");
}