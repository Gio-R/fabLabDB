base_url = 'http://localhost:8080/'

function set_json_data(url, selectId, setter) {
    var html_code = '';
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() { 
        if (xhr.readyState == 4 && xhr.status == 200)
            setter(selectId, JSON.parse(xhr.responseText));
    }
    xhr.open('GET', url, true);
    xhr.send();
}

function set_select_list(selectId, options) {
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

function set_all_people(selectId) {
    set_json_data(base_url.concat('people'), selectId, set_select_list);
}

function initWindow() {
    set_all_people('select_richiedenti');
}

window.onload = initWindow();