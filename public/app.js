var getGameId = function() {
    var pathname = window.location.pathname;
    var re = /\/games\/(\w+)/;
    return pathname.match(re)[1];
}

var getWsUrl = function() {
    var loc = window.location, new_uri;
    if (loc.protocol === "https:") {
        new_uri = "wss:";
    } else {
        new_uri = "ws:";
    }
    new_uri += "//" + loc.host;
    new_uri += "/games/" + getGameId() + "/ws";
    return new_uri;
}

$(window).on('load', function(){
    ws = new WebSocket(getWsUrl());
    ws.onmessage = function(x) {
        console.log(x.data);
        $('#msg').append("<p>" + x.data + "</p>");
    };
    $("#target").click(function() {
        var d = new Date();
        ws.send(d.toString());
    });
});