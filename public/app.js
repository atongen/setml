var getGameId = function() {
    var pathname = window.location.pathname;
    var re = /\/games\/(\w+)/;
    return pathname.match(re)[1];
}

$(window).on('load', function(){
    ws = new WebSocket('ws://localhost:7777/games/' + getGameId() + '/ws');
    ws.onmessage = function(x) {
        console.log(x.data);
        $('#msg').append("<p>" + x.data + "</p>");
    };
    $("#target").click(function() {
        var d = new Date();
        ws.send(d.toString());
    });
});