var ipadres = "127.0.0.1";
var port = 9161;
var addOnUrl = "";
var ws;
var timeoutId;

function openWebSocket() {
    if ( "WebSocket" in window) {
		console.log("Attempting to Connect");
        ws = new WebSocket("ws://"+ipadres+":"+port+"/"+addOnUrl);
    
        ws.onopen =     function() {
                            onOpen();
                        };
        ws.onmessage =  function(event) {
                            onMessage(event.data);
                        };
        ws.onerror =    function(error) {
                            onError(error);
                        };
        ws.onclose =    function() {
                            onClose();
                        };
    } else {
        alert("Websocket is not supported in this browser!");
    }
}

function onOpen() {
    //Connection may be opened but interrupted during page load
    if(ws.readyState == 1) {
        setupConnection();
		timeoutId = setTimeout(destroyOnClosingState, 100);
    }
}

function onMessage(message) {
    console.log("incoming: " + message);
    fromJsonObject(message);
}

function onError(error) {
	console.log("error");
}

function onClose() {
	setTimeout(openWebSocket, 100);	
}

function sendMessage(message) {
    ws.send(message);
    console.log("outgoing: "+ message);
}

function destroyOnClosingState() {
	if(ws.readyState == 2) {
		ws.close();
	} else {
		timeoutId = setTimeout(destroyOnClosingState, 100);
	}
}

function closeWebSocket() {
	console.log(ws);
	clearTimeout(timeoutId);
	ws.close();
}