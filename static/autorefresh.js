console.log("establishing socket connection for autorefresh");
let socket = new WebSocket(`ws://${window.location.host}/ws/autorefresh`);

socket.onopen = function onopen() {
  console.log("opening websocket");
}

socket.onmessage = function onmessage(ev) {
  console.log("got a message", ev);
  location.reload();
}

socket.onclose = function onclose(ev) {
  if (ev.wasClean) {
    console.log("clean close", ev);
  } else {
    console.log("dirty close", ev);
  }
}

socket.onerror = function onerror(err) {
  console.log("got an error", err);
}
