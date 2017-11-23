function boot(main) {
  if(main) {
    // alert("before full");
    // var elemenet = document.getElementById('target');
    // var app = Elm.Main.embed(elemenet);
    var app = main.fullscreen();
    setupPorts(app);
    // alert("finish setup ports");
  }else{
    alert("boot error!! main object is not available.");
  }
}

function setupPorts(app) {
  app.ports.sendToJs.subscribe(function(str) {// Elm 側からデータを受け取る
    alert(str);
    app.ports.receiveFromJs.send(str);// Elm 側に結果を返す
  });
}