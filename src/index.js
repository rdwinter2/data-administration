'use strict';

require("./styles.scss");

const {Elm} = require('./Main');
var flags = {
    counter: 6,
    screen: {
      width: window.innerWidth,
      height: window.innerHeight
    }
};
var app = Elm.Main.init(flags);

app.ports.toJs.subscribe(data => {
    console.log(data);
})
// Use ES2015 syntax and let Babel compile it for you
var testFn = (inp) => {
    let a = inp + 1;
    return a;
}