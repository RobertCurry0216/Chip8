import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
// import fs from 'fs';
//  Buffer = require('buffer').Buffer;

// const byteList = []
// fs.open('src/roms/horseWorldOnline.ch8', 'r', function(err, fd) {
//   if (err)
//     throw err;
//   var buffer = Buffer.alloc(1);
//   while (true)
//   {   
//     var num = fs.readSync(fd, buffer, 0, 1, null);
//     if (num === 0)
//       break;
//     byteList.push(buffer[0]);
//   }
// });


Elm.Main.init({
  node: document.getElementById('root')
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
