import './main.css';
import './pico.css';
import { Elm } from './Main.elm';
import {loadRom, roms} from './roms/index'

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: Object.keys(roms)
});

console.log(window);

app.ports.fetchRom.subscribe(rom => {
  loadRom(rom).then(
    d => app.ports.loadRom.send(d)
  );
});
