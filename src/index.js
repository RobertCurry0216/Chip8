import './main.css';
import './pico.css';
import { Elm } from './Main.elm';
import {loadRom} from './roms/index'

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.fetchRom.subscribe(rom => {
  loadRom(rom).then(
    d => app.ports.loadRom.send(d)
  );
});
