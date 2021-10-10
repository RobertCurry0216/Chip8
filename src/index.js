import './main.css';
import 'tuicss';
import { Elm } from './Main.elm';
import {loadRom, roms} from './roms/index';
import beepsfx from './assets/beep.mp3';

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: Object.keys(roms)
});

// sound effects
const beep = document.createElement("audio");
beep.setAttribute("src", beepsfx);
beep.crossOrigin = "anonymous";

document.body.appendChild(beep);

app.ports.playSound.subscribe(() => {
  return beep.play();
});

// load roms
app.ports.fetchRom.subscribe(rom => {
  loadRom(rom).then(
    d => app.ports.loadRom.send(d)
  );
});
