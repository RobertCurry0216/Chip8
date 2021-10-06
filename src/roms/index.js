import pong from './PONG';
import puzzle from './PUZZLE';
import missile from './MISSILE';
import tetris from './TETRIS';
import welcome from './Picture.ch8'
import puzzle15 from './15PUZZLE';
import blinky from './BLINKY';
import blitz from './BLITZ';
import brix from './BRIX';
import connect4 from './CONNECT4';
import guess from './GUESS';
import hidden from './HIDDEN';
import invaders from './INVADERS';
import kaleid from './KALEID';
import maze from './MAZE';
import merlin from './MERLIN';
import pong2 from './PONG2';
import syzygy from './SYZYGY';
import tank from './TANK';
import tictac from './TICTAC';
import ufo from './UFO';
import vbrix from './VBRIX';
import vers from './VERS';
import wipeoff from './WIPEOFF';

export const roms = {
  welcome,
  pong,
  puzzle,
  missile,
  tetris,
  puzzle15,
  blinky,
  blitz,
  brix,
  connect4,
  guess,
  hidden,
  invaders,
  kaleid,
  maze,
  merlin,
  pong2,
  syzygy,
  tank,
  tictac,
  ufo,
  vbrix,
  vers,
  wipeoff
}

export const loadRom = (name) => {
  const path = roms[name];
  if (!path)
    return Promise.resolve([]);

  return fetch(path)
  .then(r => r.arrayBuffer())
  .then(d => [...new Uint8Array(d)])
  .catch(err => console.log(err))
}
