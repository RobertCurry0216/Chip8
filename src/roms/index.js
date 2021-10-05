import pong from './PONG';
import puzzle from './PUZZLE';
import missile from './MISSILE';
import tetris from './TETRIS';

const roms = {
  ["Pong"]: pong,
  ["Puzzle"]: puzzle,
  ["Missile"]: missile,
  ["Tetris"]: tetris
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
