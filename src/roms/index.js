import pong from './PONG';
import puzzle from './PUZZLE';
import missile from './MISSILE';

const roms = {
  ["Puzzle"]: puzzle,
  ["Missile"]: missile
}

const parseData = (data) => {
  return [...data].map(c => c.charCodeAt());
}

export const loadRom = (name) => {
  const path = roms[name];
  if (!path)
    return Promise.resolve([]);

  return fetch(path)
  .then(r => r.text())
  .then(d => parseData(d))
}
