import { mts } from './miso/mts';
import { bts } from './miso/bts';

console.log('hey everyone');

/* init bts () */
bts();

/* init mts() */
globalThis['renderPage'] = function () {
  mts();
}

/* etc. */
globalThis['processData'] = () => {};

/* runWorklet boilerplate */
globalThis['runWorklet'] = (worklet, params) => {
  return worklet(params);
}
