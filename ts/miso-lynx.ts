import {
  TextDecoder,
  TextEncoder,
} from "text-encoding";

import JSBI from "jsbi";

/* Polyfills global rAF w/ lynx */
globalThis['requestAnimationFrame'] = lynx['requestAnimationFrame'];
globalThis['cancelAnimationFrame'] = lynx['cancelAnimationFrame'];

/* Polyfills for native, these come first */
globalThis['TextEncoder'] = TextEncoder;
globalThis['BigInt'] = JSBI.BigInt;
globalThis['JSBI'] = JSBI;

import { bts } from './miso/bts';
import { mts } from './miso/mts';
import { drawingContext as btsDC, eventContext as btsEC, componentContext as btsCX } from './miso/bts/context';
import { drawingContext as mtsDC, eventContext as mtsEC, componentContext as mtsCX } from './miso/mts/context';

// both use nodeId
globalThis['nodeId'] = 1;
globalThis['initialDraw'] = true;

if (__BACKGROUND__) {
  globalThis['native'] = { drawingContext: btsDC, eventContext: btsEC, componentContext: btsCX };
  globalThis['patches'] = [];
  bts();
} else {
  globalThis['native'] = { drawingContext: mtsDC, eventContext: mtsEC, componentContext: mtsCX };
  globalThis['renderPage'] = () => mts();
  globalThis['runWorklet'] = (worklet, params) => worklet(params);
}

/* etc. */
globalThis['processData'] = () => {};

