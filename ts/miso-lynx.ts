import { mts } from './miso/mts';
import { bts } from './miso/bts';
import * as btsContext from './miso/bts/context';
import * as mtsContext from './miso/mts/context';

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

if (!(__MAIN_THREAD__)) {
  globalThis['native'] = {
    drawingContext : btsContext.drawingContext,
    eventContext : btsContext.eventContext
  }
  globalThis['patches'] = [];
  globalThis['nodeId'] = 1;
  bts();
} else {
  globalThis['native'] = {
    drawingContext : mtsContext.drawingContext,
    eventContext : mtsContext.eventContext
  };
}

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
