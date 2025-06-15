import { context } from './miso/context/native';

import {
 TextDecoder,
 TextEncoder,
} from "text-encoding";

import JSBI from "jsbi";

/* polyfills for native, these come first */
globalThis['TextDecoder'] = TextDecoder;
globalThis['TextEncoder'] = TextEncoder;
globalThis['BigInt'] = JSBI.BigInt;
globalThis['JSBI'] = JSBI;

/* Polyfills global rAF w/ lynx */
globalThis['requestAnimationFrame'] = lynx['requestAnimationFrame'];
globalThis['cancelAnimationFrame'] = lynx['cancelAnimationFrame'];

/* export native context globally */
globalThis['native'] = context;

/* Used for event handling on main thread */
globalThis['runWorklet'] = (worklet, params) => {
  return worklet(params);
}

/* First function call, create global page, synonym to body for Haskell layer */
globalThis['renderPage'] = function() {
  var page = __CreatePage("0", 0);
  var pageId = __GetElementUniqueID(page);
  globalThis['native']['currentPageId'] = pageId;
  globalThis['page'] = page;

  /* sets page as root node to document, like body */
  globalThis['document'] = {};
  globalThis['document']['body'] = page;
}

/*
  dmj: this is for something, not sure what, can be exposed to Haskell layer if need be.
  For now let's just log out the lynx object.
 */
globalThis['processData'] = function () {

}

/* dmj: invoke lynx behaviors */
globalThis['invokeExec'] = (selector: string, params: Object) => {
  lynx.createSelectorQuery()
      .select(selector)
      .invoke(params)
      .exec();
}
