/*

[BTS notes]
  - Setup event listener for main thread communication (requires abstracting out delegator into context object).
  - Add handler for receiving events from main thread (meant for processing on bg thread)
    - Receives stack then dispatches through VDOM, causes diff which creates patches sent to main thread
    - Send patches from post-diff `flush`() w/ `componentIds` to main thread
  - Once `miso` is on `npm` then consume `miso` typescript from `miso-lynx` (to avoid type duplication).

*/

import
  { EventContext,
    PATCH,
    ComponentId,
    ProcessEvent,
  } from "haskell-miso";

import
  { drawingContext
  , eventContext
  } from '../miso/bts/context';

import
  { ElementRef
  } from '@lynx-js/type-element-api';

import {
 TextDecoder,
 TextEncoder,
} from "text-encoding";

import JSBI
  from "jsbi";

export function bts () {
  'background only'
  /* Polyfills for native, these come first */
  globalThis['TextDecoder'] = TextDecoder;
  globalThis['TextEncoder'] = TextEncoder;
  globalThis['BigInt'] = JSBI.BigInt;
  globalThis['JSBI'] = JSBI;
  
  /* Polyfills global rAF w/ lynx */
  globalThis['requestAnimationFrame'] = lynx['requestAnimationFrame'];
  globalThis['cancelAnimationFrame'] = lynx['cancelAnimationFrame'];
  
  /* export native context globally */
  globalThis['native'] = {};
  globalThis['native']['drawingContext'] = drawingContext;
  globalThis['native']['eventContext'] = eventContext;
  
  /* Init BTS state */
  globalThis['patches'] = [];
  globalThis['nodeId'] = 1;

  /* Used for event handling on main thread */
  globalThis['runWorklet'] = (worklet, params) => {
    return worklet(params);
  }

  /* invoke exec */
  globalThis['invokeExec'] = function
    ( selector: string,
      method: string,
      params: Object,
      success: (result: any) => void,
      fail: (result: string) => void
    ) {
    'background only';

   /* Set arguments Object */
   const args = {
     params,
     method,
     success,
     fail
   };

   /* Invoke Exec */
   return lynx.createSelectorQuery()
       .select(selector)
       .invoke(args as any)
       .exec();
    }
}


