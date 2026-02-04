import
  { drawingContext,
    eventContext
  } from './miso/context/lynx';

import
  { EventContext,
    PATCH,
    ComponentId,
    ProcessEvent,
  } from "haskell-miso";

import
  { ElementRef
  } from '@lynx-js/type-element-api';

import {
 TextDecoder,
 TextEncoder,
} from "text-encoding";

import JSBI
  from "jsbi";

/* polyfills for native, these come first */
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

  /* sets page as root node in document */
  globalThis['document'] = {} as any;
  globalThis['document']['body'] = page as any;
  initMainThreadProcessing();
}

/*
    [MAIN THREAD]

    - Setup main thread runtime state to hold components, along with their models, and root nodeId.

    - Setup event listeners for background thread communication and main thread runtime state.

      - Add handler for initial event delegation (received from bg thread).
        - On events, build stack for background thread processing, post via WebWorker to bg thread.

      - Add handler for patch application and component creation.
        - Place nodeId on every DOM node for event delegation (easier to build event stack).

      - Add handler for receiving updated model from bg thread
        - This is meant to be used by background thread for read-only purposes

      - Main thread events require modification to `miso` itself.
        - We'd need to add top-level handlers that take `mainThread :: Object -> IO ()`
          - where `Object` is a JSON'ified version of `model`.


    [BACKGROUND THREAD]

    - Setup event listener for main thread communication (requires abstracting out delegator into context object).

    - Add handler for receiving events from main thread (meant for processing on bg thread)
      - Receives stack then dispatches through VDOM, causes diff which creates patches sent to main thread
      - Send patches from post-diff `flush`() w/ `componentIds` to main thread

    - Once `miso` is on `npm` then consume `miso` typescript from `miso-lynx` (to avoid type duplication).

*/

/* Method to initialize main thread event handling / processing */
function initMainThreadProcessing () {
  'main thread';
  const context = lynx.getJSContext();

  /* initialize runtime state */
  const runtime : Runtime = {
    components : {},
    nodes : {}
  };

  /* Receive messages from BG */
  context.addEventListener("message", (payload : MessageEvent) => {
      const messages : Array<PATCH> = JSON.parse(payload.toString()); /* .toString()? */

    /* process patch messages in order as received */
    messages.forEach ((m) => processMessage(m,runtime));
  });
}

/* main thread message processing */
function processMessage (m, runtime) {
  'main thread';
  switch (m.type) {
    case "addEventListeners":
      addListeners (m.events);
      break;
    case "createElement":
      runtime.nodes[m.nodeId] = drawingContext.createElement (m.tag);
      runtime.nodes[m.nodeId]['nodeId'] = m.nodeId;
      break;
    case "createTextNode":
      runtime.nodes[m.nodeId] = drawingContext.createTextNode (m.text);
      runtime.nodes[m.nodeId]['nodeId'] = m.nodeId;
      break;
    case "createElementNS":
      runtime.nodes[m.nodeId] = drawingContext.createElementNS (m.namespace, m.tag);
      runtime.nodes[m.nodeId]['nodeId'] = m.nodeId;
      break;
    case "swapDOMRefs":
      drawingContext.swapDOMRefs
        (runtime.nodes[m.nodeA], runtime.nodes[m.nodeB], runtime.nodes[m.parent]);
      break;
    case "insertBefore":
      drawingContext.insertBefore
        (runtime.nodes[m.parent], runtime.nodes[m.child], runtime.nodes[m.node]);
      break;
    case "setAttribute":
      drawingContext.setAttribute (runtime.nodes[m.nodeId], m.key, m.value);
      break;
    case "setAttributeNS":
      drawingContext.setAttributeNS (runtime.nodes[m.nodeId], m.namespace, m.key, m.value);
      break;
    case "setTextContent":
      drawingContext.setTextContent (runtime.nodes[m.nodeId], m.text);
      break;
    case "appendChild":
      drawingContext.appendChild (runtime.nodes[m.parent], runtime.nodes[m.child]);
      break;
    case "removeChild":
      drawingContext.removeChild (runtime.nodes[m.parent], runtime.nodes[m.child]);
      dropChildren (runtime.nodes, runtime.nodes[m.child]);
      break;
    case "replaceChild":
      drawingContext.replaceChild (runtime.nodes[m.parent], runtime.nodes[m.new], runtime.nodes[m.current]);
      dropChildren (runtime.nodes, runtime.nodes[m.current]);
      break;
    case "removeAttribute":
      drawingContext.removeAttribute (runtime.nodes[m.nodeId], m.key);
      break;
    case "setTextContent":
      drawingContext.setTextContent (runtime.nodes[m.nodeId], m.text);
      break;
    case "setInlineStyle":
      drawingContext.setInlineStyle (m.current, m.new, runtime.nodes[m.nodeId]);
      break;
    case "addClass":
      drawingContext.addClass (m.key, runtime.nodes[m.nodeId]);
      break;
    case "removeClass":
      drawingContext.removeClass (m.key, runtime.nodes[m.nodeId]);
      break;
    case "flush":
      drawingContext.flush ();
      break;
    case "mount":
      /* dmj: TODO */
      break;
    case "unmount":
      /* dmj: TODO */
      break;
    case "modelHydration":
      /* dmj: TODO */
      break;
    default:
      console.error('Unknown message received', m);
      break;
  }
}

/* This purges all descendants from runtime.nodes map */
function dropChildren (nodeMap, node) {
   delete nodeMap[node.nodeId];
   for (const child of node.children) {
      dropChildren(nodeMap, child);
   }
}

/* Initialize global event delegation on main thread
   This should only be invoked once on application load.
 */
function addListeners (events : Record <string, boolean>) {
    'main thread';
    const page = drawingContext.getRoot();
    /* delegate on page */
    Object.entries(events).forEach (([event, capture]) => {
       eventContext.addEventListener (page, event, eventListener, capture);
    });
}

/* function eventListener */
function eventListener (events : Array<Event> | Event) : void {
   'main thread';
    /* dmj: lynx events can be arrays */
    if (Array.isArray(events))
      return events.forEach (dispatch);
    else
      return dispatch (events);
}

/* POST outgoing background thread event to BTS */
function dispatch (event: Event) : void {
  'main thread';
   const stack = buildStack(drawingContext.getRoot(), eventContext.getTarget(event));
   const context = lynx.getJSContext();
   const outgoingMessage : ProcessEvent = { event, stack, type : "processEvent" };
   return context.postMessage (JSON.stringify(outgoingMessage));
}

/* walk physical DOM, mark the path */
function buildStack(element: ElementRef, target: ElementRef): Array<number> {
  'main thread';
  var stack = [];
  while (!eventContext.isEqual(element, target)) {
    stack.unshift(target.nodeId);
    /* dmj: ^ nodeId is what is accumulated */
    if (target && eventContext.parentNode(target)) {
      target = eventContext.parentNode(target);
    } else {
      return stack;
    }
  }
  return stack;
}

/* dmj: TODO, add `Component` type that holds main thread event handlers, and models
*/

export type Runtime = {
  nodes : Record <number, ElementRef>,
  components : Record <ComponentId, Component>
}

export type Component = {
  model : Object,
  /* updated model sent from bg to main */
  componentModel: Object,
  /* component model */
  mainThreadEvents : Record <string, ((o:Object) => void)>,
  /* dmj: these will need to be sent from bg to main on app load*/
  rootId : number,
  /* the root node to diff from */ 
}

/* dmj: This is for something.
 */
globalThis['processData'] = function () {};

/* dmj: Invoke lynx behaviors, convenience function */
globalThis['invokeExec'] = function
  ( selector: string,
    method: string,
    params: Object,
    success: (result: any) => void,
    fail: (result: string) => void
  ) {

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
