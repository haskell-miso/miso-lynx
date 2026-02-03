import { drawingContext, eventContext } from './miso/context/lynx';

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

  /* sets page as root node to document, like body */
  globalThis['document'] = {};
  globalThis['document']['body'] = page;
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
  const runtime : Runtime = {};
  runtime.components = {};
  runtime.nodes = {};

  /* Receive messages from BG */
  context.addEventListener("message", (payload) => {
    const messages : Array<PATCH> = JSON.parse(payload);

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
      break;
    case "createTextNode":
      runtime.nodes[m.nodeId] = drawingContext.createTextNode (m.text);
      break;
    case "createElementNS":
      runtime.nodes[m.nodeId] = drawingContext.createElement (m.namespace, m.tag);
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
    case "flush":
      drawingContext.flush ();
      break;
    default:
      console.error('Unknown message received', m);
      break;
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
       /* dmj: TODO, incorporate `capture` */
        __AddEvent(page, 'catchEvent', event, { type : 'worklet', value : eventListener });
    });
}

/* function eventListener */
function eventListener (events : Array<Event> | Event) : void {
   'main thread';
    /* dmj: lynx events can be arrays */
    if (Array.isArray(events))
      return events.forEach (dispatch);
    else
      return dispatch (e);
}

/* POST outgoing background thread event to BTS */
function dispatch (event: Event) : void {
  'main thread';
   const stack = buildStack(drawingContext.getRoot(), eventContext.getTarget(event));
   const context = lynx.getJSContext();
   const outgoingMessage : ProcessEvent = { event, stack };
   return context.postMessage (JSON.stringify(outgoingMessage));
}

/* walk physical DOM, mark the path */
function buildStack(element: T, target: T): Array<number> {
  'main thread';
  var stack = [];
  while (!drawingContext.isEqual(element, target)) {
    stack.unshift(target.nodeId);
    /* dmj: ^ nodeId is what is accumulated */
    if (target && drawingContext.parentNode(target)) {
      target = drawingContext.parentNode(target);
    } else {
      return stack;
    }
  }
  return stack;
}

/* dmj: TODO, add `Component` type that holds main thread event handlers, and models
*/

export type Runtime = {
  nodes : Record <NodeId, ElementRef>,
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

/* Process event
   Used to send BG events to Main thread
 */
export type ProcessEvent = {
  stack: Array<number>,
  event: Object, /* Event object, will be JSON'ified */
  type: "processEvent"
};

/* Message protocol for bidirectional synchronization between MTS / BTS */
export type PATCH
  = InsertBefore
  | SwapDOMRefs
  | CreateElement
  | CreateElementNS
  | CreateTextNode
  | SetAttribute
  | SetAttributeNS
  | AppendChild
  | RemoveChild
  | ReplaceChild
  | RemoveAttribute
  | SetTextContent
  | SetInlineStyle
  | MountComponent
  | UnmountComponent
  | ModelHydration
  | AddClass
  | RemoveClass
  | AddEventListeners
  | ProcessEvent
  | Flush;

export type ProcessEvent = {
  stack: Array<number>,
  event: string, /* JSON'ified event */
  type: "processEvent"
};


export type AddEventListeners = {
  events: Record<string, boolean>,
  type: "addEventListeners"
};

export type ModelHydration = {
  componentId: ComponentId,
  model: Object,
  type: "modelHydration"
};

export type MountComponent = {
  type: "mount",
  componentId: ComponentId,
  events: Array<EventCapture>,
  model: Object,
  mountPoint: number
};

export type UnmountComponent = {
  type: "unmount",
  componentId: ComponentId,
};

export type InsertBefore = {
  type: "insertBefore",
  parent: number,
  child: number,
  node: number
};

export type SwapDOMRefs = {
  nodeA: number,
  nodeB: number,
  parent: number,
  type: "swapDOMRefs"
};

export type CreateElement = {
  nodeId: number,
  tag: string,
  type: "createElement"
};

export type CreateElementNS = {
  nodeId: number,
  tag: string,
  namespace: string,
  type: "createElementNS"
};

export type CreateTextNode = {
  nodeId: number,
  text: string,
  type: "createTextNode"
};

export type SetAttribute = {
  key: string,
  value: any,
  nodeId: number,
  type: "setAttribute"
};

export type SetAttributeNS = {
  key: string,
  value: any,
  nodeId: number,
  type: "setAttributeNS",
  namespace: string,
};

export type AppendChild = {
  parent: number,
  child: number,
  type: "appendChild"
};

export type ReplaceChild = {
  componentId: ComponentId,
  current: number,
  new: number,
  parent: number,
  type: "replaceChild"
};

export type RemoveChild = {
  componentId: ComponentId,
  parent: number,
  child: number,
  type: "removeChild"
};

export type RemoveAttribute = {
  componentId: ComponentId,
  type: "removeAttribute",
  nodeId: number,
  key: string,
};

export type RemoveClass = {
  componentId: ComponentId,
  type: "removeClass",
  nodeId: number,
  key: string,
};

export type AddClass = {
  componentId: ComponentId,
  type: "addClass",
  nodeId: number,
  key: string,
};

export type SetTextContent = {
  type: "setTextContent",
  nodeId: number,
  text: string,
};

export type SetInlineStyle = {
  componentId: ComponentId,
  new : Record<string, any>,
  current : Record<string, any>,
  nodeId: number,
  type: "setInlineStyle"
};

export type Flush = {
  type: "flush"
};


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
   params: params,
   method: method,
   success: success,
   fail: fail
 };

 /* Invoke Exec */
 return lynx.createSelectorQuery()
     .select(selector)
     .invoke(args)
     .exec();
}
