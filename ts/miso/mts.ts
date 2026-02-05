/*

[MTS notes]
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

*/

import
  { EventContext,
    PATCH,
    ComponentId,
    Component,
    Runtime,
    ProcessEvent,
    EventCapture,
  } from "haskell-miso";

import
  { drawingContext
  , eventContext
  } from '../miso/mts/context';

import
  { ElementRef
  } from '@lynx-js/type-element-api';

export function mts () {
  console.log('inside mts!');
  var page = __CreatePage("0", 0);
  var pageId = __GetElementUniqueID(page);
  globalThis['native'] = {};
  globalThis['native']['currentPageId'] = pageId;
  globalThis['native']['drawingContext'] = drawingContext;
  globalThis['native']['eventContext'] = eventContext;
  globalThis['page'] = page;

  /* sets page as root node in document */
  globalThis['document'] = {} as any;
  globalThis['document']['body'] = page as any;
  initMainThreadProcessing();
}

/* Method to initialize main thread event handling / processing */
function initMainThreadProcessing () {
  const context = lynx.getJSContext();

  /* initialize runtime state */
  const runtime : Runtime<ElementRef> = {
    components : {},
    nodes : {}
  };

  /* Receive messages from BG */
  context.addEventListener("message", (messages : MessageEvent<Array<PATCH>>) => {
    /* process patch messages in order as received */
    messages.data.forEach ((m) => processMessage(m,runtime));
  });
}

/* main thread message processing */
function processMessage (m : PATCH, runtime) {
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
      runtime.components[m.componentId] = {
        model : m.model,
        mainThreadEvents : {},
        rootId : m.mountPoint
      } as Component;
      break;
    case "unmount":
      delete runtime.components[m.componentId];
      break;
    case "modelHydration":
      runtime.components[m.componentId].model = m.model;
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
function addListeners (events : Array <EventCapture>) {
    const page = drawingContext.getRoot();

    /* delegate on page */
    events.forEach ((event : EventCapture) => {
      const { name, capture } = event;
      eventContext.addEventListener (page, name, eventListener, capture);
    });
}

/* function eventListener */
function eventListener (events : Array<Event> | Event) : void {
    /* dmj: lynx events can be arrays */
    if (Array.isArray(events))
      return events.forEach (dispatch);
    else
      return dispatch (events);
}

/* POST outgoing background thread event to BTS */
function dispatch (event: Event) : void {
   const stack = buildStack(drawingContext.getRoot(), eventContext.getTarget(event));
   const context = lynx.getJSContext();
   const outgoingMessage : ProcessEvent = { event, stack, type : "processEvent" };
   return context.postMessage (outgoingMessage);
}

/* walk physical DOM, mark the path */
function buildStack(element: ElementRef, target: ElementRef): Array<number> {
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
