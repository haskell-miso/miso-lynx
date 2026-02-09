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
  var page = __CreatePage("0", 0);
  var pageId = __GetElementUniqueID(page);
  globalThis['native']['currentPageId'] = pageId;
  globalThis['page'] = page;

  /* sets page as root node in document */
  globalThis['document'] = {} as any;
  globalThis['document']['body'] = page as any;
  initMainThreadProcessing();
}

/* Method to initialize main thread event handling / processing */
function initMainThreadProcessing () {
  console.log ('inside initMainThreadProcessing');
  const context = lynx.getJSContext();

  /* initialize runtime state */
  const runtime : Runtime<ElementRef> = {
    components : {},
    nodes : {}
  };

  runtime.nodes[0] = globalThis['page'];

  /* Receive messages from BG */
  context.addEventListener("message", (messages : MessageEvent<Array<PATCH>>) => {
    /* process patch messages in order as received */
    for (const m of messages.data) {
       processMessage(m,runtime);
    }
    if (messages.data.length > 0) {
       native.drawingContext.flush();
    }
  });
}

/* main thread message processing */
function processMessage (m : PATCH, runtime) {
  let node = null;
  switch (m.type) {
    case "addEventListeners":
      addListeners (m.events);
      break;
    case "createElement":
      node = native.drawingContext.createElement (m.tag);
      __SetConfig (node, { nodeId : m.nodeId });
      runtime.nodes[m.nodeId] = node;
      break;
    case "createTextNode":
      runtime.nodes[m.nodeId] = native.drawingContext.createTextNode (m.text);
      break;
    case "createElementNS":
      node = native.drawingContext.createElementNS (m.namespace, m.tag);
      __SetConfig (node, { nodeId : m.nodeId });
      runtime.nodes[m.nodeId] = node;
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
      native.drawingContext.setAttribute (runtime.nodes[m.nodeId], m.key, m.value);
      break;
    case "setAttributeNS":
      drawingContext.setAttributeNS (runtime.nodes[m.nodeId], m.namespace, m.key, m.value);
      break;
    case "setTextContent":
      drawingContext.setTextContent (runtime.nodes[m.nodeId], m.text);
      break;
    case "appendChild":
      native.drawingContext.appendChild (runtime.nodes[m.parent], runtime.nodes[m.child]);
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
      native.drawingContext.setInlineStyle (m.current, m.new, runtime.nodes[m.nodeId]);
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
   console.log ('dropChildren');
   delete nodeMap[node.nodeId];
   for (const child of node.children) {
      dropChildren(nodeMap, child);
   }
}

/* Initialize global event delegation on main thread
   This should only be invoked once on application load.
*/
function addListeners (events : Array <EventCapture>) {
    console.log ('addListeners');

    const page = native.drawingContext.getRoot();
    /* delegate on page */
    for (const { name, capture } of events) {
      native.eventContext['addEventListener'] (page, name, listen, capture);
    }
}

/* function eventListener */
function listen (events : Array<Event> | Event) : void {
  console.log ('listen!', events);
  /* dmj: lynx events can be arrays */
  console.log ('dispatching!');
  console.log ('target', events[0].target, 'config', __GetConfig(events[0].target.elementRefptr));
  const context = lynx.getJSContext();
  const root = native.drawingContext.getRoot();
  console.log ('did i make it here?!');
  if (Array.isArray(events)) {
    for (const e of events) {
      const stack = buildStack(root, e.target.elementRefptr);
      stack.pop()
      const outgoingMessage : ProcessEvent = { event: e, stack, type : "processEvent" };
      return context.postMessage (outgoingMessage);
    }
  } else {
      const stack = buildStack(root, events.target.elementRefptr);
      const outgoingMessage : ProcessEvent = { event: events, stack, type : "processEvent" };
      return context.postMessage (outgoingMessage);
  }
}

/* walk physical DOM, mark the path */
function buildStack(element: ElementRef, target: ElementRef): Array<number> {
  var stack = [];
  console.log ('config', __GetConfig (target));
  while (!__ElementIsEqual(element, target)) {
    stack.unshift(__GetConfig (target)['nodeId']);
    /* dmj: ^ nodeId is what is accumulated */
    if (target && __GetParent(target)) {
      target = __GetParent(target);
    } else {
      return stack;
    }
  }
  return stack;
}
