import { DrawingContext, EventContext } from '../types';
import { ElementRef } from '@lynx-js/type-element-api';

const eventContext : EventContext<Node> = {
  addEventListener : (mount : Element, event : string, listener, capture : boolean) => {
    return __AddEvent(mount, 'catchEvent', event, { type : 'worklet', value : listener });
  },
  removeEventListener : (mount : Element, event : string, listener, capture : boolean) => {
      /* dmj: todo implement */
     return;
  },
  isEqual : (x, y) => {
    return __ElementIsEqual(x,y);
  },
  getTarget : (e) => {
    return e.target.elementRefptr;
  },
  parentNode : (node: Element) => {
    return __GetParent(node);
  }
};

const drawingContext : DrawingContext<Node> = {
  nextSibling : (x) => {
    return x.nextSibling.domRef;
  },
  createTextNode : (s: string) => {
    return __CreateRawText(s);
  },
  createElementNS : (ns : string, tag : string) => {
   return globalThis['miso']['context']['createElement'](tag);
  },
  createElement : (tag : string) => {
      var pageId = globalThis['native']['currentPageId'];
      switch (tag) {
          case 'view':
              return __CreateView(pageId);
              break;
          case 'scroll-view':
              return __CreateScrollView(pageId);
              break;
          case 'text':
              return __CreateText(pageId);
              break;
          case 'list':
              return __CreateList(pageId, undefined, null, null);
              break;
          case 'image':
              return __CreateImage(pageId);
              break;
          case 'frame':
              return __CreateFrame(pageId, null);
              break;
          default:
              return __CreateElement(tag, pageId);
              break;
      }
  },
  appendChild : (parent, child) => {
    return __AppendElement (parent, child);
  },
  replaceChild : (parent, n, o) => {
    return __ReplaceElements (parent, [n], [o]);
  },
  removeChild : (parent, child) => {
    return __RemoveElement (parent, child);
  },
  insertBefore : (parent, child, node) => {
    return __InsertElementBefore (parent, child, node);
  },
  swapDOMRefs: (a: Node, b: Node, p: Node): void => {
    return __SwapElement(a,b);
  },
  setAttribute : (node, key, value) => {
    if (key === 'id') return __SetID(node, value);
    return __SetAttribute(node,key,value);
  },
  removeAttribute : (node : ElementRef, key: string) => {
    return __SetAttribute(node, key, '');
  },
  setAttributeNS : (node, ns, key, value) => {
    return __SetAttribute(node,key,value);
  },
  setTextContent : (node, text) => {
    return __SetAttribute(node,'text',text);
  },
  setInlineStyle : (cCss, nCss, node) => {
    if (cCss != nCss)
      return __SetInlineStyles(node, nCss)
  },
  flush : (): void => {
    return __FlushElementTree();
  },
  getRoot : (): ElementRef => {
     return globalThis['page'];
  },
  getHead : (): ElementRef => {
    /* dmj: todo implement */
    return null;
  }
};

export {
  drawingContext, eventContext
}
