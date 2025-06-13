import { Context } from '../types';
import { ElementRef } from '@lynx-js/type-element-api';

const context : Context = {
  addEventListener : (mount : Element, event : string, listener, capture : boolean) => {
    return __AddEvent(mount, 'catchEvent', event, { type : 'worklet', value : listener });
  },
  firstChild : (node: Element) => {
    return __FirstElement(node);
  },
  lastChild : (node: Element) => {
    return __LastElement(node);
  },
  parentNode : (node: Element) => {
    return __GetParent(node);
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
  querySelectorAll: (sel: string) => {
    return lynx.querySelectorAll(sel);
  },
  removeAttribute : (node : ElementRef, key: string) => {
    return __SetAttribute(node, key, '');
  },
  setAttribute : (node, key, value) => {
    if (key === 'id') return __SetID(node, value);
    return __SetAttribute(node,key,value);
  },
  getAttribute : (node, key) => {
    if (key === 'id') return __GetID(node);
    return __GetAttributeByName(node,key);
  },
  setStyle : (cCss, nCss, node) => {
    if (cCss != nCss)
      return __SetInlineStyles(node, nCss)
  },
  getStyle : (node, key) => {
    return __GetInlineStyles(node)[key]
  },
  getTag : (node) => {
    var s = __GetTag(node);
    if (s === "text") return "#text";     
    return s;
  },
  setAttributeNS : (node, ns, key, value) => {
    return __SetAttribute(node,key,value);
  },
  setTextContent : (node, text) => {
    return __SetAttribute(node,'text',text);
  },
  getTextContent : (node) => {
    return __GetAttribute('text', node);
  },
  isEqual : (x, y) => {
    return __ElementIsEqual(x,y);
  },
  children : (node: ElementRef) => {
    return __GetChildren(node);
  },
  getTarget : (e) => {
    return e.target.elementRefptr;
  },
  setComponentId : (id: string) => {
    return __SetDataset(globalThis['page'], { 'component-id' : id })
  },
  requestAnimationFrame : (callback: (number) => void): void => {
     lynx.requestAnimationFrame (callback);
  },
  flush : (): void => {
    return __FlushElementTree();
  },
  getRoot : (): void => {
     return globalThis['page'];
  }
}

export {
 context
};

