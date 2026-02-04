import { NodeId, getDOMRef, VComp, DrawingContext, EventContext, VTree } from 'haskell-miso';
import { ElementRef } from '@lynx-js/type-element-api';

const eventContext : EventContext<ElementRef> = {
  delegator : () => {
    /* dmj: implement */
  },
  addEventListener : (mount : ElementRef, event : string, listener, capture : boolean) => {
    /* use capture */
    return __AddEvent(mount, 'catchEvent', event, { type : 'worklet', value : listener });
  },
  isEqual : (x, y) => {
    return __ElementIsEqual(x,y);
  },
  getTarget : (e) => {
    /* BASE_STATIC_STRING_DECL(kElementRefptr, "elementRefptr"); */
    return (e.target as any).elementRefptr as ElementRef;
  },
  parentNode : (node: ElementRef) => {
    return __GetParent(node);
  }
};

const drawingContext : DrawingContext<ElementRef> = {
  addClass : (className : string, domRef : ElementRef) => {
      __AddClass(domRef, className);
  },
  removeClass : (className : string, domRef : ElementRef) => {
      /* dmj: PR a __RemoveClass PAPI call to lynx ? */
      const classes = __GetClasses(domRef);
      if (!(classes.includes(className))) {
          classes.push(className);
          __SetClasses(domRef, classes.join(' '));
      }
  },
  nextSibling : (x : VComp<NodeId>) => {
      return getDOMRef(x.nextSibling);
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
  swapDOMRefs: (a: ElementRef, b: ElementRef, p: ElementRef): void => {
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
  flush : () => {
    return __FlushElementTree();
  },
  getRoot : () => {
     return globalThis['page'];
  },
  getHead : () => {
    /* dmj: todo implement */
    return null;
  }
};

export {
  drawingContext, eventContext
}
