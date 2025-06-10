/* core type for virtual DOM */
type CSS = Record<string, string>;
type DOMRef = HTMLElement | SVGElement | MathMLElement;

/*
  dmj: Context used for dependency injection of native or browser environment.
*/
type Context = {
  addEventListener : (mount : Node, event : string, listener : any, capture : boolean) => void;
  createTextNode : (s: string) => Text;
  createElementNS : (ns : string, tag : string) => Element;
  appendChild : (parent, child) => void;
  replaceChild : (parent, n, o) => void;
  removeChild : (parent, child) => void;
  createElement : (name : string) => Element;
  insertBefore : (parent, child, node) => void;
  swapDOMRefs: (a: Node, b: Node, p: Node) => void;
  querySelectorAll: (sel: string) => NodeListOf<Element>;
  setAttribute : (node, key, value) => void;
  removeAttribute : (node, key) => void;
  setAttributeNS : (node, ns, key, value) => void;
  setTextContent : (node, text) => void;
  getTextContent : (node) => string;
  isEqual : (n1, n2) => boolean;
  getTarget : (e: Event) => EventTarget;
  setComponentId : (componentId: string) => void;
  children : (e: Node) => NodeListOf<ChildNode>;
  getStyle : (e, string) => string;
  setStyle : (cCss: CSS, nCss: CSS, node : DOMRef) => void;
  getAttribute : (e: Element, string) => string;
  getTag : (e) => string;
  firstChild : (e) => Element;
  lastChild : (e) => Element;
  parentNode : (e) => Element;
  requestAnimationFrame : (callback: ((timestamp: number) => void)) => void;
  flush : () => void;
  getRoot : () => Element;
};

export {
  Context,
};
