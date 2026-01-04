/* imports */
import { ElementRef } from '@lynx-js/type-element-api';

/* core type for virtual DOM */
type CSS = Record<string, string>;
type DOMRef = HTMLElement | SVGElement | MathMLElement;

/*
  dmj: Context used for dependency injection of native or browser environment.
*/
export type EventContext<T> = {
  addEventListener : (mount : T, event : string, listener : any, capture : boolean) => void;
  removeEventListener : (mount : T, event : string, listener : any, capture : boolean) => void;
  isEqual : (n1: T, n2: T) => boolean;
  getTarget : (e: Event) => T;
  parentNode : (node: T) => T;
}

export type DrawingContext<T> = {
  nextSibling : (node: VTree<T>) => T;
  createTextNode : (s: string) => T;
  createElementNS : (ns: string, tag : string) => T;
  createElement : (name: string) => T;
  appendChild : (parent: T, child: T) => void;
  replaceChild : (parent: T, n: T, o: T) => void;
  removeChild : (parent: T, child: T) => void;
  insertBefore : (parent: T, child: T, node: T) => void;
  swapDOMRefs: (a: T, b: T, p: T) => void;
  setAttribute : (node: T, key: string, value : any) => void;
  removeAttribute : (node: T, key :string) => void;
  setAttributeNS : (node: T, ns: string, key: string, value: any) => void;
  setTextContent : (node: T, text: string) => void;
  setInlineStyle : (cCss: CSS, nCss: CSS, node : T) => void;
  addClass : (c: string, domRef: T) => void;
  removeClass : (c: string, domRef: T) => void;
  flush : () => void;
  /** @since 1.9.0.0 */
  getHead : () => T;
  getRoot : () => T;
};


export {
   EventContext, DrawingContext
};
