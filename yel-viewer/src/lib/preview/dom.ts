/**
 * DOM Interface Implementation for Yel Preview
 *
 * This module provides the yel:ui/dom@0.1.0 interface for running
 * jco-transpiled WASM components directly in the browser.
 */

// Map DSL tags to HTML elements
const TAG_MAP: Record<string, string> = {
  VStack: "div",
  HStack: "div",
  Button: "yel-button",
  Text: "yel-text",
  Card: "div",
  Input: "input",
  Image: "img",
};

// Node tracking
let nextNodeId = 1;
const nodeIdToElement = new Map<number, Node>();
const eventHandlers = new Map<string, number>(); // "nodeId:eventName" -> handlerId

// Dispatch callback - set by component host
let dispatchCallback: ((handlerId: number) => void) | null = null;

export function setDispatchCallback(callback: (handlerId: number) => void) {
  dispatchCallback = callback;
}

export function getHandlerId(
  nodeId: number,
  eventName: string
): number | undefined {
  return eventHandlers.get(`${nodeId}:${eventName}`);
}

export function reset(rootElement: HTMLElement) {
  nextNodeId = 1;
  nodeIdToElement.clear();
  eventHandlers.clear();
  // Store root element with ID 0
  nodeIdToElement.set(0, rootElement);
}

// DOM functions
export function createElement(tag: string): number {
  const id = nextNodeId++;
  console.log("[DOM] createElement:", tag, "-> id:", id);
  // Map DSL tags to HTML elements, add CSS classes for styling
  const htmlTag = TAG_MAP[tag] || tag.toLowerCase();
  const el = document.createElement(htmlTag);
  el.setAttribute("data-yel-tag", tag);
  el.setAttribute("data-node-id", String(id));

  // Apply layout classes based on tag
  if (tag === "VStack") {
    el.style.display = "flex";
    el.style.flexDirection = "column";
    el.style.gap = "8px";
  } else if (tag === "HStack") {
    el.style.display = "flex";
    el.style.flexDirection = "row";
    el.style.gap = "8px";
  } else if (tag === "Button") {
    el.style.padding = "8px 16px";
    el.style.borderRadius = "4px";
    el.style.border = "1px solid #444";
    el.style.background = "#2d2d2d";
    el.style.color = "#fff";
    el.style.cursor = "pointer";
  } else if (tag === "Text") {
    el.style.display = "inline";
  }

  nodeIdToElement.set(id, el);
  return id;
}

export function createText(content: string): number {
  const id = nextNodeId++;
  console.log("[DOM] createText:", JSON.stringify(content), "-> id:", id);
  const el = document.createTextNode(content);
  nodeIdToElement.set(id, el);
  return id;
}

export function createComment(content: string): number {
  const id = nextNodeId++;
  console.log("[DOM] createComment:", JSON.stringify(content), "-> id:", id);
  const el = document.createComment(content);
  nodeIdToElement.set(id, el);
  return id;
}

// AttributeValue variant type from WIT
// variant attribute-value {
//   str(string), bool(bool), s8(s8), s16(s16), s32(s32), s64(s64),
//   u8(u8), u16(u16), u32(u32), u64(u64), f32(f32), f64(f64), char(char)
// }
export type AttributeValue =
  | { tag: "str"; val: string }
  | { tag: "bool"; val: boolean }
  | { tag: "s8"; val: number }
  | { tag: "s16"; val: number }
  | { tag: "s32"; val: number }
  | { tag: "s64"; val: bigint }
  | { tag: "u8"; val: number }
  | { tag: "u16"; val: number }
  | { tag: "u32"; val: number }
  | { tag: "u64"; val: bigint }
  | { tag: "f32"; val: number }
  | { tag: "f64"; val: number }
  | { tag: "char"; val: string };

// Convert AttributeValue to string for DOM setAttribute
function attributeValueToString(value: AttributeValue): string {
  switch (value.tag) {
    case "str":
      return value.val;
    case "bool":
      return value.val ? "true" : "false";
    case "s8":
    case "s16":
    case "s32":
    case "u8":
    case "u16":
    case "u32":
      return String(value.val);
    case "f32":
      // Round f32 to 6 significant digits to avoid precision artifacts
      // (f32 has ~7 decimal digits of precision)
      return formatFloat(value.val, 6);
    case "f64":
      // Round f64 to 15 significant digits
      return formatFloat(value.val, 15);
    case "s64":
    case "u64":
      return value.val.toString();
    case "char":
      return value.val;
    default:
      return "";
  }
}

// Format a float with the given number of significant digits, removing trailing zeros
function formatFloat(value: number, precision: number): string {
  if (!Number.isFinite(value)) return String(value);
  // Use toPrecision to limit significant digits, then parseFloat to remove trailing zeros
  const formatted = parseFloat(value.toPrecision(precision));
  return String(formatted);
}

export function setAttribute(
  node: number,
  name: string,
  value: AttributeValue
): void {
  const el = nodeIdToElement.get(node);
  if (el && el instanceof HTMLElement) {
    const strValue = attributeValueToString(value);
    console.log(
      "[DOM] setAttribute: node=",
      node,
      "name=",
      name,
      "value=",
      value,
      "->",
      strValue
    );

    // Auto-set input type based on value type when setting "value" attribute
    if (name === "value" && el.tagName === "INPUT") {
      const inputType = inferInputType(value.tag);
      if (inputType) {
        el.setAttribute("type", inputType);
      }
    }

    el.setAttribute(name, strValue);
  }
}

// Infer HTML input type from AttributeValue tag
function inferInputType(tag: AttributeValue["tag"]): string | null {
  switch (tag) {
    case "f32":
    case "f64":
    case "s8":
    case "s16":
    case "s32":
    case "s64":
    case "u8":
    case "u16":
    case "u32":
    case "u64":
      return "number";
    case "bool":
      return "checkbox";
    case "str":
    case "char":
    default:
      return null; // Default to text
  }
}

export function removeAttribute(node: number, name: string): void {
  console.log("[DOM] removeAttribute: node=", node, "name=", name);
  const el = nodeIdToElement.get(node);
  if (el && el instanceof HTMLElement) {
    el.removeAttribute(name);
  }
}

export function setTextContent(node: number, content: string): void {
  console.log(
    "[DOM] setTextContent: node=",
    node,
    "content=",
    JSON.stringify(content)
  );
  const el = nodeIdToElement.get(node);
  if (el) {
    if (el.nodeType === Node.TEXT_NODE) {
      el.nodeValue = content;
    } else {
      el.textContent = content;
    }
  }
}

export function setStyle(node: number, property: string, value: string): void {
  console.log(
    "[DOM] setStyle: node=",
    node,
    "property=",
    property,
    "value=",
    value
  );
  const el = nodeIdToElement.get(node);
  if (el && el instanceof HTMLElement) {
    // Convert kebab-case to camelCase for style property
    const camelCase = property.replace(/-([a-z])/g, (_, letter) =>
      letter.toUpperCase()
    );
    (el.style as any)[camelCase] = value;
  }
}

export function setClass(node: number, className: string): void {
  console.log("[DOM] setClass: node=", node, "className=", className);
  const el = nodeIdToElement.get(node);
  if (el && el instanceof HTMLElement) {
    el.className = className;
  }
}

export function appendChild(parent: number, child: number): void {
  console.log("[DOM] appendChild: parent=", parent, "child=", child);
  const parentEl = nodeIdToElement.get(parent);
  const childEl = nodeIdToElement.get(child);
  console.log("[DOM]   parentEl=", parentEl, "childEl=", childEl);
  if (parentEl && childEl) {
    // Check if parent supports appendChild
    if (
      parentEl.nodeType === Node.ELEMENT_NODE ||
      parentEl.nodeType === Node.DOCUMENT_FRAGMENT_NODE
    ) {
      parentEl.appendChild(childEl);
    } else if (
      parentEl.nodeType === Node.TEXT_NODE ||
      parentEl.nodeType === Node.COMMENT_NODE
    ) {
      // Text/comment nodes for "if"/"for" control flow markers can't have children
      // Insert the child after the marker node instead
      const actualParent = parentEl.parentNode;
      if (actualParent) {
        actualParent.insertBefore(childEl, parentEl.nextSibling);
      }
    } else {
      console.warn("[DOM] Cannot appendChild to node type:", parentEl.nodeType);
    }
  }
}

export function insertBefore(
  parent: number,
  node: number,
  reference: number
): void {
  const parentEl = nodeIdToElement.get(parent);
  const nodeEl = nodeIdToElement.get(node);
  const refEl = reference === 0 ? null : nodeIdToElement.get(reference);
  if (parentEl && nodeEl) {
    parentEl.insertBefore(nodeEl, refEl || null);
  }
}

export function removeChild(parent: number, child: number): void {
  // Never remove the root element (node 0)
  if (child === 0) return;

  const parentEl = nodeIdToElement.get(parent);
  const childEl = nodeIdToElement.get(child);
  if (parentEl && childEl && parentEl.contains(childEl)) {
    parentEl.removeChild(childEl);
  }
}

export function remove(node: number): void {
  // Never remove the root element (node 0)
  if (node === 0) return;

  const el = nodeIdToElement.get(node);
  if (el && el.parentNode) {
    el.parentNode.removeChild(el);
  }
  nodeIdToElement.delete(node);
}

export function getParent(node: number): number {
  const el = nodeIdToElement.get(node);
  if (!el || !el.parentNode) return 0;
  // Find parent's node ID
  for (const [id, n] of nodeIdToElement) {
    if (n === el.parentNode) return id;
  }
  return 0;
}

export function getNextSibling(node: number): number {
  const el = nodeIdToElement.get(node);
  if (!el || !el.nextSibling) return 0;
  // Find sibling's node ID
  for (const [id, n] of nodeIdToElement) {
    if (n === el.nextSibling) return id;
  }
  return 0;
}

// Normalize DSL event names to browser event names
function normalizeBrowserEvent(eventName: string): string {
  const eventMap: Record<string, string> = {
    onclick: "click",
    clicked: "click",
    onmousedown: "mousedown",
    onmouseup: "mouseup",
    onmouseover: "mouseover",
    onmouseout: "mouseout",
    onkeydown: "keydown",
    onkeyup: "keyup",
    onchange: "change",
    oninput: "input",
    onfocus: "focus",
    onblur: "blur",
  };
  return eventMap[eventName] || eventName;
}

export function addEventListener(
  node: number,
  event: string,
  handlerId: number
): void {
  console.log(
    "[DOM] addEventListener: node=",
    node,
    "event=",
    event,
    "handlerId=",
    handlerId
  );
  const el = nodeIdToElement.get(node);
  if (!el || !(el instanceof HTMLElement)) return;

  const key = `${node}:${event}`;
  eventHandlers.set(key, handlerId);

  const browserEvent = normalizeBrowserEvent(event);
  const listener = (e: Event) => {
    e.preventDefault();
    if (dispatchCallback) {
      dispatchCallback(handlerId);
    }
  };

  // Store listener for removal
  (el as any)._yelListeners = (el as any)._yelListeners || {};
  (el as any)._yelListeners[`${event}:${handlerId}`] = listener;
  el.addEventListener(browserEvent, listener);
}

export function removeEventListener(
  node: number,
  event: string,
  handlerId: number
): void {
  const el = nodeIdToElement.get(node);
  if (!el || !(el instanceof HTMLElement)) return;

  const key = `${node}:${event}`;
  eventHandlers.delete(key);

  const browserEvent = normalizeBrowserEvent(event);
  const listenerKey = `${event}:${handlerId}`;
  const listener = (el as any)._yelListeners?.[listenerKey];
  if (listener) {
    el.removeEventListener(browserEvent, listener);
    delete (el as any)._yelListeners[listenerKey];
  }
}

export function insertAfter(
  parent: number,
  node: number,
  anchor: number
): void {
  console.log(
    "[DOM] insertAfter: parent=",
    parent,
    "node=",
    node,
    "anchor=",
    anchor
  );
  const parentEl = nodeIdToElement.get(parent);
  const nodeEl = nodeIdToElement.get(node);
  const anchorEl = anchor === 0 ? null : nodeIdToElement.get(anchor);
  console.log(
    "[DOM]   parentEl=",
    parentEl,
    "nodeEl=",
    nodeEl,
    "anchorEl=",
    anchorEl
  );
  if (parentEl && nodeEl) {
    if (anchorEl) {
      // Check if anchor is actually a child of parent
      if (anchorEl.parentNode !== parentEl) {
        console.error(
          "[DOM]   ERROR: anchor is not a child of parent!",
          "\n  anchor.parentNode=",
          anchorEl.parentNode,
          "\n  expected parent=",
          parentEl,
          "\n  anchor node id=",
          anchor,
          "\n  parent node id=",
          parent
        );
      }
      // Insert after anchor = insert before anchor's next sibling
      console.log(
        "[DOM]   inserting before anchorEl.nextSibling=",
        anchorEl.nextSibling
      );
      parentEl.insertBefore(nodeEl, anchorEl.nextSibling);
    } else {
      // No anchor = append at end
      console.log("[DOM]   no anchor, appending to end");
      parentEl.appendChild(nodeEl);
    }
  } else {
    console.error("[DOM]   ERROR: missing parentEl or nodeEl");
  }
}

// Export all DOM functions as an object for jco instantiation
export const dom = {
  createElement,
  createText,
  createComment,
  setAttribute,
  removeAttribute,
  setTextContent,
  setStyle,
  setClass,
  appendChild,
  insertBefore,
  removeChild,
  remove,
  getParent,
  getNextSibling,
  addEventListener,
  removeEventListener,
  insertAfter,
};
