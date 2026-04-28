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
  Button: "button",
  Text: "span",
  Card: "div",
  Box: "div",
  ZStack: "div",
  TextInput: "input",
  IntegerInput: "input",
  FloatInput: "input",
  Image: "img",
  Select: "select",
  Option: "option",
  // `<Icon name="…" size="…" tint="…" variant="…" />` renders as an
  // inline `<i>` whose `name` attribute drives the visible glyph (see
  // setAttribute below — `name` becomes a CSS class and the textContent
  // for emoji-fallback rendering when no icon font is loaded).
  Icon: "i",
};

// Node tracking
let nextNodeId = 1;
const nodeIdToElement = new Map<number, Node>();
const eventHandlers = new Map<string, number>(); // "nodeId:eventName" -> handlerId

// Dispatch callback - set by component host
/**
 * Tagged-union matching the `yel:ui/dispatch@0.1.0#event-value` WIT variant.
 * One arm per DOM event payload shape. `none` covers click / hover /
 * pressed / focus / blur / etc. The `input-*` arms carry the native form
 * of a DOM `<input>`'s current value, picked per the element's `type`
 * attribute so the guest never has to parse strings.
 */
export type EventValue =
  | { tag: "none" }
  | { tag: "input-text"; val: string }
  | { tag: "input-f64"; val: number }
  | { tag: "input-f32"; val: number }
  | { tag: "input-s32"; val: number }
  | { tag: "input-bool"; val: boolean };

let dispatchCallback:
  | ((handlerId: number, event: EventValue) => void)
  | null = null;

export function setDispatchCallback(
  callback: (handlerId: number, event: EventValue) => void,
) {
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
  mountTime = performance.now();
  ensureFlashStylesInstalled();
}

// ============================================================================
// Reactive-update debug flash
// ============================================================================
//
// When `reactiveFlashEnabled` is true, every DOM mutation that isn't part of
// the initial mount briefly outlines the affected element so the user can
// *see* which DOM nodes the reactive system is updating. Behaviour mirrors
// React DevTools' "Highlight updates when components render" toggle.
//
// We suppress flashes that fire within `MOUNT_QUIESCE_MS` of the last
// `reset()` call so the initial render (which mutates every node) doesn't
// drown the UI in one big flash. After that window, every post-mount
// mutation flashes.
let reactiveFlashEnabled = false;
let mountTime = 0;
const MOUNT_QUIESCE_MS = 60;
const FLASH_DURATION_MS = 250;
const FLASH_CLASS = "yel-reactive-flash";
const FLASH_STYLE_ID = "yel-reactive-flash-style";

export function setReactiveFlashEnabled(enabled: boolean): void {
  reactiveFlashEnabled = enabled;
}

function ensureFlashStylesInstalled(): void {
  if (document.getElementById(FLASH_STYLE_ID)) return;
  const style = document.createElement("style");
  style.id = FLASH_STYLE_ID;
  // Outline + subtle fill so it works on both text-in-a-span and block
  // containers. 450ms total: 150ms fade in via box-shadow expansion,
  // 300ms fade out. Tuned to be noticeable without being annoying on
  // rapid-update streams (typing, drag).
  style.textContent = `
    @keyframes ${FLASH_CLASS}-pulse {
      0%   { outline: 2px solid rgba(99, 179, 237, 0.9); background-color: rgba(99, 179, 237, 0.18); }
      60%  { outline: 2px solid rgba(99, 179, 237, 0.55); background-color: rgba(99, 179, 237, 0.08); }
      100% { outline: 2px solid rgba(99, 179, 237, 0);   background-color: rgba(99, 179, 237, 0); }
    }
    .${FLASH_CLASS} {
      animation: ${FLASH_CLASS}-pulse ${FLASH_DURATION_MS}ms ease-out forwards;
      outline-offset: -2px;
      transition: none;
    }
  `;
  document.head.appendChild(style);
}

/**
 * Play the flash animation on an HTMLElement directly. Restarts the
 * animation if it's already running so rapid successive updates each
 * produce a visible pulse. Skipped if the flash is disabled or we're
 * still in the mount-quiesce window.
 */
function flashElement(el: HTMLElement): void {
  if (!reactiveFlashEnabled) return;
  if (performance.now() - mountTime < MOUNT_QUIESCE_MS) return;
  el.classList.remove(FLASH_CLASS);
  void el.offsetWidth;
  el.classList.add(FLASH_CLASS);
  setTimeout(() => {
    if (el.classList.contains(FLASH_CLASS)) {
      el.classList.remove(FLASH_CLASS);
    }
  }, FLASH_DURATION_MS + 20);
}

/**
 * Trigger a flash on the Yel node whose id is `id`. If the node is a
 * text/comment node (which can't carry CSS classes), flash its parent
 * element instead so the update is still visible.
 */
function flashReactiveUpdate(id: number): void {
  if (!reactiveFlashEnabled) return;
  const node = nodeIdToElement.get(id);
  if (!node) return;
  flashNodeOrAncestor(node);
}

/**
 * Flash the given DOM node, or the nearest HTMLElement ancestor if the
 * node itself can't carry classes (text / comment / detached). Used by
 * structural mutations (appendChild / insertAfter / remove) where the
 * visible change is "content under this container" rather than "an
 * attribute on this element".
 */
function flashNodeOrAncestor(node: Node): void {
  if (!reactiveFlashEnabled) return;
  let cur: Node | null = node;
  while (cur && !(cur instanceof HTMLElement)) {
    cur = cur.parentNode;
  }
  if (cur instanceof HTMLElement) {
    flashElement(cur);
  }
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
    el.style.cursor = "pointer";
  } else if (tag === "Text") {
    el.style.display = "inline";
  } else if (tag === "Select") {
    el.style.padding = "4px 8px";
    el.style.borderRadius = "4px";
    el.style.border = "1px solid #444";
    el.style.background = "#2d2d2d";
    el.style.color = "inherit";
    el.style.minWidth = "150px";
  } else if (tag === "Icon") {
    el.style.display = "inline-flex";
    el.style.alignItems = "center";
    el.style.justifyContent = "center";
    el.style.fontStyle = "normal";
    el.style.lineHeight = "1";
    el.style.userSelect = "none";
  } else if (tag === "TextInput" || tag === "IntegerInput" || tag === "FloatInput") {
    el.style.padding = "4px 8px";
    el.style.borderRadius = "4px";
    el.style.border = "1px solid #444";
    el.style.background = "#2d2d2d";
    el.style.color = "inherit";
    if (el instanceof HTMLInputElement) {
      if (tag === "IntegerInput") {
        el.type = "number";
        el.step = "1";
      } else if (tag === "FloatInput") {
        el.type = "number";
        el.step = "any";
      } else {
        el.type = "text";
      }
    }
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

/**
 * Layout-neutral wrapper element used by `for` iterations and `if`
 * branches to group their content under a single DOM root. Removing
 * the wrapper cascades to detach every descendant, so iter / branch
 * teardown is a single host `remove` call regardless of body shape.
 *
 * Implemented as a `yel-frag` custom element with `display: contents`
 * (set in CSS) so it has zero visual / flex / grid effect — children
 * lay out exactly as if they were direct children of the wrapper's
 * parent.
 */
export function createFragment(): number {
  const id = nextNodeId++;
  console.log("[DOM] createFragment -> id:", id);
  const el = document.createElement("yel-frag");
  el.setAttribute("data-node-id", String(id));
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

    el.setAttribute(name, strValue);

    // Icon-specific attribute mapping — `Icon` is just an `<i>` shell
    // by default (see TAG_MAP); per-property semantics live here so
    // every Icon prop change shows up visually without users having to
    // wire CSS by hand. Match the property surface declared in any
    // `element Icon { name; size; tint; variant; }` decl in user yel.
    if (el.getAttribute("data-yel-tag") === "Icon") {
      switch (name) {
        case "name": {
          // Drive both an `icon-<name>` class (for an icon font) AND
          // textContent (a hardcoded emoji-ish fallback for the cases
          // we know about) so the icon is visible even before a real
          // icon-font stylesheet is wired in.
          el.className = `icon icon-${strValue}`;
          const fallback: Record<string, string> = {
            flag: "🚩",
            heart: "♥",
            star: "★",
            check: "✓",
            close: "✕",
            menu: "☰",
            search: "🔍",
            settings: "⚙",
            user: "👤",
            home: "⌂",
          };
          el.textContent = fallback[strValue] ?? "";
          break;
        }
        case "size": {
          // s32 in WIT — strValue is a base-10 integer string. Apply
          // as CSS pixel size for both width/height and font-size so
          // both real glyph fonts and emoji fallbacks scale.
          const px = parseInt(strValue, 10);
          if (!Number.isNaN(px)) {
            el.style.width = `${px}px`;
            el.style.height = `${px}px`;
            el.style.fontSize = `${px}px`;
          }
          break;
        }
        case "tint":
          // `tint: color` — accept any CSS color string. The codegen
          // currently surfaces colours as their literal string form.
          el.style.color = strValue;
          break;
        case "variant":
          // "regular" / "fill" — append as a modifier class so user
          // CSS can target `.icon.regular` vs `.icon.fill`.
          el.classList.add(strValue);
          break;
      }
    }

    // DOM quirk: after the user has typed into an <input>, the
    // browser tracks the visible value via the `.value` PROPERTY
    // not the attribute. `setAttribute("value", ...)` only seeds
    // the initial value on mount; subsequent attribute writes are
    // ignored by the browser's display logic. To keep reactive
    // two-way bindings working, mirror the write into the property
    // too. Same treatment for `checked` on checkboxes.
    if (el instanceof HTMLInputElement) {
      if (name === "value") {
        if (el.value !== strValue) el.value = strValue;
      } else if (name === "checked") {
        const desired = value.tag === "bool" ? (value as any).val : strValue === "true";
        if (el.checked !== desired) el.checked = desired;
      }
    } else if (el instanceof HTMLTextAreaElement && name === "value") {
      if (el.value !== strValue) el.value = strValue;
    } else if (el instanceof HTMLSelectElement && name === "value") {
      if (el.value !== strValue) el.value = strValue;
    }

    flashReactiveUpdate(node);
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
    flashReactiveUpdate(node);
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
    flashReactiveUpdate(node);
  }
}

export function setClass(node: number, className: string): void {
  console.log("[DOM] setClass: node=", node, "className=", className);
  const el = nodeIdToElement.get(node);
  if (el && el instanceof HTMLElement) {
    el.className = className;
    flashReactiveUpdate(node);
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
      // Flash the containing element so if/for re-mounts are visible.
      flashNodeOrAncestor(parentEl);
    } else if (
      parentEl.nodeType === Node.TEXT_NODE ||
      parentEl.nodeType === Node.COMMENT_NODE
    ) {
      // Text/comment nodes for "if"/"for" control flow markers can't have children
      // Insert the child after the marker node instead
      const actualParent = parentEl.parentNode;
      if (actualParent) {
        actualParent.insertBefore(childEl, parentEl.nextSibling);
        flashNodeOrAncestor(actualParent);
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
    flashNodeOrAncestor(parentEl);
  }
}

export function removeChild(parent: number, child: number): void {
  // Never remove the root element (node 0)
  if (child === 0) return;

  const parentEl = nodeIdToElement.get(parent);
  const childEl = nodeIdToElement.get(child);
  if (parentEl && childEl && parentEl.contains(childEl)) {
    // Flash BEFORE the remove so the element that's about to vanish
    // has a last visible blink on its container.
    flashNodeOrAncestor(parentEl);
    parentEl.removeChild(childEl);
  }
}

export function remove(node: number): void {
  // Never remove the root element (node 0)
  if (node === 0) return;

  const el = nodeIdToElement.get(node);
  if (el && el.parentNode) {
    // Flash the PARENT before removal — the element itself is about to
    // detach and can't carry the animation.
    flashNodeOrAncestor(el.parentNode);
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
    // Diagnostic: logs every DOM event the guest registered a
    // handler for. If you don't see this firing in the console,
    // the browser isn't dispatching the event (likely causes:
    // `<input type=...>` blocks `input` events for certain kinds;
    // the listener was never attached; the target element is
    // different from the one we installed on).
    console.log(
      "[DOM] event fired:",
      browserEvent,
      "on node",
      node,
      "handlerId=",
      handlerId,
      "target=",
      e.target,
    );
    e.preventDefault();
    if (!dispatchCallback) return;

    // Build the event-value payload matching the guest's expected
    // variant. For DOM `input` events on <input>/<textarea>/<select>
    // we inspect the target's `type` and read its native value
    // (`valueAsNumber` for number inputs, `checked` for checkboxes).
    // For everything else we send `none` — the guest ignores the arg
    // unless it's a binding-setter handler.
    let payload: EventValue = { tag: "none" };
    if (browserEvent === "input" || browserEvent === "change") {
      const target = e.target;
      if (target instanceof HTMLInputElement) {
        const kind = target.type;
        if (kind === "number" || kind === "range") {
          payload = { tag: "input-f64", val: target.valueAsNumber };
        } else if (kind === "checkbox" || kind === "radio") {
          payload = { tag: "input-bool", val: target.checked };
        } else {
          payload = { tag: "input-text", val: target.value };
        }
      } else if (
        target instanceof HTMLTextAreaElement ||
        target instanceof HTMLSelectElement
      ) {
        payload = { tag: "input-text", val: target.value };
      }
    }

    dispatchCallback(handlerId, payload);
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
    // Flash the container so if/for re-mounts are visible. Works for
    // both the insert-after and the append-fallback branches.
    flashNodeOrAncestor(parentEl);
  } else {
    console.error("[DOM]   ERROR: missing parentEl or nodeEl");
  }
}

// Export all DOM functions as an object for jco instantiation
export const dom = {
  createElement,
  createText,
  createComment,
  createFragment,
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
