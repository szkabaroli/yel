/**
 * Yel Component Host for Preview
 *
 * This module uses jco to transpile WebAssembly Component Model binaries
 * and instantiate them with DOM bindings for browser preview.
 */

// @ts-ignore - jco types may not be available
// The bare `@bytecodealliance/jco` specifier is pinned to the top-
// level 1.18.1 install's `src/browser.js` via a vite resolve.alias in
// vite.config.ts — that's the wasm-bindgen-generated `generate`
// wrapper whose bundled wasmparser 0.245.1 accepts our GC rec-group
// type declarations. Without the alias, vite can follow a transitive
// 1.15.4 resolution whose older wasmparser rejects rec groups.
import { transpile } from "@bytecodealliance/jco";
import * as dom from "./dom";
import type { EventValue } from "./dom";

export interface ComponentInstance {
  mount(rootId: number): void;
  unmount(): void;
  dispatch(handlerId: number, event: EventValue): void;
  getProperty(name: string): any;
  setProperty(name: string, value: any): void;
  /**
   * Property names discoverable on this component — every `getFoo()`
   * method on the underlying resource becomes `foo` here. Mirrors what
   * the generated WIT getter/setter pairs expose to the host.
   */
  listProperties(): string[];
}

export interface HostedComponent {
  instance: ComponentInstance | null;
  componentClass: any;
  dispatch: ((handlerId: number, event: EventValue) => void) | null;
}

/**
 * Transpile and instantiate a WASM component from bytes using jco
 */
export interface InstantiateOptions {
  callbacks?: Record<string, () => void>;
  onDispatch?: (handlerId: number, event: EventValue) => void;
}

export async function instantiateComponent(
  wasmBytes: Uint8Array,
  rootElement: HTMLElement,
  options?: InstantiateOptions
): Promise<HostedComponent> {
  const { callbacks, onDispatch } = options || {};
  // Reset DOM state and set root element
  dom.reset(rootElement);

  try {
    console.log("[Preview] Transpiling component with jco...", wasmBytes.length, "bytes");

    // Transpile the component using jco with instantiation mode
    // This generates an instantiate() function that accepts imports
    const result = await transpile(wasmBytes, {
      name: "component",
      // The "browser" export of jco (`src/browser.js`) aliases
      // `transpile` to the raw underlying `generate` function, which
      // expects the pre-wrapped variant form `{ tag: 'async' }`.
      // The node/CLI `transpile` wraps a plain string — different
      // entrypoint, different API. We're in the browser path here.
      instantiation: { tag: "async" },
    });

    console.log("[Preview] Transpile result:", result);

    // Get files from result
    const files: Map<string, string | Uint8Array> = result.files || result;

    // Find the main JS file and core WASM files
    let mainJs: string | null = null;
    const wasmModules: Map<string, Uint8Array> = new Map();

    for (const [filename, content] of files) {
      console.log("[Preview] File:", filename, typeof content);
      if (filename === "component.js") {
        mainJs = typeof content === "string" ? content : new TextDecoder().decode(content);
      } else if (filename.endsWith(".wasm")) {
        const bytes = content instanceof Uint8Array ? content : new TextEncoder().encode(content as string);
        wasmModules.set(filename, bytes);
      }
    }

    if (!mainJs) {
      throw new Error("jco transpile did not produce component.js");
    }

    console.log("[Preview] Main JS length:", mainJs.length);
    console.log("[Preview] WASM modules:", wasmModules.size);

    // Create a function to compile WASM from our stored modules
    const getCoreModule = async (url: string): Promise<WebAssembly.Module> => {
      // Extract filename from URL
      const filename = url.split('/').pop() || url;
      console.log("[Preview] Loading core module:", filename);

      const bytes = wasmModules.get(filename);
      if (!bytes) {
        throw new Error(`WASM module not found: ${filename}`);
      }
      return WebAssembly.compile(bytes as BufferSource);
    };

    // Create blob URLs for WASM files (needed by the generated code)
    const wasmBlobUrls = new Map<string, string>();
    for (const [filename, bytes] of wasmModules) {
      const blob = new Blob([bytes as BlobPart], { type: "application/wasm" });
      const blobUrl = URL.createObjectURL(blob);
      wasmBlobUrls.set(filename, blobUrl);
    }

    // Replace URL references in the generated JS with blob URLs
    let modifiedJs = mainJs;
    for (const [filename, blobUrl] of wasmBlobUrls) {
      const escapedFilename = filename.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
      // Replace fetchCompile(new URL('./filename', import.meta.url))
      modifiedJs = modifiedJs.replace(
        new RegExp(`new URL\\(['"]\\.\\/` + escapedFilename + `['"],\\s*import\\.meta\\.url\\)`, 'g'),
        `"${blobUrl}"`
      );
    }

    console.log("[Preview] Modified JS (first 1000 chars):", modifiedJs.substring(0, 1000));

    // Create blob URL for the main module
    const mainBlob = new Blob([modifiedJs], { type: "text/javascript" });
    const mainBlobUrl = URL.createObjectURL(mainBlob);

    console.log("[Preview] Importing module from blob URL...");

    // Dynamically import the transpiled module
    const componentModule = await import(/* @vite-ignore */ mainBlobUrl);

    console.log("[Preview] Module imported:", Object.keys(componentModule));

    // Set up dispatch callback for DOM events
    let dispatchFn: ((handlerId: number, event: EventValue) => void) | null =
      null;
    dom.setDispatchCallback((handlerId, event) => {
      console.log("[Preview] DOM Dispatch:", handlerId, event);
      if (dispatchFn) {
        dispatchFn(handlerId, event);
      }
      // Notify caller that dispatch happened (for state updates)
      onDispatch?.(handlerId, event);
    });

    // Create the imports object for instantiation
    // jco expects imports keyed by the WIT interface name
    // Wrap with Proxy to catch ALL function calls
    const domImplBase = {
      createElement: dom.createElement,
      createText: dom.createText,
      createComment: dom.createComment,
      createFragment: dom.createFragment,
      setAttribute: dom.setAttribute,
      removeAttribute: dom.removeAttribute,
      setTextContent: dom.setTextContent,
      setStyle: dom.setStyle,
      setClass: dom.setClass,
      appendChild: dom.appendChild,
      insertBefore: dom.insertBefore,
      removeChild: dom.removeChild,
      remove: dom.remove,
      getParent: dom.getParent,
      getNextSibling: dom.getNextSibling,
      addEventListener: dom.addEventListener,
      removeEventListener: dom.removeEventListener,
      insertAfter: dom.insertAfter,
    };

    // Proxy to catch any function call, including ones with different names
    const domImpl = new Proxy(domImplBase, {
      get(target, prop) {
        const key = String(prop);
        console.log("[DOM-PROXY] Accessing:", key);
        if (key in target) {
          const fn = (target as any)[key];
          if (typeof fn === "function") {
            return (...args: any[]) => {
              console.log(`[DOM-PROXY] Calling ${key} with:`, args);
              return fn(...args);
            };
          }
          return fn;
        }
        console.log("[DOM-PROXY] Unknown property:", key);
        return undefined;
      }
    });

    // Extract the raw u32 handle from whatever jco hands our callback. The
    // WIT declares `self: borrow<resource>`, so jco lifts the u32 coming
    // from the core module into a resource instance (stored on a private
    // `Symbol('handle')`). We reach into the symbol bag to recover the id
    // without depending on jco module internals.
    const unwrapHandle = (value: unknown): number | undefined => {
      if (typeof value === 'number') return value;
      if (value && typeof value === 'object') {
        for (const sym of Object.getOwnPropertySymbols(value)) {
          if (sym.description === 'handle') {
            const h = (value as Record<symbol, unknown>)[sym];
            if (typeof h === 'number') return h;
          }
        }
      }
      return undefined;
    };

    // Registry of handle → live component instance. Populated right after
    // `new ComponentClass()` below so callbacks fired from inside the
    // WASM have full access to the exported resource methods (mount,
    // unmount, getters/setters) without the host having to keep a
    // separate side table. One entry per `[resource-new]` call.
    const componentByHandle = new Map<number, Record<string, any>>();

    // Create a proxy that handles any callback interface. Every callback
    // takes the invoking component's resource handle as its first param so
    // the host can route the call back to the live component instance.
    const callbacksProxy = new Proxy({}, {
      get(target, prop) {
        return (selfArg: unknown, ...rest: unknown[]) => {
          const selfHandle = unwrapHandle(selfArg);
          const owner = selfHandle !== undefined
            ? componentByHandle.get(selfHandle)
            : undefined;
          console.log(
            `[Preview] Callback: ${String(prop)} handle=${selfHandle} owner=`,
            owner
          );
          const userCb = callbacks && callbacks[String(prop)];
          if (typeof userCb === 'function') {
            // Hand user code the rich component instance if we have one,
            // otherwise fall back to the raw handle.
            const firstArg = owner ?? selfHandle;
            (userCb as (...args: unknown[]) => void)(firstArg, ...rest);
          }
        };
      }
    });

    // Stub for `{component}-component` interfaces that appear as world
    // imports purely to satisfy wit-component's encoder ordering (so
    // callbacks' `use {component}-component.{resource}` can resolve). The
    // component actually EXPORTS these interfaces; the import is a
    // forward-declaration and nothing on it is ever called at runtime.
    // jco still destructures the resource class out of the object at
    // instantiate time, so return an object with a dummy constructor.
    const componentImportStub = new Proxy({}, {
      get() {
        return class StubResource {};
      },
      has() {
        return true;
      },
    });

    // Try different key formats that jco might expect
    // The imports use a Proxy to handle any interface name dynamically
    const imports: Record<string, any> = new Proxy({
      // Known imports
      "yel:ui/dom@0.1.0": domImpl,
      "yel:ui/dom": domImpl,
    }, {
      get(target, prop) {
        const key = String(prop);
        // Return known imports
        if (key in target) {
          return (target as any)[key];
        }
        // Handle any callbacks interface dynamically
        if (key.includes('callbacks')) {
          console.log(`[Preview] Providing callbacks for: ${key}`);
          return callbacksProxy;
        }
        // `{component}-component` interfaces appear as both imports and
        // exports in the generated world — the import side is just for
        // wit-component's type-resolution and never invoked at runtime.
        if (key.endsWith('-component') || key.includes('-component@')) {
          console.log(`[Preview] Providing component-interface stub for: ${key}`);
          return componentImportStub;
        }
        console.log(`[Preview] Unknown import requested: ${key}`);
        return undefined;
      },
      has(target, prop) {
        const key = String(prop);
        return key in target
          || key.includes('callbacks')
          || key.endsWith('-component')
          || key.includes('-component@');
      }
    });

    console.log("[Preview] Imports object keys:", Object.keys(imports));

    console.log("[Preview] Calling instantiate with imports...");

    // The transpiled module should export an instantiate function
    if (typeof componentModule.instantiate !== "function") {
      console.error("[Preview] Module exports:", componentModule);
      throw new Error("Transpiled module does not export instantiate function");
    }

    // Instantiate the component with our imports
    const instance = await componentModule.instantiate(getCoreModule, imports);

    console.log("[Preview] Instance created:", instance);
    console.log("[Preview] Instance keys:", Object.keys(instance));

    // Clean up blob URLs
    setTimeout(() => {
      URL.revokeObjectURL(mainBlobUrl);
      wasmBlobUrls.forEach(url => URL.revokeObjectURL(url));
    }, 5000);

    // Find the component class and freestanding dispatch from the instance exports.
    // The exported interface contains a resource class (constructor) and a
    // freestanding `dispatch` function.
    let ComponentClass: any = null;
    let componentName: string | null = null;

    for (const [key, value] of Object.entries(instance)) {
      console.log("[Preview] Instance export:", key, typeof value);
      if (typeof value === "object" && value !== null && !Array.isArray(value)) {
        const iface = value as Record<string, unknown>;
        for (const [memberName, memberValue] of Object.entries(iface)) {
          if (typeof memberValue === "function" && memberName !== "dispatch") {
            ComponentClass = memberValue;
            componentName = memberName;
            console.log("[Preview] Found component class:", memberName);
          }
          if (memberName === "dispatch" && typeof memberValue === "function") {
            dispatchFn = memberValue as (
              handlerId: number,
              event: EventValue,
            ) => void;
            console.log("[Preview] Found freestanding dispatch function");
          }
        }
      }
    }

    if (!ComponentClass) {
      // The component has no exported resource to instantiate. This is a
      // valid state for libraries, globals-only files, and in-progress
      // code. Render a placeholder instead of erroring so the preview tab
      // stays usable while the user edits.
      console.log(
        "[Preview] No component class to instantiate; rendering empty placeholder.",
      );
      rootElement.textContent = "";
      const empty = document.createElement("div");
      empty.style.cssText =
        "padding: 1rem; color: var(--color-muted-foreground, #888); font-size: 0.875rem; text-align: center;";
      empty.textContent =
        "No exported component to preview. Add `export` before a component declaration.";
      rootElement.appendChild(empty);
      
      return {
        instance: {
          mount() {},
          unmount() {},
          dispatch(_handlerId: number, _event: EventValue) {},
          getProperty: () => undefined,
          setProperty: () => undefined,
          listProperties: () => [],
        },
        componentClass: null,
        dispatch: null,
      };
    }

    // Create component instance
    console.log("[Preview] Creating component instance...");
    const componentInstance = new ComponentClass();
    console.log("[Preview] Component instance created:", componentInstance);
    console.log("[Preview] Component instance methods:", Object.getOwnPropertyNames(Object.getPrototypeOf(componentInstance)));

    // Register the instance under its resource handle so callbacks fired
    // from inside the WASM can look up the live JS component (with its
    // full getter/setter surface) via `self: borrow<resource>`.
    const instanceHandle = unwrapHandle(componentInstance);
    if (instanceHandle !== undefined) {
      componentByHandle.set(instanceHandle, componentInstance);
      console.log(`[Preview] Registered component handle=${instanceHandle}`);
    }

    // Create wrapper
    const wrapper: ComponentInstance = {
      mount(rootId: number) {
        console.log("[Preview] Mounting to root:", rootId);
        if (componentInstance.mount) {
          componentInstance.mount(rootId);
        }
      },
      unmount() {
        if (componentInstance.unmount) {
          componentInstance.unmount();
        }
      },
      dispatch(handlerId: number, event: EventValue) {
        if (dispatchFn) {
          dispatchFn(handlerId, event);
        }
      },
      getProperty(name: string) {
        const getter = componentInstance[`get${name.charAt(0).toUpperCase()}${name.slice(1)}`];
        if (getter) {
          return getter.call(componentInstance);
        }
        return undefined;
      },
      setProperty(name: string, value: any) {
        const setter = componentInstance[`set${name.charAt(0).toUpperCase()}${name.slice(1)}`];
        if (setter) {
          setter.call(componentInstance, value);
        }
      },
      listProperties() {
        // jco camel-cases WIT kebab names: `get-count` → `getCount`,
        // `set-dark-mode` → `setDarkMode`. A property is any `getFoo`
        // method whose sibling `setFoo` also exists.
        const proto = Object.getPrototypeOf(componentInstance);
        if (!proto) return [];
        const names = new Set<string>();
        for (const key of Object.getOwnPropertyNames(proto)) {
          if (!key.startsWith('get') || key.length <= 3) continue;
          if (typeof (componentInstance as any)[key] !== 'function') continue;
          const base = key[3].toLowerCase() + key.slice(4);
          // Skip things like `getCurrentFoo` unless there's a paired setter.
          const setter = 'set' + key.slice(3);
          if (typeof (componentInstance as any)[setter] === 'function') {
            names.add(base);
          }
        }
        return [...names];
      },
    };

    console.log(`[Preview] Component "${componentName}" loaded successfully`);

    return {
      instance: wrapper,
      componentClass: ComponentClass,
      dispatch: dispatchFn,
    };
  } catch (error) {
    console.error("[Preview] Failed to instantiate component:", error);
    throw error;
  }
}

/**
 * Host class for managing component lifecycle
 */
export class YelPreviewHost {
  private rootElement: HTMLElement | null = null;
  private component: HostedComponent | null = null;
  private mounted = false;

  constructor() {}

  init(rootElement: HTMLElement): this {
    this.rootElement = rootElement;
    dom.reset(rootElement);
    return this;
  }

  async load(
    wasmBytes: Uint8Array,
    options?: InstantiateOptions
  ): Promise<boolean> {
    if (!this.rootElement) {
      console.error("[Preview] Host not initialized");
      return false;
    }

    try {
      this.component = await instantiateComponent(wasmBytes, this.rootElement, options);
      return this.component.instance !== null;
    } catch (error) {
      console.error("[Preview] Failed to load:", error);
      return false;
    }
  }

  mount(): boolean {
    if (!this.component?.instance || !this.rootElement) {
      return false;
    }

    try {
      this.rootElement.innerHTML = "";
      this.component.instance.mount(0);
      this.mounted = true;
      console.log("[Preview] Component mounted");
      return true;
    } catch (error) {
      console.error("[Preview] Failed to mount component:", error);
      return false;
    }
  }

  unmount(): void {
    if (this.mounted && this.component?.instance) {
      try {
        this.component.instance.unmount();
      } catch (error) {
        console.error("[Preview] Failed to unmount component:", error);
      }
    }
    this.mounted = false;
    if (this.rootElement) {
      this.rootElement.innerHTML = "";
    }
  }

  dispatch(
    handlerId: number,
    event: EventValue = { tag: "none" },
  ): void {
    this.component?.instance?.dispatch(handlerId, event);
  }

  getProperty(name: string): any {
    return this.component?.instance?.getProperty(name);
  }

  setProperty(name: string, value: any): void {
    this.component?.instance?.setProperty(name, value);
  }

  listProperties(): string[] {
    return this.component?.instance?.listProperties() ?? [];
  }

  isLoaded(): boolean {
    return this.component?.instance !== null;
  }

  isMounted(): boolean {
    return this.mounted;
  }

  destroy(): void {
    this.unmount();
    this.component = null;
    this.rootElement = null;
  }
}
