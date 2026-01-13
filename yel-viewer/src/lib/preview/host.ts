/**
 * Yel Component Host for Preview
 *
 * This module uses jco to transpile WebAssembly Component Model binaries
 * and instantiate them with DOM bindings for browser preview.
 */

// @ts-ignore - jco types may not be available
import { transpile } from "@bytecodealliance/jco";
import * as dom from "./dom";

export interface ComponentInstance {
  mount(rootId: number): void;
  unmount(): void;
  dispatch(handlerId: number): void;
  getProperty(name: string): any;
  setProperty(name: string, value: any): void;
}

export interface HostedComponent {
  instance: ComponentInstance | null;
  componentClass: any;
  dispatch: ((handlerId: number) => void) | null;
}

/**
 * Transpile and instantiate a WASM component from bytes using jco
 */
export interface InstantiateOptions {
  callbacks?: Record<string, () => void>;
  onDispatch?: (handlerId: number) => void;
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
    let dispatchFn: ((handlerId: number) => void) | null = null;
    dom.setDispatchCallback((handlerId) => {
      console.log("[Preview] DOM Dispatch:", handlerId);
      if (dispatchFn) {
        dispatchFn(handlerId);
      }
      // Notify caller that dispatch happened (for state updates)
      onDispatch?.(handlerId);
    });

    // Create the imports object for instantiation
    // jco expects imports keyed by the WIT interface name
    // Wrap with Proxy to catch ALL function calls
    const domImplBase = {
      createElement: dom.createElement,
      createText: dom.createText,
      createComment: dom.createComment,
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

    // Create a proxy that handles any callback interface
    const callbacksProxy = new Proxy({}, {
      get(target, prop) {
        return () => {
          console.log(`[Preview] Callback: ${String(prop)}`);
          if (callbacks && typeof callbacks[String(prop)] === 'function') {
            callbacks[String(prop)]();
          }
        };
      }
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
        console.log(`[Preview] Unknown import requested: ${key}`);
        return undefined;
      },
      has(target, prop) {
        const key = String(prop);
        return key in target || key.includes('callbacks');
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

    // Find the component class from the instance exports
    let ComponentClass: any = null;
    let componentName: string | null = null;

    for (const [key, value] of Object.entries(instance)) {
      console.log("[Preview] Instance export:", key, typeof value);
      if (typeof value === "object" && value !== null && !Array.isArray(value)) {
        // Find the component class (resource constructor)
        for (const [className, classValue] of Object.entries(value as object)) {
          if (typeof classValue === "function") {
            ComponentClass = classValue;
            componentName = className;
            console.log("[Preview] Found component class:", className);
            break;
          }
        }
      }
    }

    if (!ComponentClass) {
      console.error("[Preview] Instance exports:", instance);
      throw new Error("No component class found in instantiated module");
    }

    // Create component instance
    console.log("[Preview] Creating component instance...");
    const componentInstance = new ComponentClass();
    console.log("[Preview] Component instance created:", componentInstance);
    console.log("[Preview] Component instance methods:", Object.getOwnPropertyNames(Object.getPrototypeOf(componentInstance)));

    // dispatch is now a method on the resource instance, not a separate interface
    if (typeof componentInstance.dispatch === "function") {
      dispatchFn = (handlerId: number) => componentInstance.dispatch(handlerId);
      console.log("[Preview] Found dispatch method on component instance");
    } else {
      console.warn("[Preview] No dispatch method found on component instance");
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
      dispatch(handlerId: number) {
        if (dispatchFn) {
          dispatchFn(handlerId);
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

  dispatch(handlerId: number): void {
    this.component?.instance?.dispatch(handlerId);
  }

  getProperty(name: string): any {
    return this.component?.instance?.getProperty(name);
  }

  setProperty(name: string, value: any): void {
    this.component?.instance?.setProperty(name, value);
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
