<br>
<br>
<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="docs/yel_logo_dark.svg">
    <img width="500" alt="Yel language logo" src="docs/yel_logo_light.svg">
  </picture>
</p>

<br>

<p align="center">
  Declarative, portable domain specific language to build user interfaces 
</p>

<br>

<div align="center">

[![License](https://img.shields.io/github/license/szkabaroli/yel)](LICENSE)
  [![Last Commit](https://img.shields.io/github/last-commit/szkabaroli/yel)](https://github.com/szkabaroli/yel)
  [![Rust](https://img.shields.io/badge/rust-1.70+-orange.svg)](https://www.rust-lang.org)
  [![Cargo](https://img.shields.io/badge/cargo-latest-blue.svg)](https://crates.io)
</div>

<p align="center">
  <strong>⚠️ Highly WIP - Broken builds on main are common</strong>
</p>

## Project goals

- **Portability**: Compile Yel components to WebAssembly for true cross-platform UI development
- **Declarative syntax**: Clean, readable syntax for building user interfaces with components, properties, and callbacks
- **Reactivity**: Seamlessly built-in reactive signals - property changes automatically update the UI
- **Type safety**: Strong static typing with comprehensive type checking and inference
- **Zero trust**: Enable the building of pluggable user interfaces, without compromising security
- **Developer experience**: Tooling including VS Code extension, compiler diagnostics, and interactive viewer

## Example

```yel
package yel:counter@1.0.0;

export component Counter {
    count: s32 = 0;

    VStack {
        Text { "Count: {count}" }

        HStack {
            Button {
                "-"
                clicked: { count -= 1; }
            }
            Button {
                "+"
                clicked: { count += 1; }
            }
        }
    }
}
```

## License

[Apache-2.0](LICENSE)