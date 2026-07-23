//! Generates the MeshX design-system builtin-element table from
//! `design-elements.json` (the single source of truth shared with `yel-smith`).
//! Emits `PropDesc`/`PropType` literals included by `stdlib_lookup.rs`.

use std::{env, fs, path::PathBuf};

fn map_type(ty: &str) -> &'static str {
    match ty {
        "string" => "PropType::String",
        "bool" => "PropType::Bool",
        "s32" => "PropType::S32",
        "f32" => "PropType::F32",
        "color" => "PropType::Color",
        other => panic!("design-elements.json: unsupported prop type {:?}", other),
    }
}

fn main() {
    let manifest = env::var("CARGO_MANIFEST_DIR").unwrap();
    let json_path = PathBuf::from(&manifest).join("design-elements.json");
    println!("cargo:rerun-if-changed={}", json_path.display());

    let raw = fs::read_to_string(&json_path).expect("read design-elements.json");
    let doc: serde_json::Value = serde_json::from_str(&raw).expect("parse design-elements.json");
    let elements = doc["elements"].as_array().expect("elements array");

    let mut out = String::new();
    out.push_str("// @generated from design-elements.json by build.rs — do not edit.\n");
    out.push_str("static DESIGN_ELEMENTS: &[(&str, &[PropDesc])] = &[\n");
    for el in elements {
        let name = el["name"].as_str().expect("element name");
        out.push_str(&format!("    ({:?}, &[", name));
        for prop in el["props"].as_array().expect("props array") {
            let pname = prop["name"].as_str().expect("prop name");
            let pty = map_type(prop["type"].as_str().expect("prop type"));
            out.push_str(&format!("PropDesc {{ name: {:?}, ty: {} }}, ", pname, pty));
        }
        out.push_str("]),\n");
    }
    out.push_str("];\n");

    let dest = PathBuf::from(env::var("OUT_DIR").unwrap()).join("design_elements.rs");
    fs::write(&dest, out).expect("write generated design_elements.rs");
}
