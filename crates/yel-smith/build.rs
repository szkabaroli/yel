//! Generates the MeshX design-system element table for the fuzzer from
//! `../yel-core/design-elements.json` (the single source of truth shared with
//! the compiler stdlib). Emits `TypeRef` literals included by `lib.rs`.

use std::{env, fs, path::PathBuf};

fn map_type(ty: &str) -> &'static str {
    match ty {
        "string" => "TypeRef::String",
        "bool" => "TypeRef::Bool",
        "s32" => "TypeRef::S32",
        "f32" => "TypeRef::F32",
        "color" => "TypeRef::Color",
        other => panic!("design-elements.json: unsupported prop type {:?}", other),
    }
}

fn main() {
    let manifest = env::var("CARGO_MANIFEST_DIR").unwrap();
    let json_path = PathBuf::from(&manifest)
        .join("..")
        .join("yel-core")
        .join("design-elements.json");
    println!("cargo:rerun-if-changed={}", json_path.display());

    let raw = fs::read_to_string(&json_path).expect("read design-elements.json");
    let doc: serde_json::Value = serde_json::from_str(&raw).expect("parse design-elements.json");
    let elements = doc["elements"].as_array().expect("elements array");

    let mut out = String::new();
    out.push_str(
        "// @generated from ../yel-core/design-elements.json by build.rs — do not edit.\n",
    );
    out.push_str("static DESIGN_ELEMENTS: &[(&str, &[(&str, TypeRef)])] = &[\n");
    for el in elements {
        let name = el["name"].as_str().expect("element name");
        out.push_str(&format!("    ({:?}, &[", name));
        for prop in el["props"].as_array().expect("props array") {
            let pname = prop["name"].as_str().expect("prop name");
            let pty = map_type(prop["type"].as_str().expect("prop type"));
            out.push_str(&format!("({:?}, {}), ", pname, pty));
        }
        out.push_str("]),\n");
    }
    out.push_str("];\n");

    let dest = PathBuf::from(env::var("OUT_DIR").unwrap()).join("design_elements.rs");
    fs::write(&dest, out).expect("write generated design_elements.rs");
}
