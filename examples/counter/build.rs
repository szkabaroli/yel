use std::env;
use std::path::PathBuf;
use std::process::Command;
use std::fs::File;
use std::io::Write;

fn main() {
    println!("cargo:rerun-if-changed=counter.yel");

    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let counter_yel = PathBuf::from(&manifest_dir).join("counter.yel");
    
    let out_dir = env::var("OUT_DIR").unwrap();
    let wasm_output = PathBuf::from(&out_dir).join("counter.wasm");
    let wit_output = PathBuf::from(&out_dir).join("counter.wit");

    // Try to find yelc in PATH
    let yelc = which::which("yelc")
        .expect("yelc not found in PATH. Please install yelc: cargo install --path crates/yelc");

    // Compile to WASM
    let wasm_output_bytes = Command::new(&yelc)
        .arg("compile")
        .arg(&counter_yel)
        .arg("-o")
        .arg("wasm")
        .output()
        .expect("Failed to run yelc");

    if !wasm_output_bytes.status.success() {
        eprintln!("yelc stderr: {}", String::from_utf8_lossy(&wasm_output_bytes.stderr));
        panic!("yelc WASM compilation failed");
    }

    File::create(&wasm_output)
        .and_then(|mut f| f.write_all(&wasm_output_bytes.stdout))
        .expect("Failed to write WASM output");

    // Compile to WIT
    let wit_output_bytes = Command::new(&yelc)
        .arg("compile")
        .arg(&counter_yel)
        .arg("-o")
        .arg("wit")
        .output()
        .expect("Failed to run yelc");

    if !wit_output_bytes.status.success() {
        eprintln!("yelc stderr: {}", String::from_utf8_lossy(&wit_output_bytes.stderr));
        panic!("yelc WIT generation failed");
    }

    File::create(&wit_output)
        .and_then(|mut f| f.write_all(&wit_output_bytes.stdout))
        .expect("Failed to write WIT output");

    // Copy files to wit folder and project root for wac composition
    let wit_dir = PathBuf::from(&manifest_dir).join("wit");
    let wit_copy = wit_dir.join("counter.wit");
    let wasm_copy = PathBuf::from(&manifest_dir).join("build/counter-ui.wasm");
    
    // Ensure wit directory exists
    std::fs::create_dir_all(&wit_dir)
        .expect("Failed to create wit directory");
    
    std::fs::copy(&wit_output, &wit_copy)
        .expect("Failed to copy WIT file to wit/");
    std::fs::copy(&wasm_output, &wasm_copy)
        .expect("Failed to copy WASM file");

    println!("cargo:warning=Generated WASM component at: {}", wasm_output.display());
    println!("cargo:warning=Generated WIT interface at: {}", wit_output.display());
    println!("cargo:warning=WIT file copied to: {}", wit_copy.display());
    println!("cargo:warning=WASM file copied to: {}", wasm_copy.display());
    println!("cargo:warning=Use 'make compose' to create the final counter-app.wasm");
}
