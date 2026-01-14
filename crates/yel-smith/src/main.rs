//! CLI for generating random Yel source files.
//!
//! Usage:
//!   yel-smith < random_bytes > output.yel
//!   yel-smith --seed 12345 > output.yel

use arbitrary::Unstructured;
use yel_smith::{Config, YelModule};
use std::io::{self, Read, Write};

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let data: Vec<u8> = if args.len() > 2 && args[1] == "--seed" {
        // Generate deterministic data from seed
        let seed: u64 = args[2].parse()?;
        generate_from_seed(seed)
    } else {
        // Read from stdin
        let mut data = Vec::new();
        io::stdin().read_to_end(&mut data)?;
        if data.is_empty() {
            // Generate some random data if stdin is empty
            generate_from_seed(std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)?
                .as_nanos() as u64)
        } else {
            data
        }
    };

    let mut u = Unstructured::new(&data);
    let config = Config::default();

    match YelModule::arbitrary_with_config(&mut u, &config) {
        Ok(module) => {
            let source = module.to_source();
            io::stdout().write_all(source.as_bytes())?;
        }
        Err(e) => {
            eprintln!("Generation failed: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}

/// Generate pseudo-random bytes from a seed using a simple LCG.
fn generate_from_seed(seed: u64) -> Vec<u8> {
    let mut state = seed;
    let mut data = Vec::with_capacity(4096);

    for _ in 0..4096 {
        // Linear congruential generator
        state = state.wrapping_mul(6364136223846793005).wrapping_add(1);
        data.push((state >> 33) as u8);
    }

    data
}
