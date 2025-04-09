use reflect_to::{Reflect, ToPython};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Reflect, Serialize, Deserialize, Debug)]
struct SimpleStruct {
    field_a: i32,
    field_b: String,
}

#[derive(Reflect, Serialize, Deserialize, Debug)]
enum SimpleEnum {
    One,
    Two(String),
    Three { id: u64, name: String },
}

#[derive(Reflect, Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")] // Python generator aims for snake_case fields by default
struct ComplexStruct {
    simple: SimpleStruct,
    #[serde(rename = "status_list")] // Renaming will be respected -> status_list
    statuses: Vec<SimpleEnum>,
    optional_path: Option<PathBuf>, // PathBuf -> str
    #[serde(skip_serializing_if = "Option::is_none")]
    // skip attribute currently ignored by Py generator
    maybe_number: Option<f64>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut generator = ToPython::default();

    generator.add_type::<ComplexStruct>()?; // Dependencies should be handled if already added

    let py_code = generator.generate()?;

    println!("{py_code}");

    Ok(())
}
