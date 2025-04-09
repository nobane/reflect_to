use reflect_to::{Reflect, ToTypescript};
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
#[serde(rename_all = "camelCase")]
struct ComplexStruct {
    simple: SimpleStruct,
    #[serde(rename = "status_list")]
    statuses: Vec<SimpleEnum>,
    optional_path: Option<PathBuf>, // PathBuf -> string
    #[serde(skip_serializing_if = "Option::is_none")]
    maybe_number: Option<f64>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut generator = ToTypescript::default();

    generator.add_type::<ComplexStruct>()?;

    let ts_code = generator.generate()?;

    println!("{ts_code}");

    Ok(())
}
