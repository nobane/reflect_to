# Reflect To


- Generate type information in other languages based on rust types.
    - Adhere to `serde` serialization behaviors whenever possible.
- Core `reflect_to` crate to use [rtti](https://en.wikipedia.org/wiki/Run-time_type_information) in rust.
- Supported language types:
    - typescript via `to_typescript` crate
    - python via `to_python` crate

# Typescript Example

By using the `Reflect` derive:

```rs
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
```

the following type information can be generated at runtime (for instance as part of a wasm build process):

```ts
export interface SimpleStruct {
    field_a: number;
    field_b: string;
}


export type SimpleEnum =
    "One"
    | { "Two": string }
    | { "Three": {
        id: number;
        name: string;
    } };


export interface ComplexStruct {
    simple: SimpleStruct;
    status_list: SimpleEnum[];
    optionalPath: string | null;
    maybeNumber: number | null;
}
```

# Demo

Run one of the examples via

```
    cargo run --example to_python
```
or

```
    cargo run --example to_typescript
```

# Documentation

For now, the best documentation is the [typescript](crates/to_typescript/examples/to_typescript.rs) and [python](crates/to_python/examples/to_python.rs) examples.