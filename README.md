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
use reflect_to::Reflect;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, path::PathBuf};

#[derive(Reflect, Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct User {
    email: String,
    is_active: bool,
    uploaded_files: Vec<UserPost>,
    profile_image: Option<String>,
    settings: UserSettings,
    status: UserStatus,
}

#[derive(Reflect, Serialize, Deserialize, Debug, Clone)]
pub enum UserStatus {
    Offline,
    Online { status: String },
    Unknown(String),
}

#[derive(Reflect, Serialize, Deserialize, Debug, Clone)]
pub struct UserPost {
    post_name: Option<String>,
    contents: Vec<String>,
}

#[derive(Reflect, Serialize, Deserialize, Debug, Clone)]
#[serde(rename = "camelCase")]
pub struct UserSettings {
    theme_path: PathBuf,
    email_notifications: bool,
    #[serde(rename = "custom")]
    custom_settings: HashMap<String, String>,
}
```

the following type information can be generated at runtime (for instance as part of a wasm build process):

```ts
export interface User {
    email: string;
    isActive: boolean;
    uploadedFiles: UserPost[];
    profileImage: string | null;
    settings: UserSettings;
    status: UserStatus;
}

export type UserStatus =
    "Offline"
    | { "Online": {
        status: string;
    } }
    | { "Unknown": string }

export interface UserPost {
    post_name: string | null;
    contents: string[];
}

export interface UserSettings {
    theme_path: string;
    email_notifications: boolean;
    custom: Record<string, string>;
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