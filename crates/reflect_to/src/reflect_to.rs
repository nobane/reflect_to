use std::{any::TypeId, collections::BTreeMap, error::Error};

/// Trait for types that can provide reflection metadata about themselves.
///
/// This trait is intended to be derived using `#[derive(Reflection)]` from the `reflect_to` crate.
/// Manually implementing this trait is complex and generally discouraged.
pub trait Reflection {
    /// Returns the reflection metadata for this type.
    fn reflect() -> TypeInfo;

    /// Returns the TypeId for this type.
    fn type_id() -> TypeId
    where
        Self: 'static,
    {
        TypeId::of::<Self>()
    }
}

/// Trait for registry implementations that can store types
pub trait TypeRegistry {
    /// Register a type in the registry with its type information
    fn register_type(&mut self, type_id: TypeId, info: TypeInfo) -> Result<(), Box<dyn Error>>;
}

/// Top-level information about a reflected type.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    /// The simple name of the type (e.g., "User").
    pub name: String,
    /// The Rust module path where the type is defined (e.g., "crate::models::users").
    /// Used by generators to determine namespacing/module structure.
    pub module_path: String,
    /// The kind of data this type represents (Struct, Enum, etc.).
    pub data: DataKind,
    /// Serde attributes relevant at the type level.
    pub attributes: TypeAttributes,
    /// Documentation comments associated with the type.
    pub docs: Vec<String>,
    /// List of functions that can add dependencies of this type to a registry
    pub dependencies: Vec<DependencyAdder>,
}

/// Function type for adding a dependency to a registry
type DependencyAdder = fn(&mut dyn TypeRegistry) -> Result<(), Box<dyn Error>>;

/// Represents the different kinds of data structures we can reflect.
#[derive(Debug, Clone, PartialEq)]
pub enum DataKind {
    Struct(StructInfo),
    Enum(EnumInfo),
}

/// Information specific to a struct.
#[derive(Debug, Clone, PartialEq)]
pub struct StructInfo {
    /// The kind of fields the struct has.
    pub fields: FieldsInfo,
}

/// Information specific to an enum.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumInfo {
    /// The variants of the enum.
    pub variants: Vec<VariantInfo>,
    /// How the enum is represented in serialization (influences target generation).
    pub representation: EnumRepresentation,
}

/// Represents the fields within a struct or enum variant.
#[derive(Debug, Clone, PartialEq)]
pub enum FieldsInfo {
    /// Named fields like `struct Foo { bar: i32 }`.
    Named(Vec<FieldInfo>),
    /// Unnamed fields like `struct Foo(i32, String)`.
    Unnamed(Vec<FieldInfo>), // FieldInfo name will be None
    /// No fields, like `struct Unit;` or `enum Foo::Bar`.
    Unit,
}

/// Information about a single field in a struct or enum variant.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldInfo {
    /// The name of the field (None for unnamed fields).
    pub name: Option<String>,
    /// The type of the field.
    pub ty: TypeRef,
    /// Serde attributes relevant to the field.
    pub attributes: FieldAttributes,
    /// Documentation comments associated with the field.
    pub docs: Vec<String>,
    /// Index for unnamed fields (0, 1, 2...). Set to 0 for named fields for simplicity.
    pub index: usize,
}

/// Information about a single variant in an enum.
#[derive(Debug, Clone, PartialEq)]
pub struct VariantInfo {
    /// The name of the variant (e.g., "Success").
    pub name: String,
    /// The fields associated with the variant.
    pub fields: FieldsInfo,
    /// Serde attributes relevant to the variant.
    pub attributes: VariantAttributes,
    /// Documentation comments associated with the variant.
    pub docs: Vec<String>,
}

/// Represents a reference to a type, crucial for dependency tracking.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeRef {
    /// A primitive Rust type with a known direct mapping in target languages.
    Primitive(PrimitiveType),
    /// A path to another Rust type (struct, enum, or external type).
    Path {
        /// The fully qualified path string (e.g., "std::string::String", "crate::models::User", "Vec").
        path: String,
        /// Generic arguments, if any
        generic_args: Vec<TypeRef>,
        /// Indicates if this path corresponds to a type that implements `Reflection`.
        is_reflect_to: bool,
        /// Type ID for reflected types if known at compile time
        #[doc(hidden)]
        type_id: Option<TypeId>,
    },
    /// A tuple type like `(String, i32)`.
    Tuple(Vec<TypeRef>),
    /// A fixed-size array type like `[u8; 32]`. Target languages often treat this like a Vec/List.
    Array { elem_type: Box<TypeRef>, len: usize },
    /// Represents `Option<T>`.
    Option(Box<TypeRef>),
    /// Represents `Result<T, E>`. Generators might only care about T.
    Result {
        ok_type: Box<TypeRef>,
        err_type: Box<TypeRef>,
    },
    /// Represents `Box<T>`. Generators typically treat this as just `T`.
    Box(Box<TypeRef>),
    /// Represents `Vec<T>` or slice `&[T]`.
    Vec(Box<TypeRef>),
    /// Represents `HashMap<K, V>` or `BTreeMap<K, V>`.
    Map {
        key_type: Box<TypeRef>,
        value_type: Box<TypeRef>,
    },
    /// Represents the unit type `()`.
    Unit,
    /// Represents the never type `!`.
    Never,
    /// A type that couldn't be fully resolved or is not directly supported.
    /// Generators should likely map this to `any` or `unknown`.
    Unsupported(String), // Contains the `syn::Type` rendered as string
}

/// Enumeration of common Rust primitive types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    String,
    Str,
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    F32,
    F64,
    PathBuf,
    Char,
}

/// Serde attributes applicable at the type level (struct/enum).
#[derive(Debug, Clone, PartialEq, Default)]
pub struct TypeAttributes {
    pub rename: Option<String>,
    pub rename_all: Option<RenameRuleValue>,
    pub other: BTreeMap<String, String>, // For unrecognized serde attrs or others
}

/// Serde attributes applicable at the field level.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct FieldAttributes {
    pub rename: Option<String>,
    pub skip: bool,
    pub skip_serializing: bool,
    pub skip_deserializing: bool,
    pub skip_serializing_if: Option<String>, // Store the path string
    pub flatten: bool,
    pub other: BTreeMap<String, String>,
}

/// Serde attributes applicable at the variant level.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct VariantAttributes {
    pub rename: Option<String>,
    pub skip: bool,
    pub skip_serializing: bool,
    pub skip_deserializing: bool,
    pub other: BTreeMap<String, String>,
}

/// Represents the different Serde enum tagging strategies.
#[derive(Debug, Clone, PartialEq)]
pub enum EnumRepresentation {
    ExternallyTagged,
    InternallyTagged { tag: String },
    AdjacentlyTagged { tag: String, content: String },
    Untagged,
}

/// Represents the different Serde rename_all strategies (as strings).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RenameRuleValue {
    Rule(String), // "lowercase", "UPPERCASE", "PascalCase", ...
}

/// Converts a string to PascalCase.
/// "foo_bar" -> "FooBar", "foo-bar" -> "FooBar"
pub fn to_pascal_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut capitalize_next = true;
    for c in s.chars() {
        if c == '_' || c == '-' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }
    result
}

/// Converts a string to snake\_case.
/// "FooBar" -> "foo_bar", "fooBar" -> "foo_bar", "FOO_BAR" -> "foo_bar"
pub fn to_snake_case(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }
    let mut output = String::with_capacity(s.len() + s.len() / 3); // Estimate
    let mut chars = s.chars().peekable();
    let mut last_char_was_upper = false;

    while let Some(c) = chars.next() {
        if c.is_uppercase() {
            // Check if we need to insert an underscore
            if !output.is_empty() && !output.ends_with('_') {
                // Insert underscore if previous char was not uppercase (handles acronyms like HTTP)
                // or if the next char is lowercase (handles FooBar)
                let next_is_lower = chars.peek().is_some_and(|next| next.is_lowercase());
                if !last_char_was_upper || next_is_lower {
                    output.push('_');
                }
            }
            output.push(c.to_ascii_lowercase());
            last_char_was_upper = true;
        } else if c == '-' || c == '_' || c.is_whitespace() {
            // Replace separators with underscore, avoiding duplicates
            if !output.is_empty() && !output.ends_with('_') {
                output.push('_');
            }
            last_char_was_upper = false;
        } else {
            // Lowercase letters or digits
            output.push(c);
            last_char_was_upper = false;
        }
    }
    // Trim leading/trailing underscores that might occur if input starts/ends with separator
    output.trim_matches('_').to_string()
}

/// Converts a string to SCREAMING_SNAKE_CASE.
/// "fooBar" -> "FOO_BAR"
pub fn to_screaming_snake_case(s: &str) -> String {
    to_snake_case(s).to_ascii_uppercase()
}

/// Converts a string to kebab-case.
/// "fooBar" -> "foo-bar"
pub fn to_kebab_case(s: &str) -> String {
    to_snake_case(s).replace('_', "-")
}

/// Converts a string to camelCase.
/// "foo_bar" -> "fooBar", "FooBar" -> "fooBar"
pub fn to_camel_case(s: &str) -> String {
    let pascal = to_pascal_case(s);
    if let Some(first) = pascal.chars().next() {
        format!("{}{}", first.to_lowercase(), &pascal[first.len_utf8()..])
    } else {
        pascal // Empty string
    }
}

/// Applies a rename rule to an original string based on the `RenameRuleValue`.
pub fn apply_rename_rule(original: &str, rule: &Option<RenameRuleValue>) -> Option<String> {
    match rule {
        Some(RenameRuleValue::Rule(rule_str)) => match rule_str.as_str() {
            "lowercase" => Some(original.to_lowercase()),
            "UPPERCASE" => Some(original.to_uppercase()),
            "PascalCase" => Some(to_pascal_case(original)),
            "camelCase" => Some(to_camel_case(original)),
            "snake_case" => Some(to_snake_case(original)),
            "SCREAMING_SNAKE_CASE" => Some(to_screaming_snake_case(original)),
            "kebab-case" => Some(to_kebab_case(original)),
            _ => {
                eprintln!("Warning: Unknown rename_all rule '{}'", rule_str);
                None
            }
        },
        None => None,
    }
}
