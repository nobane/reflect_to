use std::collections::BTreeMap;

// --- Core Trait ---

/// Trait for types that can provide reflection metadata about themselves.
///
/// This trait is intended to be derived using `#[derive(ReflectTo)]` from the `reflect_to` crate.
/// Manually implementing this trait is complex and generally discouraged.
pub trait ReflectTo {
    /// Returns the reflection metadata for this type.
    fn reflect() -> TypeInfo;
}

// --- Metadata Structures ---
// (These are copied verbatim from the previous `reflect_to/src/lib.rs`,
//  as they don't depend on the macro itself)

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
}

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
        /// Generators use this to identify the type.
        path: String,
        /// Generic arguments, if any (e.g., for `Vec<String>`, this would contain `TypeRef::Primitive(PrimitiveType::String)`).
        generic_args: Vec<TypeRef>,
        /// Indicates if this path corresponds to a type that implements `ReflectTo`.
        /// Generators use this to know if they can recursively call `reflect()` for this type.
        is_reflect_to: bool, // The macro will determine this heuristically or via trait bounds later
    },
    /// A tuple type like `(String, i32)`.
    Tuple(Vec<TypeRef>),
    /// A fixed-size array type like `[u8; 32]`. Target languages often treat this like a Vec/List.
    Array {
        elem_type: Box<TypeRef>,
        len: usize, // We might not always capture this from syn easily, maybe just store elem_type
    },
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
    ExternallyTagged, // Default
    InternallyTagged { tag: String },
    AdjacentlyTagged { tag: String, content: String },
    Untagged,
}

/// Represents the different Serde rename_all strategies (as strings).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RenameRuleValue {
    Rule(String), // "lowercase", "UPPERCASE", "PascalCase", ...
    None,
}
