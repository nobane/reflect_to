use __internals::{
    apply_rename_rule, DataKind, EnumInfo, EnumRepresentation, FieldInfo, FieldsInfo,
    PrimitiveType, Reflection, RenameRuleValue, StructInfo, TypeAttributes, TypeInfo, TypeRef,
    TypeRegistry, VariantInfo,
};

use std::{
    any::TypeId,
    collections::{BTreeMap, HashMap, HashSet},
    error::Error,
    fs::File,
    io::{self, Write},
    path::Path,
};
use thiserror::Error;

// --- Error Handling ---
#[derive(Error, Debug)]
pub enum ToTypeScriptError {
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    #[error("Type resolution error: {0}")]
    Resolution(String),
    #[error("Ambiguous type name '{0}'. Found in modules: [{1}]. Consider using more specific imports or paths.")]
    AmbiguousTypeName(String, String),
    #[error("Generation failed for type '{0}': {1}")]
    GenerationFailed(String, String),
    #[error("Type with ID {0:?} not found in registry. Was it added?")]
    TypeNotFound(TypeId),
    #[error("Dependency Error: {0}")]
    Dependency(String),
}

pub type ModuleName = String;
pub type TypeName = String;
pub type TypeKey = (ModuleName, TypeName);

/// Generates TypeScript definitions from Rust types implementing `Reflection`.
#[derive(Default)]
pub struct ToTypescript {
    /// Stores reflection info keyed by TypeId. Using TypeId assumes 'static types.
    type_map: HashMap<TypeId, TypeInfo>,
    /// Maps a (ModulePath, TypeName) pair to the TypeId for lookup during generation.
    type_registry: BTreeMap<TypeKey, TypeId>,
    /// Tracks types currently being processed in `add_type` to detect cycles.
    visited_type_ids: HashSet<TypeId>,
    /// Maintains insertion order for somewhat stable output. Keyed by TypeId.
    type_id_visit_order: Vec<TypeId>,
}

// Implement TypeRegistry trait for recursive dependency handling
impl TypeRegistry for ToTypescript {
    fn register_type(&mut self, type_id: TypeId, info: TypeInfo) -> Result<(), Box<dyn Error>> {
        // Skip if already registered or being processed (avoid cycles)
        if self.type_map.contains_key(&type_id) || self.visited_type_ids.contains(&type_id) {
            return Ok(());
        }

        // Mark as processing
        self.visited_type_ids.insert(type_id);

        // Add to registry
        let ts_module_path = info.module_path.replace("::", ".");
        let ts_name = info.name.clone();
        let name_key = (ts_module_path, ts_name);

        // Check for name collisions
        if let Some(existing_id) = self.type_registry.get(&name_key) {
            if *existing_id != type_id {
                eprintln!(
                    "Warning: TS Type name collision for '{}' in module '{}'. Overwriting.",
                    info.name, info.module_path
                );
            }
        }

        // Register the type first
        self.type_registry.insert(name_key, type_id);
        self.type_map.insert(type_id, info.clone());
        self.type_id_visit_order.push(type_id);

        // Process dependencies recursively
        for add_dependency in &info.dependencies {
            if let Err(e) = add_dependency(self) {
                self.visited_type_ids.remove(&type_id);
                return Err(e);
            }
        }

        // Done processing
        self.visited_type_ids.remove(&type_id);

        Ok(())
    }
}

impl ToTypescript {
    /// Adds a Rust type `T` (which must implement `Reflection` and be `'static`)
    /// to the generator. Recursively adds all dependent types.
    pub fn add_type<T>(&mut self) -> Result<(), ToTypeScriptError>
    where
        T: Reflection + 'static,
    {
        let type_id = TypeId::of::<T>();
        let info = T::reflect();

        // Use the TypeRegistry trait implementation to handle this type
        self.register_type(type_id, info)
            .map_err(|e| ToTypeScriptError::Dependency(e.to_string()))
    }

    /// Generates the final TypeScript code string.
    pub fn generate(&self) -> Result<String, ToTypeScriptError> {
        let mut output = String::new();
        output.push_str("// DO NOT EDIT: Auto-generated TypeScript definitions\n");
        output.push_str("/* eslint-disable @typescript-eslint/no-namespace */\n\n");

        let mut modules: BTreeMap<String, Vec<TypeId>> = BTreeMap::new();
        for type_id in &self.type_id_visit_order {
            if let Some(info) = self.type_map.get(type_id) {
                let ts_module_path = info.module_path.replace("::", ".");
                modules.entry(ts_module_path).or_default().push(*type_id);
            } else {
                eprintln!(
                    "Warning: TypeId {:?} from generation order not found in types map.",
                    type_id
                );
            }
        }

        for (module_path, type_ids_in_module) in modules {
            let is_namespaced = !module_path.is_empty();
            if is_namespaced {
                output.push_str(&format!("export namespace {} {{\n", module_path));
            }

            for type_id in type_ids_in_module {
                let info = self
                    .type_map
                    .get(&type_id)
                    .ok_or(ToTypeScriptError::TypeNotFound(type_id))?;
                match self.generate_ts_for_type(info) {
                    Ok(definition) => {
                        if !info.docs.is_empty() {
                            if is_namespaced {
                                output.push_str("  ");
                            }
                            output.push_str("/**\n");
                            for line in &info.docs {
                                if is_namespaced {
                                    output.push_str("  ");
                                }
                                output.push_str(" * ");
                                output.push_str(line);
                                output.push('\n');
                            }
                            if is_namespaced {
                                output.push_str("  ");
                            }
                            output.push_str(" */\n");
                        }
                        if is_namespaced {
                            for line in definition.lines() {
                                if !line.trim().is_empty() {
                                    output.push_str("  ");
                                }
                                output.push_str(line);
                                output.push('\n');
                            }
                            if !definition.ends_with('\n') {
                                output.push('\n');
                            }
                            output.push('\n');
                        } else {
                            output.push_str(&definition);
                            if !definition.ends_with('\n') {
                                output.push('\n');
                            }
                            output.push('\n');
                        }
                    }
                    Err(e) => {
                        eprintln!("Error generating TS for type '{}': {}", info.name, e);
                        let error_comment =
                            format!("// Error generating type {}: {}\n", info.name, e);
                        if is_namespaced {
                            output.push_str("  ");
                        }
                        output.push_str(&error_comment);
                        if is_namespaced {
                            output.push_str("\n\n");
                        } else {
                            output.push('\n');
                        }
                    }
                }
            }

            if is_namespaced {
                while output.ends_with("\n\n") {
                    output.pop();
                }
                if !output.ends_with('\n') {
                    output.push('\n');
                }
                output.push_str("}\n\n");
            }
        }
        while output.ends_with('\n') {
            output.pop();
        }
        output.push('\n');
        Ok(output)
    }

    /// Generates the TypeScript definition string for a single TypeInfo.
    fn generate_ts_for_type(&self, info: &TypeInfo) -> Result<String, ToTypeScriptError> {
        let ts_name = self.get_local_ts_name(info)?;
        match &info.data {
            DataKind::Struct(s_info) => self.generate_struct_ts(&ts_name, s_info, &info.attributes),
            DataKind::Enum(e_info) => self.generate_enum_ts(&ts_name, e_info, &info.attributes),
        }
    }

    // --- Struct Generation ---
    fn generate_struct_ts(
        &self,
        ts_name: &str,
        s_info: &StructInfo,
        type_attrs: &TypeAttributes,
    ) -> Result<String, ToTypeScriptError> {
        let rename_all_rule = &type_attrs.rename_all;
        match &s_info.fields {
            FieldsInfo::Named(fields) => {
                let mut field_defs = String::new();
                for field in fields {
                    if field.attributes.skip || field.attributes.skip_serializing {
                        continue;
                    }
                    let field_name_rust = field.name.as_ref().ok_or_else(|| {
                        ToTypeScriptError::GenerationFailed(
                            ts_name.to_string(),
                            "Named field missing name".into(),
                        )
                    })?;
                    let field_type_ts = self.get_ts_type_str(&field.ty)?;

                    // Handle temporary String correctly
                    let field_rename = field.attributes.rename.clone(); // Clone Option<String>
                    let rule_rename = apply_rename_rule(field_name_rust, rename_all_rule); // -> Option<String>
                    let ts_field_name = field_rename
                        .or(rule_rename)
                        .unwrap_or_else(|| field_name_rust.to_string()); // Get final name String

                    if !field.docs.is_empty() {
                        field_defs.push_str("  /**\n");
                        for line in &field.docs {
                            field_defs.push_str(&format!("   * {}\n", line));
                        }
                        field_defs.push_str("   */\n");
                    }
                    field_defs.push_str(&format!("  {}: {}\n", ts_field_name, field_type_ts));
                }
                Ok(format!("export interface {} {{\n{}}}", ts_name, field_defs))
            }
            FieldsInfo::Unnamed(fields) => {
                if fields.len() == 1 {
                    let inner_type_ts = self.get_ts_type_str(&fields[0].ty)?;
                    Ok(format!("export type {} = {}", ts_name, inner_type_ts))
                } else {
                    let types: Result<Vec<String>, _> =
                        fields.iter().map(|f| self.get_ts_type_str(&f.ty)).collect();
                    Ok(format!("export type {} = [{}]", ts_name, types?.join(", ")))
                }
            }
            FieldsInfo::Unit => Ok(format!("export type {} = null", ts_name)),
        }
    }

    // --- Enum Generation ---
    fn generate_enum_ts(
        &self,
        ts_name: &str,
        e_info: &EnumInfo,
        type_attrs: &TypeAttributes,
    ) -> Result<String, ToTypeScriptError> {
        let rename_all_rule = &type_attrs.rename_all;
        let representation = &e_info.representation;
        let is_simple_c_like = e_info
            .variants
            .iter()
            .all(|v| matches!(v.fields, FieldsInfo::Unit));

        if is_simple_c_like && *representation == EnumRepresentation::ExternallyTagged {
            let results: Result<Vec<Option<String>>, ToTypeScriptError> = e_info
                .variants
                .iter()
                .map(|variant| {
                    if variant.attributes.skip || variant.attributes.skip_serializing {
                        return Ok(None);
                    }
                    let variant_name_rust = &variant.name;
                    let variant_rename = variant.attributes.rename.clone();
                    let rule_rename = apply_rename_rule(variant_name_rust, rename_all_rule);
                    let ts_variant_name = variant_rename
                        .or(rule_rename)
                        .unwrap_or_else(|| variant_name_rust.to_string());
                    Ok(Some(format!("\"{}\"", ts_variant_name)))
                })
                .collect();
            let variant_strings: Vec<String> = results?.into_iter().flatten().collect();
            Ok(format!(
                "export type {} = {}",
                ts_name,
                if variant_strings.is_empty() {
                    "never".to_string()
                } else {
                    variant_strings.join(" | ")
                }
            ))
        } else {
            // Complex enum generation (discriminated union)
            let variant_defs_results: Result<Vec<String>, ToTypeScriptError> = e_info
                .variants
                .iter()
                .filter_map(|variant| {
                    // Filter out skipped variants first
                    if variant.attributes.skip || variant.attributes.skip_serializing {
                        return None;
                    }
                    Some(self.generate_variant_def_ts(variant, representation, rename_all_rule))
                })
                .collect(); // Collect results for non-skipped variants

            // Handle the Result before checking is_empty
            let variant_defs = variant_defs_results?; // Propagate error or get the Vec

            // Now check if the resulting Vec is empty
            let final_type_body = if variant_defs.is_empty() {
                "never".to_string() // No non-skipped variants, map to never
            } else {
                variant_defs.join("\n  | ")
            };

            Ok(format!("export type {} =\n  {}", ts_name, final_type_body))
        }
    }

    /// Generates the TypeScript definition string for a single enum variant.
    fn generate_variant_def_ts(
        &self,
        variant: &VariantInfo,
        representation: &EnumRepresentation,
        enum_rename_all_rule: &Option<RenameRuleValue>,
    ) -> Result<String, ToTypeScriptError> {
        let variant_name_rust = &variant.name;
        // Handle temporary String correctly
        let variant_rename = variant.attributes.rename.clone();
        let rule_rename = apply_rename_rule(variant_name_rust, enum_rename_all_rule);
        let ts_variant_name = variant_rename
            .or(rule_rename)
            .unwrap_or_else(|| variant_name_rust.to_string());

        let data_ts_type = self.get_variant_data_ts_type(&variant.fields, enum_rename_all_rule)?;

        match representation {
            EnumRepresentation::ExternallyTagged => match variant.fields {
                FieldsInfo::Unit => Ok(format!("\"{}\"", ts_variant_name)),
                _ => Ok(format!("{{ \"{}\": {} }}", ts_variant_name, data_ts_type)),
            },
            EnumRepresentation::InternallyTagged { tag } => {
                let tag_field = format!("\"{}\": \"{}\"", tag, ts_variant_name);
                match variant.fields {
                    FieldsInfo::Unit => Ok(format!("{{ {} }}", tag_field)),
                    _ if data_ts_type.trim_start().starts_with('{')
                        && data_ts_type.trim_end().ends_with('}') =>
                    {
                        let inner_fields = data_ts_type
                            .trim_start_matches('{')
                            .trim_end_matches('}')
                            .trim();
                        Ok(format!("{{ {};\n{} }}", tag_field, inner_fields))
                    }
                    _ => Ok(format!("{{ {} }} & {}", tag_field, data_ts_type)),
                }
            }
            EnumRepresentation::AdjacentlyTagged { tag, content } => {
                let tag_field = format!("\"{}\": \"{}\"", tag, ts_variant_name);
                let content_field = format!("\"{}\": {}", content, data_ts_type);
                Ok(format!("{{ {}; {} }}", tag_field, content_field))
            }
            EnumRepresentation::Untagged => Ok(data_ts_type),
        }
    }

    /// Generates the TypeScript type string for the data payload of a variant.
    fn get_variant_data_ts_type(
        &self,
        fields: &FieldsInfo,
        enum_rename_all_rule: &Option<RenameRuleValue>,
    ) -> Result<String, ToTypeScriptError> {
        match fields {
            FieldsInfo::Named(named_fields) => {
                let field_defs: Result<Vec<String>, ToTypeScriptError> = named_fields
                    .iter()
                    .filter(|f| !f.attributes.skip && !f.attributes.skip_serializing)
                    .map(|field| {
                        let field_name_rust = field.name.as_ref().unwrap();
                        let field_type_ts = self.get_ts_type_str(&field.ty)?;
                        // Handle temporary String correctly
                        let field_rename = field.attributes.rename.clone();
                        let rule_rename = apply_rename_rule(field_name_rust, enum_rename_all_rule);
                        let ts_field_name = field_rename
                            .or(rule_rename)
                            .unwrap_or_else(|| field_name_rust.to_string());

                        let docs = if !field.docs.is_empty() {
                            format!("/** {} */ ", field.docs.join(" "))
                        } else {
                            "".to_string()
                        };
                        Ok(format!("  {}{}: {}", docs, ts_field_name, field_type_ts))
                    })
                    .collect();
                Ok(format!("{{\n{};\n}}", field_defs?.join(";\n")))
            }
            FieldsInfo::Unnamed(unnamed_fields) => {
                let active_fields: Vec<&FieldInfo> = unnamed_fields
                    .iter()
                    .filter(|f| !f.attributes.skip && !f.attributes.skip_serializing)
                    .collect();
                if active_fields.is_empty() {
                    Ok("null".to_string())
                }
                // All fields skipped
                else if active_fields.len() == 1 {
                    self.get_ts_type_str(&active_fields[0].ty)
                } else {
                    let types: Result<Vec<String>, _> = active_fields
                        .iter()
                        .map(|f| self.get_ts_type_str(&f.ty))
                        .collect();
                    Ok(format!("[{}]", types?.join(", ")))
                }
            }
            FieldsInfo::Unit => Ok("null".to_string()),
        }
    }

    // --- TypeRef to TypeScript String Conversion ---
    fn get_ts_type_str(&self, type_ref: &TypeRef) -> Result<String, ToTypeScriptError> {
        match type_ref {
            TypeRef::Primitive(p) => Ok(map_primitive_to_ts(p).to_string()),
            TypeRef::Path {
                path,
                is_reflect_to,
                type_id,
                ..
            } => {
                // If we have the TypeId from the macro, use it directly
                if let Some(tid) = type_id {
                    if let Some(info) = self.type_map.get(tid) {
                        let ts_module = info.module_path.replace("::", ".");
                        if ts_module.is_empty() {
                            return Ok(info.name.clone());
                        } else {
                            return Ok(format!("{}.{}", ts_module, info.name));
                        }
                    }
                }

                // Otherwise, fall back to path resolution
                if *is_reflect_to {
                    self.resolve_type_path(path)
                } else {
                    // Handle known non-reflect-to paths if they slip through
                    eprintln!("Warning: Non-reflect_to path '{}' encountered directly. Mapping to 'any'. Consider using specific TypeRef variants (Option, Vec, etc.).", path);
                    Ok("any".to_string())
                }
            }
            TypeRef::Tuple(elems) => {
                if elems.is_empty() {
                    Ok("null".to_string())
                } else {
                    let elem_types: Result<Vec<String>, _> =
                        elems.iter().map(|t| self.get_ts_type_str(t)).collect();
                    Ok(format!("[{}]", elem_types?.join(", ")))
                }
            }
            TypeRef::Array { elem_type, .. } | TypeRef::Vec(elem_type) => {
                Ok(format!("{}[]", self.get_ts_type_str(elem_type)?))
            }
            TypeRef::Option(inner) => Ok(format!("{} | null", self.get_ts_type_str(inner)?)),
            TypeRef::Result { ok_type, .. } => {
                Ok(format!("{} | null", self.get_ts_type_str(ok_type)?))
            }
            TypeRef::Box(inner) => self.get_ts_type_str(inner),
            TypeRef::Map {
                key_type,
                value_type,
            } => {
                let key_ts = self.get_ts_type_str(key_type)?;
                let value_ts = self.get_ts_type_str(value_type)?;
                if key_ts == "string" || key_ts == "number" {
                    Ok(format!("Record<{}, {}>", key_ts, value_ts))
                } else {
                    eprintln!("Warning: Map key type '{}' is not string or number. Using Record<string, {}>.", key_ts, value_ts);
                    Ok(format!("Record<string, {}>", value_ts))
                }
            }
            TypeRef::Unit => Ok("null".to_string()),
            TypeRef::Never => Ok("never".to_string()),
            TypeRef::Unsupported(s) => {
                eprintln!("Warning: Unsupported type '{}', mapping to any.", s);
                Ok("any".to_string())
            }
        }
    }

    /// Resolves a Rust path string to a fully qualified TS name.
    fn resolve_type_path(&self, rust_path_str: &str) -> Result<String, ToTypeScriptError> {
        let (rust_mod_part, type_name_part) = if let Some(last_colon) = rust_path_str.rfind("::") {
            let mod_path = rust_path_str[..last_colon]
                .strip_prefix("crate::")
                .unwrap_or(&rust_path_str[..last_colon]);
            (mod_path, &rust_path_str[last_colon + 2..])
        } else {
            ("", rust_path_str)
        };
        let ts_mod_part = rust_mod_part.replace("::", ".");

        let exact_key = (ts_mod_part.clone(), type_name_part.to_string());
        if let Some(type_id) = self.type_registry.get(&exact_key) {
            return self.get_fully_qualified_ts_name(*type_id);
        }

        let mut matches = Vec::new();
        for ((_reg_mod, reg_name), type_id) in &self.type_registry {
            if reg_name == type_name_part {
                matches.push(*type_id);
            }
        }

        match matches.len() {
            0 => Err(ToTypeScriptError::Resolution(format!(
                "Type '{}' ({}.{}) not registered",
                rust_path_str, ts_mod_part, type_name_part
            ))),
            1 => self.get_fully_qualified_ts_name(matches[0]),
            _ => {
                let possible = matches
                    .iter()
                    .map(|id| self.get_fully_qualified_ts_name(*id).unwrap_or_default())
                    .collect::<Vec<_>>()
                    .join(", ");
                Err(ToTypeScriptError::AmbiguousTypeName(
                    type_name_part.to_string(),
                    possible,
                ))
            }
        }
    }

    /// Gets the fully qualified TypeScript name for a registered TypeId.
    fn get_fully_qualified_ts_name(&self, type_id: TypeId) -> Result<String, ToTypeScriptError> {
        let info = self
            .type_map
            .get(&type_id)
            .ok_or(ToTypeScriptError::TypeNotFound(type_id))?;
        let ts_module = info.module_path.replace("::", ".");
        if ts_module.is_empty() {
            Ok(info.name.clone())
        } else {
            Ok(format!("{}.{}", ts_module, info.name))
        }
    }

    /// Gets the local TypeScript name for use within its own definition.
    fn get_local_ts_name(&self, info: &TypeInfo) -> Result<String, ToTypeScriptError> {
        Ok(info.name.clone())
    }

    /// Writes the generated TypeScript definitions to the specified file path.
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), ToTypeScriptError> {
        let output = self.generate()?;
        File::create(path)?.write_all(output.as_bytes())?;
        Ok(())
    }
}

// Helper to map PrimitiveType enum to TypeScript type strings
fn map_primitive_to_ts(p: &PrimitiveType) -> &'static str {
    match p {
        PrimitiveType::String
        | PrimitiveType::Str
        | PrimitiveType::PathBuf
        | PrimitiveType::Char => "string",
        PrimitiveType::Bool => "boolean",
        _ => "number", // All integer and float types map to number
    }
}
