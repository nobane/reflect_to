use crate::{
    apply_rename_rule,
    type_registry::{RegistryError, TypeRegistry},
    DataKind, EnumInfo, EnumRepresentation, FieldInfo, FieldsInfo, PrimitiveType, Reflection,
    RenameRuleValue, StructInfo, TypeAttributes, TypeInfo, TypeRef, VariantInfo,
};

use std::{any::TypeId, collections::BTreeMap, path::Path};

/// Generates TypeScript definitions from Rust types implementing `Reflection`.
pub struct ToTypescript {
    /// Shared type registry for dependency tracking and resolution
    registry: TypeRegistry,

    /// Whether to remove common module prefixes from paths during generation
    strip_common_prefixes: bool,
}

impl Default for ToTypescript {
    fn default() -> Self {
        Self {
            registry: Default::default(),
            strip_common_prefixes: true,
        }
    }
}

impl ToTypescript {
    /// Creates a new TypeScript generator that will strip common module prefixes.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new TypeScript generator that will keep common module prefixes.
    pub fn with_common_prefixes() -> Self {
        Self {
            strip_common_prefixes: false,
            ..Default::default()
        }
    }

    /// Adds a Rust type `T` (which must implement `Reflection` and be `'static`)
    /// to the generator. Recursively adds all dependent types.
    pub fn add_type<T>(&mut self) -> Result<(), RegistryError>
    where
        T: Reflection + 'static,
    {
        self.registry.add_type::<T>()
    }

    /// Generates the final TypeScript code string.
    pub fn generate(&self) -> Result<String, RegistryError> {
        let mut output = String::new();
        output.push_str("// DO NOT EDIT: Auto-generated TypeScript definitions\n");
        output.push_str("/* eslint-disable @typescript-eslint/no-namespace */\n\n");

        // First, get all module paths and strip crate:: prefixes
        let mut module_paths = Vec::new();
        for type_id in &self.registry.visit_order {
            if let Some(info) = self.registry.type_info.get(type_id) {
                let stripped_path = TypeRegistry::strip_crate_prefix(&info.module_path).to_string();
                if !stripped_path.is_empty() && !module_paths.contains(&stripped_path) {
                    module_paths.push(stripped_path);
                }
            }
        }

        // Detect common prefix if setting enabled
        let common_prefix = if self.strip_common_prefixes {
            TypeRegistry::detect_common_prefix(&module_paths)
        } else {
            None
        };

        // Log the common prefix for debugging
        if let Some(ref prefix) = common_prefix {
            eprintln!("Detected common prefix: '{}'", prefix);
        } else {
            println!("{module_paths:?} {}", self.strip_common_prefixes);
        }

        // Now process types with prefixes handled
        let mut modules: BTreeMap<String, Vec<TypeId>> = BTreeMap::new();
        for type_id in &self.registry.visit_order {
            if let Some(info) = self.registry.type_info.get(type_id) {
                // Process module path according to settings
                let mut ts_module_path =
                    TypeRegistry::strip_crate_prefix(&info.module_path).to_string();

                // Strip common prefix if found
                if let Some(ref prefix) = common_prefix {
                    if ts_module_path.starts_with(prefix) {
                        ts_module_path = ts_module_path[prefix.len()..].to_string();
                        // Remove leading :: if present
                        if ts_module_path.starts_with("::") {
                            ts_module_path = ts_module_path[2..].to_string();
                        }
                    }
                }

                // Convert to TS module path format
                let ts_module_path = ts_module_path.replace("::", ".");

                // Add to modules
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
                let info = self.registry.try_get_type(&type_id)?;
                match self.generate_ts_for_type(info, &common_prefix) {
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
                        eprintln!("Error generating TS for type '{}': {e}", info.name);
                        let error_comment =
                            format!("// Error generating type {}: {e}\n", info.name);
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
    fn generate_ts_for_type(
        &self,
        info: &TypeInfo,
        common_prefix: &Option<String>,
    ) -> Result<String, RegistryError> {
        let ts_name = &info.name;
        match &info.data {
            DataKind::Struct(s_info) => {
                self.generate_struct_ts(ts_name, s_info, &info.attributes, common_prefix)
            }
            DataKind::Enum(e_info) => {
                self.generate_enum_ts(ts_name, e_info, &info.attributes, common_prefix)
            }
        }
    }

    fn generate_struct_ts(
        &self,
        ts_name: &str,
        s_info: &StructInfo,
        type_attrs: &TypeAttributes,
        common_prefix: &Option<String>,
    ) -> Result<String, RegistryError> {
        let rename_all_rule = &type_attrs.rename_all;
        match &s_info.fields {
            FieldsInfo::Named(fields) => {
                let mut field_defs = String::new();
                for field in fields {
                    if field.attributes.skip || field.attributes.skip_serializing {
                        continue;
                    }
                    let field_name_rust = field.name.as_ref().ok_or_else(|| {
                        RegistryError::GenerationFailed(
                            ts_name.to_string(),
                            "Named field missing name".into(),
                        )
                    })?;
                    let field_type_ts = self.get_ts_type_str(&field.ty, common_prefix)?;

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
                    let inner_type_ts = self.get_ts_type_str(&fields[0].ty, common_prefix)?;
                    Ok(format!("export type {} = {}", ts_name, inner_type_ts))
                } else {
                    let types: Result<Vec<String>, _> = fields
                        .iter()
                        .map(|f| self.get_ts_type_str(&f.ty, common_prefix))
                        .collect();
                    Ok(format!("export type {} = [{}]", ts_name, types?.join(", ")))
                }
            }
            FieldsInfo::Unit => Ok(format!("export type {} = null", ts_name)),
        }
    }

    fn generate_enum_ts(
        &self,
        ts_name: &str,
        e_info: &EnumInfo,
        type_attrs: &TypeAttributes,
        common_prefix: &Option<String>,
    ) -> Result<String, RegistryError> {
        let rename_all_rule = &type_attrs.rename_all;
        let representation = &e_info.representation;
        let is_simple_c_like = e_info
            .variants
            .iter()
            .all(|v| matches!(v.fields, FieldsInfo::Unit));

        if is_simple_c_like && *representation == EnumRepresentation::ExternallyTagged {
            let results: Result<Vec<Option<String>>, RegistryError> = e_info
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
            let variant_defs_results: Result<Vec<String>, RegistryError> = e_info
                .variants
                .iter()
                .filter_map(|variant| {
                    // Filter out skipped variants first
                    if variant.attributes.skip || variant.attributes.skip_serializing {
                        return None;
                    }
                    Some(self.generate_variant_def_ts(
                        variant,
                        representation,
                        rename_all_rule,
                        common_prefix,
                    ))
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
        common_prefix: &Option<String>,
    ) -> Result<String, RegistryError> {
        let variant_name_rust = &variant.name;
        // Handle temporary String correctly
        let variant_rename = variant.attributes.rename.clone();
        let rule_rename = apply_rename_rule(variant_name_rust, enum_rename_all_rule);
        let ts_variant_name = variant_rename
            .or(rule_rename)
            .unwrap_or_else(|| variant_name_rust.to_string());

        let data_ts_type =
            self.get_variant_data_ts_type(&variant.fields, enum_rename_all_rule, common_prefix)?;

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
        common_prefix: &Option<String>,
    ) -> Result<String, RegistryError> {
        match fields {
            FieldsInfo::Named(named_fields) => {
                let field_defs: Result<Vec<String>, RegistryError> = named_fields
                    .iter()
                    .filter(|f| !f.attributes.skip && !f.attributes.skip_serializing)
                    .map(|field| {
                        let field_name_rust = field.name.as_ref().unwrap();
                        let field_type_ts = self.get_ts_type_str(&field.ty, common_prefix)?;
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
                    self.get_ts_type_str(&active_fields[0].ty, common_prefix)
                } else {
                    let types: Result<Vec<String>, _> = active_fields
                        .iter()
                        .map(|f| self.get_ts_type_str(&f.ty, common_prefix))
                        .collect();
                    Ok(format!("[{}]", types?.join(", ")))
                }
            }
            FieldsInfo::Unit => Ok("null".to_string()),
        }
    }

    fn get_ts_type_str(
        &self,
        type_ref: &TypeRef,
        common_prefix: &Option<String>,
    ) -> Result<String, RegistryError> {
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
                    if let Some(info) = self.registry.type_info.get(tid) {
                        // Process the module path (strip crate:: prefix and common prefix)
                        let mut module_path =
                            TypeRegistry::strip_crate_prefix(&info.module_path).to_string();

                        // Strip common prefix if found
                        if let Some(ref prefix) = common_prefix {
                            if module_path.starts_with(prefix) {
                                module_path = module_path[prefix.len()..].to_string();
                                // Remove leading :: if present
                                if module_path.starts_with("::") {
                                    module_path = module_path[2..].to_string();
                                }
                            }
                        }

                        let ts_module = module_path.replace("::", ".");
                        return if ts_module.is_empty() {
                            Ok(info.name.clone())
                        } else {
                            Ok(format!("{ts_module}.{}", info.name))
                        };
                    }
                }

                // Otherwise, fall back to path resolution based on name
                if *is_reflect_to {
                    let type_name_part = if let Some(last_colon) = path.rfind("::") {
                        &path[last_colon + 2..]
                    } else {
                        path
                    };

                    // Try to find the type by name
                    if let Some(type_id) = self.registry.find_type_by_name(type_name_part) {
                        if let Some(info) = self.registry.get_type(&type_id) {
                            // Process the module path (strip crate:: prefix and common prefix)
                            let mut module_path =
                                TypeRegistry::strip_crate_prefix(&info.module_path).to_string();

                            // Strip common prefix if found
                            if let Some(ref prefix) = common_prefix {
                                if module_path.starts_with(prefix) {
                                    module_path = module_path[prefix.len()..].to_string();
                                    // Remove leading :: if present
                                    if module_path.starts_with("::") {
                                        module_path = module_path[2..].to_string();
                                    }
                                }
                            }

                            let ts_module = module_path.replace("::", ".");
                            return if ts_module.is_empty() {
                                Ok(info.name.clone())
                            } else {
                                Ok(format!("{ts_module}.{}", info.name))
                            };
                        }
                    }

                    // If type not found, return an error
                    Err(RegistryError::Resolution(format!(
                        "Type {path:?} not found in registry",
                    )))
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
                    let elem_types: Result<Vec<String>, _> = elems
                        .iter()
                        .map(|t| self.get_ts_type_str(t, common_prefix))
                        .collect();
                    Ok(format!("[{}]", elem_types?.join(", ")))
                }
            }
            TypeRef::Array { elem_type, .. } | TypeRef::Vec(elem_type) => Ok(format!(
                "{}[]",
                self.get_ts_type_str(elem_type, common_prefix)?
            )),
            TypeRef::Option(inner) => Ok(format!(
                "{} | null",
                self.get_ts_type_str(inner, common_prefix)?
            )),
            TypeRef::Result { ok_type, .. } => Ok(format!(
                "{} | null",
                self.get_ts_type_str(ok_type, common_prefix)?
            )),
            TypeRef::Box(inner) => self.get_ts_type_str(inner, common_prefix),
            TypeRef::Map {
                key_type,
                value_type,
            } => {
                let key_ts = self.get_ts_type_str(key_type, common_prefix)?;
                let value_ts = self.get_ts_type_str(value_type, common_prefix)?;
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

    /// Writes the generated TypeScript definitions to the specified file path.
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), RegistryError> {
        let output = self.generate()?;
        self.registry.write_to_file(path, &output)
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
