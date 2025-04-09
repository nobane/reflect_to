use crate::{
    apply_rename_rule, to_pascal_case, to_screaming_snake_case, to_snake_case, DataKind, EnumInfo,
    EnumRepresentation, FieldInfo, FieldsInfo, PrimitiveType, Reflection, RenameRuleValue,
    StructInfo, TypeAttributes, TypeInfo, TypeRef, TypeRegistry, VariantInfo,
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
pub enum ToPythonError {
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    #[error("Type resolution error: {0}")]
    Resolution(String),
    #[error("Ambiguous type name '{0}'. Found in modules: [{1}].")]
    AmbiguousTypeName(String, String),
    #[error("Generation failed for type '{0}': {1}")]
    GenerationFailed(String, String),
    #[error("Type with ID {0:?} not found in registry.")]
    TypeNotFound(TypeId),
    #[error("Unsupported feature for Python generation: {0}")]
    Unsupported(String),
    #[error("Dependency Error: {0}")]
    Dependency(String),
}

// --- Python Generator ---

/// Generates Python type stub (`.pyi`) files from Rust types implementing `Reflection`.
#[derive(Default)]
pub struct ToPython {
    /// Stores reflection info keyed by TypeId.
    type_map: HashMap<TypeId, TypeInfo>,
    /// Maps a (Python Module Path, Python Type Name) pair to the TypeId.
    name_registry: BTreeMap<(String, String), TypeId>,
    /// Tracks types currently being processed in `add_type`.
    visited_type_ids: HashSet<TypeId>,
    /// Maintains insertion order.
    type_id_visit_order: Vec<TypeId>,
}

// Implement TypeRegistry trait for recursive dependency handling
impl TypeRegistry for ToPython {
    fn register_type(&mut self, type_id: TypeId, info: TypeInfo) -> Result<(), Box<dyn Error>> {
        // Skip if already registered or being processed (avoid cycles)
        if self.type_map.contains_key(&type_id) || self.visited_type_ids.contains(&type_id) {
            return Ok(());
        }

        // Mark as processing to detect cycles
        self.visited_type_ids.insert(type_id);

        // Register the type first
        let py_module_path = info
            .module_path
            .strip_prefix("crate::")
            .unwrap_or(&info.module_path)
            .replace("::", ".");
        let py_name = info.name.clone();
        let name_key = (py_module_path, py_name);

        // Check for name collisions
        if let Some(existing_id) = self.name_registry.get(&name_key) {
            if *existing_id != type_id {
                eprintln!(
                    "Warning: Python type name collision for '{}' in module '{}'. Overwriting.",
                    info.name, info.module_path
                );
            }
        }

        // Register the type
        self.name_registry.insert(name_key, type_id);
        self.type_map.insert(type_id, info.clone());
        self.type_id_visit_order.push(type_id);

        // Process dependencies recursively
        for add_dependency in &info.dependencies {
            add_dependency(self)?;
        }

        // Done processing this type
        self.visited_type_ids.remove(&type_id);

        Ok(())
    }
}

impl ToPython {
    /// Adds a Rust type `T` to the generator.
    pub fn add_type<T>(&mut self) -> Result<(), ToPythonError>
    where
        T: Reflection + 'static,
    {
        let type_id = TypeId::of::<T>();
        let info = T::reflect();

        // Use the TypeRegistry trait implementation to handle this type
        self.register_type(type_id, info)
            .map_err(|e| ToPythonError::Dependency(e.to_string()))
    }

    /// Generates the final Python type stub (`.pyi`) string.
    pub fn generate(&self) -> Result<String, ToPythonError> {
        let mut imports: BTreeMap<String, HashSet<String>> = BTreeMap::new();
        let mut generated_bodies = BTreeMap::new();

        for type_id in &self.type_id_visit_order {
            let info = self
                .type_map
                .get(type_id)
                .ok_or(ToPythonError::TypeNotFound(*type_id))?;
            match self.generate_py_for_type(info, &mut imports) {
                Ok(definition) => {
                    let py_module = info
                        .module_path
                        .strip_prefix("crate::")
                        .unwrap_or(&info.module_path)
                        .replace("::", ".");
                    generated_bodies.insert((py_module, info.name.clone()), definition);
                }
                Err(e) => {
                    eprintln!("Error generating Python for type '{}': {}", info.name, e);
                    let py_module = info
                        .module_path
                        .strip_prefix("crate::")
                        .unwrap_or(&info.module_path)
                        .replace("::", ".");
                    generated_bodies.insert(
                        (py_module, info.name.clone()),
                        format!("# Error generating type {}: {}\n", info.name, e),
                    );
                }
            }
        }

        let mut header = String::new();
        header.push_str("# DO NOT EDIT: Auto-generated Python type stubs\n");
        header.push_str("# flake8: noqa\n# pylint: skip-file\n\n");
        let mut sorted_imports = imports.into_iter().collect::<Vec<_>>();
        sorted_imports.sort_by(|a, b| a.0.cmp(&b.0));
        for (module, items_set) in sorted_imports {
            let mut sorted_items: Vec<String> = items_set.into_iter().collect();
            sorted_items.sort();
            header.push_str(&format!(
                "from {} import {}\n",
                module,
                sorted_items.join(", ")
            ));
        }
        if !header.ends_with("\n\n") && !header.ends_with("# pylint: skip-file\n\n") {
            header.push('\n');
        }

        let mut output_body = String::new();
        let mut current_module = None;
        for ((py_module, _type_name), body) in generated_bodies {
            let module_comment = format!(
                "\n\n# --- Module: {} ---\n",
                if py_module.is_empty() {
                    "<root>"
                } else {
                    &py_module
                }
            );
            if current_module.as_ref() != Some(&py_module) {
                output_body.push_str(&module_comment);
                current_module = Some(py_module.clone());
            } else if !output_body.is_empty() && !output_body.ends_with('\n') {
                output_body.push('\n');
            }
            output_body.push_str(&body);
            if !body.ends_with("\n\n") {
                if body.ends_with('\n') {
                    output_body.push('\n');
                } else {
                    output_body.push_str("\n\n");
                }
            }
        }

        Ok(format!(
            "{}{}\n",
            header,
            output_body.trim_end_matches('\n')
        ))
    }

    /// Generates the Python definition string for a single TypeInfo, collecting imports.
    fn generate_py_for_type(
        &self,
        info: &TypeInfo,
        imports: &mut BTreeMap<String, HashSet<String>>,
    ) -> Result<String, ToPythonError> {
        let py_name = &info.name;
        match &info.data {
            DataKind::Struct(s_info) => {
                self.generate_struct_py(py_name, s_info, &info.attributes, &info.docs, imports)
            }
            DataKind::Enum(e_info) => {
                self.generate_enum_py(py_name, e_info, &info.attributes, &info.docs, imports)
            }
        }
    }

    // --- Struct Generation (as dataclass) ---
    fn generate_struct_py(
        &self,
        py_name: &str,
        s_info: &StructInfo,
        type_attrs: &TypeAttributes,
        docs: &[String],
        imports: &mut BTreeMap<String, HashSet<String>>,
    ) -> Result<String, ToPythonError> {
        add_import(imports, "dataclasses", "dataclass");
        let mut definition = String::new();
        definition.push_str("@dataclass(frozen=True) # Assuming immutable struct\n");
        definition.push_str(&format!("class {}:\n", py_name));
        if !docs.is_empty() {
            definition.push_str("    \"\"\"\n");
            for line in docs {
                definition.push_str(&format!("    {}\n", line));
            }
            definition.push_str("    \"\"\"\n");
        }

        let rename_all_rule = &type_attrs.rename_all;
        let mut field_defs = String::new();
        let mut has_fields = false;

        match &s_info.fields {
            FieldsInfo::Named(fields) => {
                for field in fields {
                    if field.attributes.skip {
                        continue;
                    }
                    has_fields = true;
                    let field_name_rust = field.name.as_ref().ok_or_else(|| {
                        ToPythonError::GenerationFailed(
                            py_name.into(),
                            "Named field missing name".into(),
                        )
                    })?;
                    let field_type_py = self.get_py_type_str(&field.ty, imports)?;

                    // FIX: Handle temporary String from apply_rename_rule correctly
                    let py_field_name = field
                        .attributes
                        .rename
                        .clone() // Clone Option<String>
                        .or_else(|| apply_rename_rule(field_name_rust, rename_all_rule)) // If None, try applying rule -> Option<String>
                        .map(|name| to_snake_case(&name)) // If Some(name), convert it to snake_case -> Option<String>
                        .unwrap_or_else(|| to_snake_case(field_name_rust)); // If still None, snake_case original

                    for line in &field.docs {
                        field_defs.push_str(&format!("    # {}\n", line));
                    }
                    field_defs.push_str(&format!("    {}: {}\n", py_field_name, field_type_py));
                }
            }
            FieldsInfo::Unnamed(fields) => {
                for (i, field) in fields.iter().enumerate() {
                    if field.attributes.skip {
                        continue;
                    }
                    has_fields = true;
                    let field_type_py = self.get_py_type_str(&field.ty, imports)?;
                    for line in &field.docs {
                        field_defs.push_str(&format!("    # Field {}: {}\n", i, line));
                    }
                    field_defs.push_str(&format!("    _{}: {}\n", i, field_type_py));
                }
                if has_fields && docs.is_empty() {
                    definition = format!(
                        "    \"\"\"Note: Original Rust type was a tuple struct.\"\"\"\n{}",
                        definition
                    );
                }
            }
            FieldsInfo::Unit => {
                if docs.is_empty() {
                    definition = format!(
                        "    \"\"\"Note: Original Rust type was a unit struct.\"\"\"\n{}",
                        definition
                    );
                }
            }
        }

        if !has_fields {
            field_defs.push_str("    pass\n");
        }
        definition.push_str(&field_defs);
        if has_fields && !field_defs.ends_with('\n') {
            definition.push('\n');
        }
        Ok(definition)
    }

    // --- Enum Generation ---
    fn generate_enum_py(
        &self,
        py_name: &str,
        e_info: &EnumInfo,
        type_attrs: &TypeAttributes,
        docs: &[String],
        imports: &mut BTreeMap<String, HashSet<String>>,
    ) -> Result<String, ToPythonError> {
        let rename_all_rule = &type_attrs.rename_all;
        let representation = &e_info.representation;
        let is_simple_c_like = e_info
            .variants
            .iter()
            .all(|v| matches!(v.fields, FieldsInfo::Unit));

        if is_simple_c_like && *representation == EnumRepresentation::ExternallyTagged {
            add_import(imports, "enum", "Enum");
            let mut definition = String::new();
            definition.push_str(&format!("class {}(Enum):\n", py_name));
            if !docs.is_empty() {
                definition.push_str("    \"\"\"\n");
                for line in docs {
                    definition.push_str(&format!("    {}\n", line));
                }
                definition.push_str("    \"\"\"\n");
            }

            let mut variants_defs = String::new();
            let mut has_variants = false;
            for variant in &e_info.variants {
                if variant.attributes.skip {
                    continue;
                }
                has_variants = true;
                let variant_name_rust = &variant.name;

                // FIX: Calculate member name (SCREAMING_SNAKE) correctly
                let py_variant_member_name = variant
                    .attributes
                    .rename
                    .clone() // Clone Option<String>
                    .or_else(|| apply_rename_rule(variant_name_rust, rename_all_rule))
                    .map(|name| to_screaming_snake_case(&name)) // Convert to SCREAMING_SNAKE
                    .unwrap_or_else(|| to_screaming_snake_case(variant_name_rust)); // Default conversion

                // FIX: Calculate value (serialized form) correctly
                let py_variant_value = variant
                    .attributes
                    .rename
                    .clone() // Clone Option<String>
                    .or_else(|| apply_rename_rule(variant_name_rust, rename_all_rule))
                    .unwrap_or_else(|| variant_name_rust.to_string()); // Default to original Rust name

                for line in &variant.docs {
                    variants_defs.push_str(&format!("    # {}\n", line));
                }
                // Escape quotes in the value string just in case
                variants_defs.push_str(&format!(
                    "    {} = \"{}\"\n",
                    py_variant_member_name,
                    py_variant_value.replace('"', "\\\"")
                ));
            }
            if !has_variants {
                variants_defs.push_str("    pass\n");
            }
            definition.push_str(&variants_defs);
            if has_variants && !variants_defs.ends_with('\n') {
                definition.push('\n');
            }
            Ok(definition)
        } else {
            add_import(imports, "typing", "Union");
            let mut variant_type_defs = Vec::new();
            let mut union_members = Vec::new();
            for variant in &e_info.variants {
                if variant.attributes.skip {
                    continue;
                }
                let variant_name_rust = &variant.name;

                // FIX: Calculate serialized name correctly
                let serialized_variant_name = variant
                    .attributes
                    .rename
                    .clone() // Clone Option<String>
                    .or_else(|| apply_rename_rule(variant_name_rust, rename_all_rule))
                    .unwrap_or_else(|| variant_name_rust.to_string()); // Default to original Rust name

                let (variant_py_type, maybe_def) = self.generate_py_variant_payload_type(
                    py_name,
                    variant,
                    representation,
                    rename_all_rule,
                    &serialized_variant_name,
                    imports,
                )?;
                if let Some(def) = maybe_def {
                    variant_type_defs.push(def);
                }
                union_members.push(variant_py_type);
            }

            let mut full_def = String::new();
            for def in variant_type_defs {
                full_def.push_str(&def);
                full_def.push_str("\n\n");
            }
            if !docs.is_empty() {
                full_def.push_str("\"\"\"\n");
                for line in docs {
                    full_def.push_str(&format!("{}\n", line));
                }
                full_def.push_str("\"\"\"\n");
            }
            if union_members.is_empty() {
                add_import(imports, "typing", "Never");
                full_def.push_str(&format!(
                    "{} = Never # Original enum had no non-skipped variants\n",
                    py_name
                ));
            } else if union_members.len() == 1 {
                full_def.push_str(&format!("{} = {}\n", py_name, union_members[0]));
            } else {
                full_def.push_str(&format!(
                    "{} = Union[\n    {}\n]\n",
                    py_name,
                    union_members.join(",\n    ")
                ));
            }
            Ok(full_def)
        }
    }

    /// Generates the Python type for a variant's payload, collecting imports.
    fn generate_py_variant_payload_type(
        &self,
        enum_name: &str,
        variant: &VariantInfo,
        representation: &EnumRepresentation,
        enum_rename_all_rule: &Option<RenameRuleValue>,
        serialized_variant_name: &str,
        imports: &mut BTreeMap<String, HashSet<String>>,
    ) -> Result<(String, Option<String>), ToPythonError> {
        let variant_name_pascal = to_pascal_case(&variant.name);
        let helper_type_name = format!("{}{}", enum_name, variant_name_pascal);

        match representation {
            EnumRepresentation::ExternallyTagged => {
                add_import(imports, "typing", "Literal");
                match &variant.fields {
                    FieldsInfo::Unit => {
                        Ok((format!("Literal[\"{}\"]", serialized_variant_name), None))
                    }
                    FieldsInfo::Named(_) | FieldsInfo::Unnamed(_) => {
                        add_import(imports, "typing", "Dict");
                        let payload_py_type = self.get_py_variant_data_type(
                            &variant.fields,
                            enum_rename_all_rule,
                            imports,
                        )?;
                        let key_literal = format!("Literal[\"{}\"]", serialized_variant_name);
                        Ok((format!("Dict[{}, {}]", key_literal, payload_py_type), None))
                    }
                }
            }
            EnumRepresentation::Untagged => {
                let payload_py_type =
                    self.get_py_variant_data_type(&variant.fields, enum_rename_all_rule, imports)?;
                Ok((payload_py_type, None))
            }
            EnumRepresentation::InternallyTagged { tag }
            | EnumRepresentation::AdjacentlyTagged { tag, .. } => {
                add_import(imports, "dataclasses", "dataclass");
                add_import(imports, "typing", "Literal");
                let mut helper_def = String::new();
                helper_def.push_str("@dataclass(frozen=True)\n");
                helper_def.push_str(&format!("class {}:\n", helper_type_name));
                if !variant.docs.is_empty() {
                    helper_def.push_str("    \"\"\"\n");
                    for line in &variant.docs {
                        helper_def.push_str(&format!("    {}\n", line));
                    }
                    helper_def.push_str("    \"\"\"\n");
                }
                let tag_field_name = to_snake_case(tag);
                let tag_literal = format!("Literal[\"{}\"]", serialized_variant_name);
                helper_def.push_str(&format!("    {}: {}\n", tag_field_name, tag_literal));
                let mut has_payload_fields = false;
                match &variant.fields {
                    FieldsInfo::Named(fields) => {
                        for field in fields {
                            if field.attributes.skip {
                                continue;
                            }
                            has_payload_fields = true;
                            let field_name_rust = field.name.as_ref().unwrap();
                            let field_type_py = self.get_py_type_str(&field.ty, imports)?;
                            // FIX: Handle temporary String correctly
                            let py_field_name = field
                                .attributes
                                .rename
                                .clone()
                                .or_else(|| {
                                    apply_rename_rule(field_name_rust, enum_rename_all_rule)
                                })
                                .map(|name| to_snake_case(&name))
                                .unwrap_or_else(|| to_snake_case(field_name_rust));
                            for line in &field.docs {
                                helper_def.push_str(&format!("    # {}\n", line));
                            }
                            helper_def
                                .push_str(&format!("    {}: {}\n", py_field_name, field_type_py));
                        }
                    }
                    FieldsInfo::Unnamed(fields) => {
                        for (i, field) in fields.iter().enumerate() {
                            if field.attributes.skip {
                                continue;
                            }
                            has_payload_fields = true;
                            let field_type_py = self.get_py_type_str(&field.ty, imports)?;
                            for line in &field.docs {
                                helper_def.push_str(&format!("    # Field {}: {}\n", i, line));
                            }
                            helper_def.push_str(&format!("    _{}: {}\n", i, field_type_py));
                        }
                    }
                    FieldsInfo::Unit => {}
                }

                let mut is_adjacent = false;
                if let EnumRepresentation::AdjacentlyTagged { content, .. } = representation {
                    is_adjacent = true;
                    let content_field_name = to_snake_case(content);
                    let content_payload_type = self.get_py_variant_data_type(
                        &variant.fields,
                        enum_rename_all_rule,
                        imports,
                    )?;
                    helper_def.push_str(&format!(
                        "    {}: {}\n",
                        content_field_name, content_payload_type
                    ));
                }

                if !has_payload_fields
                    && !is_adjacent
                    && helper_def.lines().count() <= 3 + variant.docs.len()
                {
                    helper_def.push_str("    pass\n");
                }
                if !helper_def.ends_with('\n') {
                    helper_def.push('\n');
                }
                Ok((helper_type_name, Some(helper_def)))
            }
        }
    }

    /// Generates the Python type string for a variant's data payload, collecting imports.
    fn get_py_variant_data_type(
        &self,
        fields: &FieldsInfo,
        _enum_rename_all_rule: &Option<RenameRuleValue>,
        imports: &mut BTreeMap<String, HashSet<String>>,
    ) -> Result<String, ToPythonError> {
        // Note: enum_rename_all_rule is no longer needed here as we simplify to Dict[str, Any] for named fields
        match fields {
            FieldsInfo::Named(named_fields) => {
                if named_fields.iter().any(|f| !f.attributes.skip) {
                    add_import(imports, "typing", "Dict");
                    add_import(imports, "typing", "Any");
                    Ok("Dict[str, Any]".to_string())
                } else {
                    add_import(imports, "typing", "Optional");
                    add_import(imports, "typing", "Any");
                    Ok("Optional[Any]".to_string()) // No non-skipped fields -> None
                }
            }
            FieldsInfo::Unnamed(unnamed_fields) => {
                let active_fields: Vec<&FieldInfo> = unnamed_fields
                    .iter()
                    .filter(|f| !f.attributes.skip)
                    .collect();
                if active_fields.is_empty() {
                    add_import(imports, "typing", "Optional");
                    add_import(imports, "typing", "Any");
                    Ok("Optional[Any]".to_string())
                } else if active_fields.len() == 1 {
                    self.get_py_type_str(&active_fields[0].ty, imports)
                } else {
                    add_import(imports, "typing", "Tuple");
                    let types: Result<Vec<String>, _> = active_fields
                        .iter()
                        .map(|f| self.get_py_type_str(&f.ty, imports))
                        .collect();
                    Ok(format!("Tuple[{}]", types?.join(", "))) // Fixed-size tuple hint
                }
            }
            FieldsInfo::Unit => {
                add_import(imports, "typing", "Optional");
                add_import(imports, "typing", "Any");
                Ok("Optional[Any]".to_string())
            }
        }
    }

    // --- TypeRef to Python String Conversion ---
    fn get_py_type_str(
        &self,
        type_ref: &TypeRef,
        imports: &mut BTreeMap<String, HashSet<String>>,
    ) -> Result<String, ToPythonError> {
        match type_ref {
            TypeRef::Primitive(p) => Ok(map_primitive_to_py(p).to_string()),
            TypeRef::Path {
                path,
                is_reflect_to,
                type_id,
                ..
            } => {
                // If we have the TypeId from the macro, use it directly
                if let Some(tid) = type_id {
                    if let Some(info) = self.type_map.get(tid) {
                        return Ok(info.name.clone());
                    }
                }

                // Otherwise, fall back to path resolution
                if *is_reflect_to {
                    self.resolve_type_path(path)
                } else {
                    add_import(imports, "typing", "Any");
                    Ok("Any".to_string())
                } // Fallback for non-reflected paths
            }
            TypeRef::Tuple(elems) => {
                if elems.is_empty() {
                    add_import(imports, "typing", "Optional");
                    add_import(imports, "typing", "Any");
                    Ok("Optional[Any]".to_string())
                } else {
                    add_import(imports, "typing", "Tuple");
                    let elem_types: Result<Vec<String>, _> = elems
                        .iter()
                        .map(|t| self.get_py_type_str(t, imports))
                        .collect();
                    Ok(format!("Tuple[{}]", elem_types?.join(", ")))
                }
            }
            TypeRef::Array { elem_type, .. } | TypeRef::Vec(elem_type) => {
                add_import(imports, "typing", "List");
                Ok(format!(
                    "List[{}]",
                    self.get_py_type_str(elem_type, imports)?
                ))
            }
            TypeRef::Option(inner) => {
                add_import(imports, "typing", "Optional");
                Ok(format!(
                    "Optional[{}]",
                    self.get_py_type_str(inner, imports)?
                ))
            }
            TypeRef::Result { ok_type, .. } => {
                add_import(imports, "typing", "Optional");
                Ok(format!(
                    "Optional[{}]",
                    self.get_py_type_str(ok_type, imports)?
                ))
            }
            TypeRef::Box(inner) => self.get_py_type_str(inner, imports),
            TypeRef::Map {
                key_type,
                value_type,
            } => {
                add_import(imports, "typing", "Dict");
                Ok(format!(
                    "Dict[{}, {}]",
                    self.get_py_type_str(key_type, imports)?,
                    self.get_py_type_str(value_type, imports)?
                ))
            }
            TypeRef::Unit => {
                add_import(imports, "typing", "Optional");
                add_import(imports, "typing", "Any");
                Ok("Optional[Any]".to_string())
            }
            TypeRef::Never => {
                add_import(imports, "typing", "NoReturn");
                Ok("NoReturn".to_string())
            }
            TypeRef::Unsupported(s) => {
                eprintln!("Warning: Unsupported type '{}', mapping to Any.", s);
                add_import(imports, "typing", "Any");
                Ok("Any".to_string())
            }
        }
    }

    /// Resolves a Rust path string to a Python name (just base name for single file).
    fn resolve_type_path(&self, rust_path_str: &str) -> Result<String, ToPythonError> {
        let (_rust_mod_part, type_name_part) = if let Some(last_colon) = rust_path_str.rfind("::") {
            let mod_path = rust_path_str[..last_colon]
                .strip_prefix("crate::")
                .unwrap_or(&rust_path_str[..last_colon]);
            (mod_path, &rust_path_str[last_colon + 2..])
        } else {
            ("", rust_path_str)
        };
        // We still need to search the registry to ensure the type was added, even if we only use the base name.
        let py_mod_part = _rust_mod_part.replace("::", "."); // Used for lookup key

        let exact_key = (py_mod_part.clone(), type_name_part.to_string());
        if let Some(type_id) = self.name_registry.get(&exact_key) {
            return self.get_base_py_name(*type_id);
        }

        let mut matches = Vec::new();
        for ((_reg_mod, reg_name), type_id) in &self.name_registry {
            if reg_name == type_name_part {
                matches.push(*type_id);
            }
        }

        match matches.len() {
            0 => Err(ToPythonError::Resolution(format!(
                "Python type '{}' ({}.{}) not registered",
                rust_path_str, py_mod_part, type_name_part
            ))),
            1 => self.get_base_py_name(matches[0]), // Found one by name, return its base name
            _ => {
                let possible = matches
                    .iter()
                    .map(|id| self.get_base_py_name(*id).unwrap_or_default())
                    .collect::<Vec<_>>()
                    .join(", ");
                Err(ToPythonError::AmbiguousTypeName(
                    type_name_part.to_string(),
                    possible,
                ))
            }
        }
    }

    /// Gets the base Python name for a registered type.
    fn get_base_py_name(&self, type_id: TypeId) -> Result<String, ToPythonError> {
        let info = self
            .type_map
            .get(&type_id)
            .ok_or(ToPythonError::TypeNotFound(type_id))?;
        Ok(info.name.clone())
    }

    /// Writes the generated Python type stubs to the specified file path.
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), ToPythonError> {
        let output = self.generate()?;
        let mut file = File::create(path)?;
        file.write_all(output.as_bytes())?;
        Ok(())
    }
}

// --- Standalone Helper Functions ---

/// Adds an import to the required imports map.
fn add_import(imports: &mut BTreeMap<String, HashSet<String>>, module: &str, item: &str) {
    imports
        .entry(module.to_string())
        .or_default()
        .insert(item.to_string());
}

/// Helper to map PrimitiveType enum to Python type strings
fn map_primitive_to_py(p: &PrimitiveType) -> &'static str {
    match p {
        PrimitiveType::String
        | PrimitiveType::Str
        | PrimitiveType::PathBuf
        | PrimitiveType::Char => "str",
        PrimitiveType::Bool => "bool",
        PrimitiveType::F32 | PrimitiveType::F64 => "float",
        _ => "int",
    }
}
