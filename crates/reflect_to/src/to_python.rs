use crate::{
    apply_rename_rule, to_pascal_case, to_screaming_snake_case, to_snake_case,
    type_registry::{RegistryError, TypeRegistry},
    DataKind, EnumInfo, EnumRepresentation, FieldInfo, FieldsInfo, PrimitiveType, Reflection,
    RenameRuleValue, StructInfo, TypeAttributes, TypeInfo, TypeRef, VariantInfo,
};
use std::{
    collections::{BTreeMap, HashSet},
    path::Path,
};

/// Generates Python type stubs from Rust types implementing `Reflection`.
pub struct ToPython {
    /// Shared type registry for dependency tracking and resolution
    registry: TypeRegistry,

    /// Whether to remove common module prefixes from paths during generation
    strip_common_prefixes: bool,
}

impl Default for ToPython {
    fn default() -> Self {
        Self {
            registry: Default::default(),
            strip_common_prefixes: true,
        }
    }
}

impl ToPython {
    /// Creates a new Python generator that will strip common module prefixes.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new Python generator that will keep common module prefixes.
    pub fn with_common_prefixes() -> Self {
        Self {
            strip_common_prefixes: false,
            ..Default::default()
        }
    }

    /// Adds a Rust type `T` to the generator.
    pub fn add_type<T>(&mut self) -> Result<(), RegistryError>
    where
        T: Reflection + 'static,
    {
        self.registry.add_type::<T>()
    }

    /// Generates the final Python type stub (`.pyi`) string.
    pub fn generate(&self) -> Result<String, RegistryError> {
        let mut imports: BTreeMap<String, HashSet<String>> = BTreeMap::new();
        let mut generated_bodies = BTreeMap::new();

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

        for type_id in &self.registry.visit_order {
            let info = self.registry.try_get_type(type_id)?;
            match self.generate_py_for_type(info, &mut imports) {
                Ok(definition) => {
                    // Process module path with common prefix stripping if needed
                    let mut py_module =
                        TypeRegistry::strip_crate_prefix(&info.module_path).to_string();

                    // Strip common prefix if found
                    if let Some(ref prefix) = common_prefix {
                        if py_module.starts_with(prefix) {
                            py_module = py_module[prefix.len()..].to_string();
                        }
                    }

                    // Convert to Python module path format
                    let py_module = py_module.replace("::", ".");

                    generated_bodies.insert((py_module, info.name.clone()), definition);
                }
                Err(e) => {
                    eprintln!("Error generating Python for type '{}': {}", info.name, e);
                    let mut py_module =
                        TypeRegistry::strip_crate_prefix(&info.module_path).to_string();

                    // Strip common prefix if found
                    if let Some(ref prefix) = common_prefix {
                        if py_module.starts_with(prefix) {
                            py_module = py_module[prefix.len()..].to_string();
                        }
                    }

                    // Convert to Python module path format
                    let py_module = py_module.replace("::", ".");

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

        let body = output_body.trim_end_matches('\n');

        Ok(format!("{header}{body}\n"))
    }

    /// Generates the Python definition string for a single TypeInfo, collecting imports.
    fn generate_py_for_type(
        &self,
        info: &TypeInfo,
        imports: &mut BTreeMap<String, HashSet<String>>,
    ) -> Result<String, RegistryError> {
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

    fn generate_struct_py(
        &self,
        py_name: &str,
        s_info: &StructInfo,
        type_attrs: &TypeAttributes,
        docs: &[String],
        imports: &mut BTreeMap<String, HashSet<String>>,
    ) -> Result<String, RegistryError> {
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
                        RegistryError::GenerationFailed(
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

    fn generate_enum_py(
        &self,
        py_name: &str,
        e_info: &EnumInfo,
        type_attrs: &TypeAttributes,
        docs: &[String],
        imports: &mut BTreeMap<String, HashSet<String>>,
    ) -> Result<String, RegistryError> {
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
    ) -> Result<(String, Option<String>), RegistryError> {
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
    ) -> Result<String, RegistryError> {
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

    fn get_py_type_str(
        &self,
        type_ref: &TypeRef,
        imports: &mut BTreeMap<String, HashSet<String>>,
    ) -> Result<String, RegistryError> {
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
                    if let Some(info) = self.registry.type_info.get(tid) {
                        return Ok(info.name.clone());
                    }
                }

                // Otherwise, fall back to path resolution
                if *is_reflect_to {
                    // For Python we just need the base name
                    if let Some(last_colon) = path.rfind("::") {
                        let type_name = &path[last_colon + 2..];
                        if let Some(type_id) = self.registry.find_type_by_name(type_name) {
                            return self.registry.get_base_name(type_id);
                        }
                    }

                    // If path doesn't have :: or type not found by name
                    add_import(imports, "typing", "Any");
                    Ok("Any".to_string())
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

    /// Writes the generated Python type stubs to the specified file path.
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), RegistryError> {
        let output = self.generate()?;
        self.registry.write_to_file(path, &output)
    }
}

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
