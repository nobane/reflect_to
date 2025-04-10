use crate::{Reflection, TypeInfo};
use std::{
    any::TypeId,
    collections::{BTreeMap, HashMap},
    error::Error,
    fs::File,
    io::{self, Write},
    path::Path,
};
use thiserror::Error;

/// A trait for types that can store type reflection information.
pub trait Visit {
    /// Register a type in the registry with its type information
    fn visit_type(&mut self, type_id: TypeId, info: TypeInfo) -> Result<(), Box<dyn Error>>;
}

/// Shared errors for type generators
#[derive(Error, Debug)]
pub enum RegistryError {
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    #[error("Type resolution error: {0}")]
    Resolution(String),
    #[error("Ambiguous type name '{0}'. Found in modules: [{1}].")]
    AmbiguousTypeName(String, String),
    #[error("Type with ID {0:?} not found in registry.")]
    TypeNotFound(TypeId),
    #[error("Generation failed for type '{0}': {1}")]
    GenerationFailed(String, String),
    #[error("Dependency error: {0}")]
    Dependency(String),
    #[error("Unsupported feature: {0}")]
    Unsupported(String),
}

pub type ModulePath = String;
pub type TypeName = String;
pub type TypeKey = (ModulePath, TypeName);

/// A shared base registry for type generators.
#[derive(Default)]
pub struct TypeRegistry {
    /// Stores reflection info keyed by TypeId.
    pub type_info: HashMap<TypeId, TypeInfo>,
    /// Maps module path and type name pair to TypeId.
    pub type_registry: BTreeMap<TypeKey, TypeId>,
    /// Maintains insertion order for consistent output.
    pub visit_order: Vec<TypeId>,
}

impl TypeRegistry {
    /// Adds a Rust type `T` to the registry.
    pub fn add_type<T>(&mut self) -> Result<(), RegistryError>
    where
        T: Reflection + 'static,
    {
        let type_id = TypeId::of::<T>();
        let info = T::reflect();

        self.visit_type(type_id, info)
            .map_err(|e| RegistryError::Dependency(e.to_string()))
    }

    pub fn get_type(&self, type_id: &TypeId) -> Option<&TypeInfo> {
        self.type_info.get(type_id)
    }

    pub fn try_get_type(&self, type_id: &TypeId) -> Result<&TypeInfo, RegistryError> {
        self.type_info
            .get(type_id)
            .ok_or(RegistryError::TypeNotFound(*type_id))
    }

    /// Processes a module path: always strips crate:: prefix
    pub fn strip_crate_prefix(module_path: &str) -> &str {
        module_path.strip_prefix("crate::").unwrap_or(module_path)
    }

    /// Helper function to detect common prefix among a collection of module paths
    pub fn detect_common_prefix(module_paths: &[String]) -> Option<String> {
        if module_paths.is_empty() {
            return None;
        }

        // Even with a single path, we should detect it as a common prefix
        // instead of returning None
        if module_paths.len() == 1 {
            return Some(module_paths[0].clone());
        }

        // Rest of the function stays the same...
        // Split paths into segments and find common prefix segments
        let path_segments: Vec<Vec<&str>> = module_paths
            .iter()
            .map(|path| path.split("::").collect())
            .collect();

        // Find minimum length path
        let min_len = path_segments.iter().map(|s| s.len()).min().unwrap_or(0);
        if min_len == 0 {
            return None;
        }

        // Find how many segments are common
        let mut common_segment_count = 0;
        for i in 0..min_len {
            let segment = path_segments[0][i];
            if path_segments.iter().all(|path| path[i] == segment) {
                common_segment_count += 1;
            } else {
                break;
            }
        }

        // Build common prefix string
        if common_segment_count == 0 {
            None
        } else {
            Some(path_segments[0][0..common_segment_count].join("::") + "::")
        }
    }

    /// Find a type ID by its name
    pub fn find_type_by_name(&self, type_name: &str) -> Option<TypeId> {
        for ((_, reg_name), type_id) in &self.type_registry {
            if reg_name == type_name {
                return Some(*type_id);
            }
        }
        None
    }

    /// Gets the base name for a registered type without module path
    pub fn get_base_name(&self, type_id: TypeId) -> Result<String, RegistryError> {
        let info = self
            .type_info
            .get(&type_id)
            .ok_or(RegistryError::TypeNotFound(type_id))?;
        Ok(info.name.clone())
    }

    /// Write the generated output to a file
    pub fn write_to_file<P: AsRef<Path>>(
        &self,
        path: P,
        content: &str,
    ) -> Result<(), RegistryError> {
        let mut file = File::create(path)?;
        file.write_all(content.as_bytes())?;
        Ok(())
    }
}

impl Visit for TypeRegistry {
    fn visit_type(&mut self, type_id: TypeId, info: TypeInfo) -> Result<(), Box<dyn Error>> {
        // Skip if already registered
        if self.type_info.contains_key(&type_id) {
            return Ok(());
        }

        // Process module path for registry key - strip crate:: for lookups
        let target_module_path = Self::strip_crate_prefix(&info.module_path).replace("::", ".");

        let type_name = info.name.clone();
        let type_key = (target_module_path, type_name);

        // Check for name collisions
        if let Some(existing_id) = self.type_registry.get(&type_key) {
            if *existing_id != type_id {
                eprintln!(
                    "Warning: Type name collision for '{}' in module '{}'. Overwriting.",
                    info.name, info.module_path
                );
            }
        }

        // Register the type
        self.type_registry.insert(type_key, type_id);
        self.type_info.insert(type_id, info.clone());
        self.visit_order.push(type_id);

        // Process dependencies recursively
        for add_dependency in &info.dependencies {
            add_dependency(self)?;
        }

        Ok(())
    }
}
