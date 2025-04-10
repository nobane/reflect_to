// crates/reflect_macro/src/lib.rs
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, Attribute, Data, DataEnum, DataStruct, DeriveInput, Field, Fields,
    GenericArgument, LitStr, Meta, PathArguments, Type, Variant,
};

#[proc_macro_derive(Reflect, attributes(serde))]
pub fn derive_reflect_to(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let docs = extract_docs(&input.attrs);
    // Parse attributes - these functions now return quote blocks directly
    let (type_attrs_quote, rename_all_rule_opt) = parse_type_attributes(&input.attrs);

    let data_kind_quote = match &input.data {
        Data::Struct(data_struct) => generate_struct_info(data_struct, &rename_all_rule_opt),
        Data::Enum(data_enum) => generate_enum_info(data_enum, &input.attrs, &rename_all_rule_opt),
        Data::Union(_) => {
            return syn::Error::new_spanned(input, "Reflection cannot be derived for unions")
                .to_compile_error()
                .into();
        }
    };

    // Module path calculation
    let module_path = quote! {
        {
            let path = module_path!();
            let type_name_str = stringify!(#name);

            let parts: Vec<&str> = path.split("::").collect();

            // Remove crate name (first part)
            let parts_no_crate = if parts.len() > 1 { &parts[1..] } else { &parts[..] };

            // If the last part is the type name, remove it
            if parts_no_crate.last() == Some(&type_name_str) {
                if parts_no_crate.len() > 1 {
                    parts_no_crate[..parts_no_crate.len()-1].join("::")
                } else {
                    String::new() // Only TypeName left after crate, so root module ""
                }
            } else {
                parts_no_crate.join("::") // Type name not last? Use full path after crate name.
            }
        }
    };

    let dependencies_quote = generate_dependency_adders(&input);

    let expanded = quote! {
        // Ensure the core crate is accessible via a known alias
        #[allow(unused_imports)]

        impl ::reflect_to::Reflection for #name {
            fn reflect() -> ::reflect_to::TypeInfo {
                ::reflect_to::TypeInfo {
                    name: stringify!(#name).to_string(),
                    module_path: { #module_path },
                    data: #data_kind_quote,
                    attributes: #type_attrs_quote,
                    docs: vec![#(#docs.to_string()),*],
                    dependencies: #dependencies_quote,
                }
            }
        }
    };

    TokenStream::from(expanded)
}

fn generate_dependency_adders(input: &DeriveInput) -> proc_macro2::TokenStream {
    let mut dependencies = Vec::new();

    match &input.data {
        Data::Struct(data_struct) => {
            collect_field_dependencies(&data_struct.fields, &mut dependencies);
        }
        Data::Enum(data_enum) => {
            for variant in &data_enum.variants {
                collect_field_dependencies(&variant.fields, &mut dependencies);
            }
        }
        Data::Union(_) => unreachable!("Unions already handled earlier"),
    }

    // Make dependencies unique by deduplicating based on the string representation
    dependencies.dedup_by(|a, b| {
        a.path.to_token_stream().to_string() == b.path.to_token_stream().to_string()
    });

    // Generate the vector of adder functions
    if dependencies.is_empty() {
        quote! { Vec::new() }
    } else {
        let adders = dependencies.iter().map(|dep_type| {
            let type_path_tokens = &dep_type.path;
            quote! {
                |registry: &mut dyn ::reflect_to::Visit| -> Result<(), Box<dyn std::error::Error>> {
                    let type_id = <#type_path_tokens as ::reflect_to::Reflection>::type_id();
                    let info = <#type_path_tokens as ::reflect_to::Reflection>::reflect();
                    registry.visit_type(type_id, info)?;
                    Ok(())
                }
            }
        });

        quote! {
            vec![
                #(#adders),*
            ]
        }
    }
}

fn collect_field_dependencies(fields: &Fields, dependencies: &mut Vec<syn::TypePath>) {
    match fields {
        Fields::Named(named_fields) => {
            for field in &named_fields.named {
                extract_dependencies_from_type(&field.ty, dependencies);
            }
        }
        Fields::Unnamed(unnamed_fields) => {
            for field in &unnamed_fields.unnamed {
                extract_dependencies_from_type(&field.ty, dependencies);
            }
        }
        Fields::Unit => {}
    }
}

// Recursively extract type paths from a type
fn extract_dependencies_from_type(ty: &Type, dependencies: &mut Vec<syn::TypePath>) {
    match ty {
        Type::Path(type_path) => {
            // Skip if the path has a qself
            if type_path.qself.is_some() {
                return;
            }

            // Get the last segment to check if it's a custom type
            if let Some(last_segment) = type_path.path.segments.last() {
                let ident_str = last_segment.ident.to_string();

                // Only add paths that seem to be Reflection types
                if !is_primitive_or_std_type(&ident_str) {
                    dependencies.push(type_path.clone());
                }

                // Process generic arguments recursively
                if let PathArguments::AngleBracketed(args) = &last_segment.arguments {
                    for arg in &args.args {
                        if let GenericArgument::Type(nested_type) = arg {
                            extract_dependencies_from_type(nested_type, dependencies);
                        }
                    }
                }
            }
        }
        Type::Reference(type_ref) => {
            extract_dependencies_from_type(&type_ref.elem, dependencies);
        }
        Type::Array(array) => {
            extract_dependencies_from_type(&array.elem, dependencies);
        }
        Type::Tuple(tuple) => {
            for elem in &tuple.elems {
                extract_dependencies_from_type(elem, dependencies);
            }
        }
        Type::Group(group) => {
            extract_dependencies_from_type(&group.elem, dependencies);
        }
        Type::Paren(paren) => {
            extract_dependencies_from_type(&paren.elem, dependencies);
        }
        Type::Slice(slice) => {
            extract_dependencies_from_type(&slice.elem, dependencies);
        }
        // For other types, we don't extract dependencies
        _ => {}
    }
}

// Check if a type name is likely a primitive or standard library type
fn is_primitive_or_std_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "String"
            | "str"
            | "bool"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f32"
            | "f64"
            | "char"
            | "Vec"
            | "HashMap"
            | "BTreeMap"
            | "Option"
            | "Result"
            | "Box"
            | "PathBuf"
    )
}

fn generate_struct_info(
    data_struct: &DataStruct,
    rename_all_rule: &Option<String>,
) -> proc_macro2::TokenStream {
    let fields_info_quote = generate_fields_info(&data_struct.fields, rename_all_rule);
    quote! {
        ::reflect_to::DataKind::Struct(::reflect_to::StructInfo {
            fields: #fields_info_quote,
        })
    }
}

fn generate_enum_info(
    data_enum: &DataEnum,
    attrs: &[Attribute],
    rename_all_rule: &Option<String>,
) -> proc_macro2::TokenStream {
    let representation = parse_enum_representation(attrs); // Returns quote! block
    let variants_quote = data_enum
        .variants
        .iter()
        .map(|variant| generate_variant_info(variant, rename_all_rule))
        .collect::<Vec<_>>();

    quote! {
        ::reflect_to::DataKind::Enum(::reflect_to::EnumInfo {
            variants: vec![#(#variants_quote),*],
            representation: #representation,
        })
    }
}

fn generate_variant_info(
    variant: &Variant,
    enum_rename_all_rule: &Option<String>,
) -> proc_macro2::TokenStream {
    let name = variant.ident.to_string();
    let docs = extract_docs(&variant.attrs);
    let variant_attrs_quote = parse_variant_attributes(&variant.attrs); // Returns quote! block
    let fields_info_quote = generate_fields_info(&variant.fields, enum_rename_all_rule);

    quote! {
        ::reflect_to::VariantInfo {
            name: #name.to_string(),
            fields: #fields_info_quote,
            attributes: #variant_attrs_quote,
            docs: vec![#(#docs.to_string()),*],
        }
    }
}

fn generate_fields_info(
    fields: &Fields,
    rename_all_rule: &Option<String>,
) -> proc_macro2::TokenStream {
    match fields {
        Fields::Named(f) => {
            let field_quotes = f
                .named
                .iter()
                .enumerate()
                .map(|(i, field)| generate_field_info(field, rename_all_rule, i))
                .collect::<Vec<_>>();
            quote! { ::reflect_to::FieldsInfo::Named(vec![#(#field_quotes),*]) }
        }
        Fields::Unnamed(f) => {
            let field_quotes = f
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, field)| generate_field_info(field, rename_all_rule, i))
                .collect::<Vec<_>>();
            quote! { ::reflect_to::FieldsInfo::Unnamed(vec![#(#field_quotes),*]) }
        }
        Fields::Unit => {
            quote! { ::reflect_to::FieldsInfo::Unit }
        }
    }
}

fn generate_field_info(
    field: &Field,
    _parent_rename_all_rule: &Option<String>, // Rule not needed here, applied by generator
    index: usize,
) -> proc_macro2::TokenStream {
    let name_quote = match &field.ident {
        Some(ident) => {
            let s = ident.to_string();
            quote! { Some(#s.to_string()) }
        }
        None => quote! { None },
    };
    let docs = extract_docs(&field.attrs);
    let field_attrs_quote = parse_field_attributes(&field.attrs); // Returns quote! block
    let ty_ref_quote = generate_type_ref(&field.ty); // Returns quote! block
    let index_quote = quote! { #index };

    quote! {
        ::reflect_to::FieldInfo {
            name: #name_quote,
            ty: #ty_ref_quote,
            attributes: #field_attrs_quote,
            docs: vec![#(#docs.to_string()),*],
            index: #index_quote,
        }
    }
}

fn generate_type_ref(ty: &Type) -> proc_macro2::TokenStream {
    match ty {
        Type::Path(type_path) => {
            if type_path.qself.is_some() {
                return generate_unsupported_type_ref(ty);
            }
            if let Some(last_segment) = type_path.path.segments.last() {
                let ident_str = last_segment.ident.to_string();
                let full_path_str = type_path
                    .path
                    .to_token_stream()
                    .to_string()
                    .replace(' ', "");

                // 1. Primitives
                if let Some(primitive_quote) = map_primitive(&ident_str) {
                    return primitive_quote;
                }

                // 2. Special Containers
                if let PathArguments::AngleBracketed(args) = &last_segment.arguments {
                    match ident_str.as_str() {
                        "Option" if args.args.len() == 1 => {
                            if let Some(GenericArgument::Type(inner)) = args.args.first() {
                                let inner_ref = generate_type_ref(inner);
                                return quote! { ::reflect_to::TypeRef::Option(Box::new(#inner_ref)) };
                            }
                        }
                        "Vec" if args.args.len() == 1 => {
                            if let Some(GenericArgument::Type(inner)) = args.args.first() {
                                let inner_ref = generate_type_ref(inner);
                                return quote! { ::reflect_to::TypeRef::Vec(Box::new(#inner_ref)) };
                            }
                        }
                        "Box" if args.args.len() == 1 => {
                            if let Some(GenericArgument::Type(inner)) = args.args.first() {
                                let inner_ref = generate_type_ref(inner);
                                return quote! { ::reflect_to::TypeRef::Box(Box::new(#inner_ref)) };
                            }
                        }
                        "Result" if args.args.len() == 2 => {
                            if let (
                                Some(GenericArgument::Type(ok)),
                                Some(GenericArgument::Type(err)),
                            ) = (args.args.first(), args.args.get(1))
                            {
                                let ok_ref = generate_type_ref(ok);
                                let err_ref = generate_type_ref(err);
                                return quote! { ::reflect_to::TypeRef::Result{ ok_type: Box::new(#ok_ref), err_type: Box::new(#err_ref)} };
                            }
                        }
                        "HashMap" | "BTreeMap" if args.args.len() == 2 => {
                            if let (
                                Some(GenericArgument::Type(key)),
                                Some(GenericArgument::Type(val)),
                            ) = (args.args.first(), args.args.get(1))
                            {
                                let key_ref = generate_type_ref(key);
                                let val_ref = generate_type_ref(val);
                                return quote! { ::reflect_to::TypeRef::Map{ key_type: Box::new(#key_ref), value_type: Box::new(#val_ref) } };
                            }
                        }
                        _ => {}
                    }
                } else if ident_str == "()" {
                    return quote! { ::reflect_to::TypeRef::Unit };
                }

                // 3. Assume User-Defined Path
                let generic_args_quote =
                    if let PathArguments::AngleBracketed(args) = &last_segment.arguments {
                        args.args
                            .iter()
                            .filter_map(|arg| {
                                if let GenericArgument::Type(inner_ty) = arg {
                                    Some(generate_type_ref(inner_ty))
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<_>>()
                    } else {
                        vec![]
                    };

                // Basic heuristic for is_reflect_to
                let is_reflect_to = !is_primitive_or_std_type(&ident_str);

                // Generate type_id if possible
                let type_id_quote = if is_reflect_to {
                    let type_path_tokens = &type_path.path;
                    quote! {
                        Some(::std::any::TypeId::of::<#type_path_tokens>())
                    }
                } else {
                    quote! { None }
                };

                return quote! {
                    ::reflect_to::TypeRef::Path {
                        path: #full_path_str.to_string(),
                        generic_args: vec![#(#generic_args_quote),*],
                        is_reflect_to: #is_reflect_to,
                        type_id: #type_id_quote,
                    }
                };
            }
            generate_unsupported_type_ref(ty)
        }
        Type::Tuple(type_tuple) => {
            if type_tuple.elems.is_empty() {
                quote! { ::reflect_to::TypeRef::Unit }
            } else {
                let elem_quotes = type_tuple
                    .elems
                    .iter()
                    .map(generate_type_ref)
                    .collect::<Vec<_>>();
                quote! { ::reflect_to::TypeRef::Tuple(vec![#(#elem_quotes),*]) }
            }
        }
        Type::Array(type_array) => {
            let elem_ty_ref = generate_type_ref(&type_array.elem);
            let len = 0; // Placeholder - syn doesn't easily evaluate the const len expr
            quote! { ::reflect_to::TypeRef::Array { elem_type: Box::new(#elem_ty_ref), len: #len } }
        }
        Type::Reference(type_ref) => generate_type_ref(&type_ref.elem),
        Type::Paren(type_paren) => generate_type_ref(&type_paren.elem),
        Type::Never(_) => quote! { ::reflect_to::TypeRef::Never },
        Type::Slice(type_slice) => {
            let inner_ty_ref = generate_type_ref(&type_slice.elem);
            quote! { ::reflect_to::TypeRef::Vec(Box::new(#inner_ty_ref)) } // Treat slice like Vec
        }
        _ => generate_unsupported_type_ref(ty),
    }
}

fn map_primitive(ident_str: &str) -> Option<proc_macro2::TokenStream> {
    let primitive_variant = match ident_str {
        "String" => quote! { String },
        "str" => quote! { Str },
        "bool" => quote! { Bool },
        "i8" => quote! { I8 },
        "i16" => quote! { I16 },
        "i32" => quote! { I32 },
        "i64" => quote! { I64 },
        "i128" => quote! { I128 },
        "isize" => quote! { Isize },
        "u8" => quote! { U8 },
        "u16" => quote! { U16 },
        "u32" => quote! { U32 },
        "u64" => quote! { U64 },
        "u128" => quote! { U128 },
        "usize" => quote! { Usize },
        "f32" => quote! { F32 },
        "f64" => quote! { F64 },
        "PathBuf" => quote! { PathBuf },
        "char" => quote! { Char },
        _ => return None,
    };
    Some(
        quote! { ::reflect_to::TypeRef::Primitive(::reflect_to::PrimitiveType::#primitive_variant) },
    )
}

fn generate_unsupported_type_ref(ty: &Type) -> proc_macro2::TokenStream {
    let type_string = ty.to_token_stream().to_string();
    quote! { ::reflect_to::TypeRef::Unsupported(#type_string.to_string()) }
}

fn extract_docs(attrs: &[Attribute]) -> Vec<String> {
    attrs
        .iter()
        .filter_map(|attr| {
            if attr.path().is_ident("doc") {
                if let Meta::NameValue(nv) = &attr.meta {
                    if let syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(s),
                        ..
                    }) = &nv.value
                    {
                        return Some(s.value().trim().to_string());
                    }
                }
            }
            None
        })
        .collect()
}

// Parses type attributes and returns (quote! block for TypeAttributes, Option<String>)
fn parse_type_attributes(attrs: &[Attribute]) -> (proc_macro2::TokenStream, Option<String>) {
    let mut rename_opt: Option<String> = None;
    let mut rename_all_opt: Option<String> = None;
    let mut other_attrs_map: std::collections::BTreeMap<String, String> = Default::default();

    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }
        // Use parse_nested_meta and ignore errors silently for attribute gathering
        let _ = attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("rename") {
                rename_opt = Some(meta.value()?.parse::<LitStr>()?.value());
            } else if meta.path.is_ident("rename_all") {
                rename_all_opt = Some(meta.value()?.parse::<LitStr>()?.value());
            } else if !meta.path.is_ident("tag")
                && !meta.path.is_ident("content")
                && !meta.path.is_ident("untagged")
            {
                // Capture others, excluding enum representation handled elsewhere
                let path = meta.path.to_token_stream().to_string();
                if let Ok(value) = meta.value()?.parse::<LitStr>() {
                    other_attrs_map.insert(path, value.value());
                } else if meta.input.is_empty() {
                    // boolean attribute
                    other_attrs_map.insert(path, "true".to_string());
                }
            }
            Ok(())
        });
    }

    let rename_quote = match rename_opt {
        Some(s) => quote! { Some(#s.to_string()) },
        None => quote! { None },
    };
    let rename_all_quote = match &rename_all_opt {
        Some(s) => {
            quote! { Some(::reflect_to::RenameRuleValue::Rule(#s.to_string())) }
        }
        _ => quote! { None },
    };
    let other_attrs_quote = other_attrs_map.into_iter().map(|(k, v)| {
        quote! { map.insert(#k.to_string(), #v.to_string()); }
    });

    let type_attrs_struct_quote = quote! {
        ::reflect_to::TypeAttributes {
            rename: #rename_quote,
            rename_all: #rename_all_quote,
            other: {
                 let mut map = ::std::collections::BTreeMap::new();
                 #(#other_attrs_quote)*
                 map
            },
        }
    };

    (type_attrs_struct_quote, rename_all_opt)
}

// Parses enum representation attributes and returns quote! block for EnumRepresentation
fn parse_enum_representation(attrs: &[Attribute]) -> proc_macro2::TokenStream {
    let mut tag: Option<String> = None;
    let mut content: Option<String> = None;
    let mut untagged = false;

    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }
        // Ignore errors during parsing for representation
        let _ = attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("tag") {
                tag = Some(meta.value()?.parse::<LitStr>()?.value());
            } else if meta.path.is_ident("content") {
                content = Some(meta.value()?.parse::<LitStr>()?.value());
            } else if meta.path.is_ident("untagged") {
                untagged = true;
            }
            Ok(())
        });
    }

    if untagged {
        quote! { ::reflect_to::EnumRepresentation::Untagged }
    } else if let (Some(t), Some(c)) = (tag.clone(), content.clone()) {
        // Clone to avoid moving tag
        quote! { ::reflect_to::EnumRepresentation::AdjacentlyTagged { tag: #t.to_string(), content: #c.to_string() } }
    } else if let Some(t) = tag {
        // Now `tag` is available if the previous `if let` was false
        quote! { ::reflect_to::EnumRepresentation::InternallyTagged { tag: #t.to_string() } }
    } else {
        quote! { ::reflect_to::EnumRepresentation::ExternallyTagged }
    }
}

// Parses field attributes and returns quote! block for FieldAttributes
fn parse_field_attributes(attrs: &[Attribute]) -> proc_macro2::TokenStream {
    let mut rename_opt: Option<String> = None;
    let mut skip_flag = false;
    let mut skip_serializing_flag = false;
    let mut skip_deserializing_flag = false;
    let mut skip_serializing_if_opt: Option<String> = None;
    let mut flatten_flag = false;
    let mut other_attrs_map: std::collections::BTreeMap<String, String> = Default::default();

    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }
        // Ignore errors during parsing for attributes
        let _ = attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("rename") {
                rename_opt = Some(meta.value()?.parse::<LitStr>()?.value());
            } else if meta.path.is_ident("skip") {
                skip_flag = true;
            } else if meta.path.is_ident("skip_serializing") {
                skip_serializing_flag = true;
            } else if meta.path.is_ident("skip_deserializing") {
                skip_deserializing_flag = true;
            } else if meta.path.is_ident("skip_serializing_if") {
                skip_serializing_if_opt = Some(meta.value()?.parse::<LitStr>()?.value());
            } else if meta.path.is_ident("flatten") {
                flatten_flag = true;
            } else {
                // Capture others
                let path = meta.path.to_token_stream().to_string();
                if let Ok(value) = meta.value()?.parse::<LitStr>() {
                    other_attrs_map.insert(path, value.value());
                } else if meta.input.is_empty() {
                    other_attrs_map.insert(path, "true".to_string());
                }
            }
            Ok(())
        });
    }

    // Apply skip logic: #[serde(skip)] implies both skipping serialization and deserialization
    if skip_flag {
        skip_serializing_flag = true;
        skip_deserializing_flag = true;
    }

    // Generate quote blocks based on the final flag values
    let rename_quote = match rename_opt {
        Some(s) => quote! { Some(#s.to_string()) },
        None => quote! { None },
    };
    let skip_quote = quote! { #skip_flag };
    let skip_serializing_quote = quote! { #skip_serializing_flag };
    let skip_deserializing_quote = quote! { #skip_deserializing_flag };
    let skip_serializing_if_quote = match skip_serializing_if_opt {
        Some(s) => quote! { Some(#s.to_string()) },
        None => quote! { None },
    };
    let flatten_quote = quote! { #flatten_flag };
    let other_attrs_quote = other_attrs_map.into_iter().map(|(k, v)| {
        quote! { map.insert(#k.to_string(), #v.to_string()); }
    });

    quote! {
        ::reflect_to::FieldAttributes {
            rename: #rename_quote,
            skip: #skip_quote,
            skip_serializing: #skip_serializing_quote,
            skip_deserializing: #skip_deserializing_quote,
            skip_serializing_if: #skip_serializing_if_quote,
            flatten: #flatten_quote,
            other: {
                 let mut map = ::std::collections::BTreeMap::new();
                 #(#other_attrs_quote)*
                 map
            },
        }
    }
}

// Parses variant attributes and returns quote! block for VariantAttributes
fn parse_variant_attributes(attrs: &[Attribute]) -> proc_macro2::TokenStream {
    let mut rename_opt: Option<String> = None;
    let mut skip_flag = false;
    let mut skip_serializing_flag = false;
    let mut skip_deserializing_flag = false;
    let mut other_attrs_map: std::collections::BTreeMap<String, String> = Default::default();

    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }
        // Ignore errors during parsing for attributes
        let _ = attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("rename") {
                rename_opt = Some(meta.value()?.parse::<LitStr>()?.value());
            } else if meta.path.is_ident("skip") {
                skip_flag = true;
            } else if meta.path.is_ident("skip_serializing") {
                skip_serializing_flag = true;
            } else if meta.path.is_ident("skip_deserializing") {
                skip_deserializing_flag = true;
            } else {
                // Capture others
                let path = meta.path.to_token_stream().to_string();
                if let Ok(value) = meta.value()?.parse::<LitStr>() {
                    other_attrs_map.insert(path, value.value());
                } else if meta.input.is_empty() {
                    other_attrs_map.insert(path, "true".to_string());
                }
            }
            Ok(())
        });
    }

    // Apply skip logic
    if skip_flag {
        skip_serializing_flag = true;
        skip_deserializing_flag = true;
    }

    // Generate quote blocks
    let rename_quote = match rename_opt {
        Some(s) => quote! { Some(#s.to_string()) },
        None => quote! { None },
    };
    let skip_quote = quote! { #skip_flag };
    let skip_serializing_quote = quote! { #skip_serializing_flag };
    let skip_deserializing_quote = quote! { #skip_deserializing_flag };
    let other_attrs_quote = other_attrs_map.into_iter().map(|(k, v)| {
        quote! { map.insert(#k.to_string(), #v.to_string()); }
    });

    quote! {
        ::reflect_to::VariantAttributes {
            rename: #rename_quote,
            skip: #skip_quote,
            skip_serializing: #skip_serializing_quote,
            skip_deserializing: #skip_deserializing_quote,
            other: {
                 let mut map = ::std::collections::BTreeMap::new();
                 #(#other_attrs_quote)*
                 map
            },
        }
    }
}
