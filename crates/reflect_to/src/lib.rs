pub use reflect_to_core::*;
pub use reflect_to_macro::*;

/// Applies a rename rule to an original string based on the `RenameRuleValue`.
pub fn apply_rename_rule(original: &str, rule: &Option<RenameRuleValue>) -> Option<String> {
    // (This function remains the same as before)
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
        Some(RenameRuleValue::None) | None => None,
    }
}

// --- Case Conversion Helpers ---

/// Converts a string to PascalCase.
/// "foo_bar" -> "FooBar", "foo-bar" -> "FooBar"
pub fn to_pascal_case(s: &str) -> String {
    // (This function remains the same as before)
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

/// Converts a string to camelCase.
/// "foo_bar" -> "fooBar", "FooBar" -> "fooBar"
pub fn to_camel_case(s: &str) -> String {
    // (This function remains the same as before)
    let pascal = to_pascal_case(s);
    if let Some(first) = pascal.chars().next() {
        format!("{}{}", first.to_lowercase(), &pascal[first.len_utf8()..])
    } else {
        pascal // Empty string
    }
}

/// Converts a string to snake\_case.
/// "FooBar" -> "foo_bar", "fooBar" -> "foo_bar", "FOO_BAR" -> "foo_bar"
/// Uses a logic inspired by the `heck` crate for better handling of acronyms.
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
    // (This function remains the same as before)
    to_snake_case(s).to_ascii_uppercase()
}

/// Converts a string to kebab-case.
/// "fooBar" -> "foo-bar"
pub fn to_kebab_case(s: &str) -> String {
    // (This function remains the same as before)
    to_snake_case(s).replace('_', "-")
}
