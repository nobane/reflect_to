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
#[serde(rename_all = "camelCase")]
pub struct UserSettings {
    theme_path: PathBuf,
    email_notifications: bool,
    #[serde(rename = "custom")]
    custom_settings: HashMap<String, String>,
}
