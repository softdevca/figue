//! Environment variable parsing for layered configuration.
//!
//! This module re-exports types from [`crate::layers::env`] for backwards compatibility.
//! New code should import directly from `layers::env`.

// Re-export all the types from layers::env
pub use crate::layers::env::{EnvConfig, EnvConfigBuilder, EnvSource, MockEnv, StdEnv};
