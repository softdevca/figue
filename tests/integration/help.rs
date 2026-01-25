#![allow(dead_code)]

use crate::assert_help_snapshot;
use facet::Facet;
use figue as args;
use figue::FigueBuiltins;

/// A sample CLI application for testing help generation.
///
/// This is a longer description that spans multiple lines
/// to test how doc comments are handled.
#[derive(Facet, Debug)]
struct SimpleArgs {
    /// Enable verbose output
    #[facet(args::named, args::short = 'v')]
    verbose: bool,

    /// Number of parallel jobs
    #[facet(args::named, args::short = 'j')]
    jobs: Option<usize>,

    /// Input file to process
    #[facet(args::positional)]
    input: String,

    /// Output file (optional)
    #[facet(default, args::positional)]
    output: Option<String>,

    /// Standard CLI options
    #[facet(flatten)]
    builtins: FigueBuiltins,
}

#[test]
fn test_help_simple_struct() {
    let config = figue::HelpConfig {
        program_name: Some("myapp".to_string()),
        version: Some("1.0.0".to_string()),
        ..Default::default()
    };
    let help = figue::generate_help::<SimpleArgs>(&config);
    assert_help_snapshot!(help);
}

/// Git-like CLI with subcommands
#[derive(Facet, Debug)]
struct GitArgs {
    /// Git command to run
    #[facet(args::subcommand)]
    command: GitCommand,

    /// Standard CLI options
    #[facet(flatten)]
    builtins: FigueBuiltins,
}

/// Available git commands
#[derive(Facet, Debug)]
#[repr(u8)]
enum GitCommand {
    /// Clone a repository
    Clone {
        /// URL of the repository to clone
        #[facet(args::positional)]
        url: String,
        /// Directory to clone into
        #[facet(default, args::positional)]
        directory: Option<String>,
    },
    /// Show commit history
    Log {
        /// Number of commits to show
        #[facet(args::named, args::short = 'n')]
        count: Option<usize>,
        /// Show one line per commit
        #[facet(args::named)]
        oneline: bool,
    },
    /// Manage remotes
    Remote {
        /// Remote subcommand
        #[facet(args::subcommand)]
        action: RemoteAction,
    },
}

/// Remote management commands
#[derive(Facet, Debug)]
#[repr(u8)]
enum RemoteAction {
    /// Add a new remote
    Add {
        /// Name of the remote
        #[facet(args::positional)]
        name: String,
        /// URL of the remote
        #[facet(args::positional)]
        url: String,
    },
    /// Remove a remote
    #[facet(rename = "rm")]
    Remove {
        /// Name of the remote to remove
        #[facet(args::positional)]
        name: String,
    },
    /// List all remotes
    #[facet(rename = "ls")]
    List {
        /// Show verbose output
        #[facet(args::named, args::short = 'v')]
        verbose: bool,
    },
}

#[test]
fn test_help_with_subcommands() {
    let config = figue::HelpConfig {
        program_name: Some("git".to_string()),
        version: Some("2.40.0".to_string()),
        ..Default::default()
    };
    let help = figue::generate_help::<GitArgs>(&config);
    assert_help_snapshot!(help);
}

#[test]
fn test_help_enum_only() {
    let config = figue::HelpConfig {
        program_name: Some("git".to_string()),
        ..Default::default()
    };
    let help = figue::generate_help::<GitCommand>(&config);
    assert_help_snapshot!(help);
}

/// Test that --help and -h flags trigger help when FigueBuiltins is present
#[test]
fn test_help_flags() {
    // --help
    let result = figue::from_slice::<SimpleArgs>(&["--help"]);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.is_help());
    assert!(err.help_text().is_some());
    let help = err.help_text().unwrap();
    assert!(help.contains("USAGE"));
    assert!(help.contains("--verbose"));

    // -h
    let result = figue::from_slice::<SimpleArgs>(&["-h"]);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.is_help());
}

/// Test that help output for tuple variant subcommands shows flattened fields
/// instead of `--0 <STRUCTNAME>`.
#[test]
fn test_tuple_variant_subcommand_help_flattening() {
    #[derive(Facet, Debug)]
    struct BuildArgs {
        /// Build in release mode
        #[facet(args::named, args::short = 'r')]
        release: bool,

        /// Disable spawning processes
        #[facet(args::named)]
        no_spawn: bool,

        /// Disable TUI mode
        #[facet(args::named)]
        no_tui: bool,
    }

    #[derive(Facet, Debug)]
    #[repr(u8)]
    enum Command {
        /// Build the project
        Build(BuildArgs),
        /// Run tests
        Test {
            /// Run in verbose mode
            #[facet(args::named, args::short = 'v')]
            verbose: bool,
        },
    }

    #[derive(Facet, Debug)]
    struct Args {
        #[facet(args::subcommand)]
        command: Command,

        #[facet(flatten)]
        builtins: FigueBuiltins,
    }

    // Test help for the main command
    let config = figue::HelpConfig {
        program_name: Some("myapp".to_string()),
        ..Default::default()
    };
    let help = figue::generate_help::<Args>(&config);
    assert_help_snapshot!("tuple_variant_main_help", help);
}
