use facet::Facet;
use figue as args;

#[derive(Facet, Debug)]
struct Args {
    #[facet(args::named, rename = "cores")]
    concurrency: i64,
}

fn main() {
    // Test with JSON - use effective_name "cores"
    let json = r#"{"cores": 4}"#;
    match facet_json::from_str::<Args>(json) {
        Ok(args) => println!("JSON with 'cores': {:?}", args),
        Err(e) => println!("JSON with 'cores' failed: {}", e),
    }

    // Test with JSON - use original name "concurrency"
    let json = r#"{"concurrency": 4}"#;
    match facet_json::from_str::<Args>(json) {
        Ok(args) => println!("JSON with 'concurrency': {:?}", args),
        Err(e) => println!("JSON with 'concurrency' failed: {}", e),
    }
}
