#![allow(dead_code)]
use tokio;

mod io;

pub struct Server {
    stdin: tokio::io::Stdin,
    stdout: tokio::io::Stdout,
}

impl Server {
    async fn run() {
        unimplemented!();
    }
}

pub trait LanguageServer {}
