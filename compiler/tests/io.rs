use std::env;
use std::io::{Write};
use assert_cmd::Command;
use tempfile::NamedTempFile;
use predicates::prelude::*;


#[test]
fn missing_source() {
    let config_path = env::var("NOTML_TEST_CFG").unwrap();

    let mut compiler_cmd = Command::cargo_bin("notmlc").unwrap();
    let compiler_assert = compiler_cmd
        .arg("-c")
        .arg(config_path)
        .arg("./NONEXISTENT")
        .assert();
    compiler_assert
        .failure()
        .stderr(predicate::str::contains("could not read source file"));
}


#[test]
fn missing_config() {
    let mut compiler_cmd = Command::cargo_bin("notmlc").unwrap();
    let compiler_assert = compiler_cmd
        .arg("-c")
        .arg("NONEXISTENT")
        .arg("../programs/fib.notml")
        .assert();
    compiler_assert
        .failure()
        .stderr(predicate::str::contains("could not read config file"));
}


#[test]
fn bad_config() {
    let mut config_file = NamedTempFile::new().unwrap();
    writeln!(config_file, "bad config");

    let mut compiler_cmd = Command::cargo_bin("notmlc").unwrap();
    let compiler_assert = compiler_cmd
        .arg("-c")
        .arg(config_file.path())
        .arg("../programs/fib.notml")
        .assert();
    compiler_assert
        .failure()
        .stderr(predicate::str::contains("incorrect configuration"));
}
