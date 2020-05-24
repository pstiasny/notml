use assert_cmd::Command;
use tempfile::tempdir;


fn run_program(
    prog_name: &str,
    dir: tempfile::TempDir
) -> assert_cmd::assert::Assert {
    let binary_path = dir.path().join(prog_name);

    let mut compiler_cmd = Command::cargo_bin("notmlc").unwrap();
    let compiler_assert = compiler_cmd
        .arg(&format!("../programs/{}.notml", prog_name))
        .arg("-o")
        .arg(&binary_path)
        .assert();
    compiler_assert.success();

    let mut program_cmd = Command::new(&binary_path);
    program_cmd.assert()
}

#[test]
fn fib() {
    let dir = tempdir().unwrap();
    let program_assert = run_program("fib", dir);
    program_assert.success().stdout("34\n");
}

#[test]
fn fact() {
    let dir = tempdir().unwrap();
    let program_assert = run_program("fact", dir);
    program_assert.success().stdout("120\n");
}

#[test]
fn fact_tail() {
    let dir = tempdir().unwrap();
    let program_assert = run_program("fact_tail", dir);
    program_assert.success().stdout("120\n");
}

#[test]
fn fizzbuzz() {
    let dir = tempdir().unwrap();
    let program_assert = run_program("fizzbuzz", dir);
    program_assert.success().stdout("\
1
2
fizz
4
buzz
fizz
7
8
fizz
buzz
11
fizz
13
14
fizzbuzz
16
17
fizz
19
buzz
fizz
22
23
fizz
buzz
");
}

#[test]
fn seq() {
    let dir = tempdir().unwrap();
    let program_assert = run_program("seq", dir);
    program_assert.success().stdout("\
10
9
8
7
6
5
4
3
2
1
0
");
}

#[test]
fn list() {
    let dir = tempdir().unwrap();
    let program_assert = run_program("list", dir);
    program_assert.success().stdout("1234");
}
