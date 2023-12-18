use std::{env, fs::File, io::BufReader};

use ice_parser::Parser;
use iiv_gen::Generator;
use iiv_llvm::Backend;

fn main() {
    let mut args = env::args();
    args.next();
    let Some(name) = args.next() else {
        eprintln!("no input file!");
        return;
    };

    let ctx = iiv::Ctx::new(File::open(&name).unwrap(), name);

    let mut parser = Parser::new(&ctx, BufReader::new(&ctx.source.file));
    let mut generator = Generator::new(&ctx);
    let mut backend = Backend::new(&ctx);

    let module = parser.parse_program();
    if ctx.flush_diagnostics() {
        return;
    }

    println!("{:?}", module);

    let package = generator.emit_iiv(&[module]);
    if ctx.flush_diagnostics() {
        println!("there were errors");
        return;
    }

    for fun in &package.funcs {
        println!("{}", fun.borrow());
        iiv::move_check::check(&mut *fun.borrow_mut());
        println!("{}", fun.borrow());
    }

    backend.transform(&package, "out.o");

    // if cfg!(windows) {
    //      cc::windows_registry::find_tool(env!("TARGET"), "link.exe");
    //     let out = Command::new("link.exe")
    //         .arg("out.o")
    //         .arg("-o out.exe")
    //         .output()
    //         .expect("linking failed");
    //     if !out.status.success() {
    //         eprintln!("linking failed");
    //     }
    // } else {
    //     println!("no link");
    // }
}
