use std::{env, fs::File, io::BufReader};

use ice_parser::Parser;
use iiv::FileSource;
use iiv_gen::Generator;
use iiv_llvm::Backend;

fn main() {
    let mut args = env::args();
    args.next();
    let Some(name) = args.next() else {
        eprintln!("no input file!");
        return;
    };

    let source = FileSource {
        file: File::open(&name).unwrap(),
        name,
    };

    let ctx = iiv::Ctx::new();
    ctx.init();

    let mut parser = Parser::new(&ctx, BufReader::new(&source.file));
    let mut generator = Generator::new(&ctx, false);
    let mut backend = Backend::new(&ctx);

    let module = parser.parse_program();
    if ctx.flush_diagnostics(&source) {
        return;
    }

    let package = generator.emit_iiv(&[module]);
    if ctx.flush_diagnostics(&source) {
        return;
    }

    for fun in &package.funcs {
        // eprintln!("{}", fun.borrow());
        iiv::move_check::copies_to_moves(
            &mut *fun.borrow_mut(),
            ctx.builtins.get_copy(),
            &package.impl_forest,
        );
        iiv::move_check::check(&ctx, *fun);
        // eprintln!("{}", fun.borrow());
    }

    if ctx.flush_diagnostics(&source) {
        return;
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
    //     eprintln!("no link");
    // }
}
