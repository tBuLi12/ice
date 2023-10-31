use std::{env, fs::File, io::BufReader};

use ice_parser::Parser;
use iiv_cl::Backend;
use iiv_gen::Generator;

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
    }

    backend.transform(&package, "out.o");
}
