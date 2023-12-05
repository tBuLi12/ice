use std::path::Path;

fn main() {
    let path_str = std::env::var("LLVMInstallDir").expect("LLVMInstallDir must be set");
    let llvm_path = Path::new(&path_str);

    let build_dir = llvm_path.parent().unwrap();
    let include = build_dir
        .parent()
        .unwrap()
        .join("llvm")
        .join("llvm")
        .join("include");
    let include2 = build_dir.join("include");

    println!("cargo:rerun-if-changed=./wrapper/wrapper.cpp");

    cc::Build::new()
        .cpp(true)
        .std("c++20")
        .include(include)
        .include(include2)
        .file("./wrapper/wrapper.cpp")
        .warnings(false)
        .compile("llvm-wrapper");

    let libs = [
        "LLVMWindowsManifest",
        "LLVMXRay",
        "LLVMLibDriver",
        "LLVMDlltoolDriver",
        "LLVMCoverage",
        "LLVMLineEditor",
        "LLVMX86TargetMCA",
        "LLVMX86Disassembler",
        "LLVMX86AsmParser",
        "LLVMX86CodeGen",
        "LLVMX86Desc",
        "LLVMX86Info",
        "LLVMOrcJIT",
        "LLVMWindowsDriver",
        "LLVMMCJIT",
        "LLVMJITLink",
        "LLVMInterpreter",
        "LLVMExecutionEngine",
        "LLVMRuntimeDyld",
        "LLVMOrcTargetProcess",
        "LLVMOrcShared",
        "LLVMDWP",
        "LLVMDebugInfoLogicalView",
        "LLVMDebugInfoGSYM",
        "LLVMOption",
        "LLVMObjectYAML",
        "LLVMObjCopy",
        "LLVMMCA",
        "LLVMMCDisassembler",
        "LLVMLTO",
        "LLVMPasses",
        "LLVMCFGuard",
        "LLVMCoroutines",
        "LLVMipo",
        "LLVMVectorize",
        "LLVMLinker",
        "LLVMInstrumentation",
        "LLVMFrontendOpenMP",
        "LLVMFrontendOpenACC",
        "LLVMFrontendHLSL",
        "LLVMExtensions",
        "LLVMDWARFLinker",
        "LLVMGlobalISel",
        "LLVMMIRParser",
        "LLVMAsmPrinter",
        "LLVMSelectionDAG",
        "LLVMCodeGen",
        "LLVMObjCARCOpts",
        "LLVMIRPrinter",
        "LLVMInterfaceStub",
        "LLVMFileCheck",
        "LLVMFuzzMutate",
        "LLVMTarget",
        "LLVMScalarOpts",
        "LLVMInstCombine",
        "LLVMAggressiveInstCombine",
        "LLVMTransformUtils",
        "LLVMBitWriter",
        "LLVMAnalysis",
        "LLVMProfileData",
        "LLVMSymbolize",
        "LLVMDebugInfoPDB",
        "LLVMDebugInfoMSF",
        "LLVMDebugInfoDWARF",
        "LLVMObject",
        "LLVMTextAPI",
        "LLVMMCParser",
        "LLVMIRReader",
        "LLVMAsmParser",
        "LLVMMC",
        "LLVMDebugInfoCodeView",
        "LLVMBitReader",
        "LLVMFuzzerCLI",
        "LLVMCore",
        "LLVMRemarks",
        "LLVMBitstreamReader",
        "LLVMBinaryFormat",
        "LLVMTargetParser",
        "LLVMTableGen",
        "LLVMSupport",
        "LLVMDemangle",
    ];

    println!(
        "cargo:rustc-link-search={}",
        llvm_path.join("lib").display()
    );

    for lib in libs {
        println!("cargo:rustc-link-lib={}", lib);
    }
}
