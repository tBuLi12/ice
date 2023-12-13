
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"
#include "llvm/Transforms/Scalar/SROA.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker/Linker.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include <stdint.h>

static constexpr int32_t cannotOpenFile = 1;
static constexpr int32_t cannotEmit = 2;
static constexpr int32_t invalidTarget = 3;

extern "C" int32_t getTargetMachine(llvm::LLVMContext* ctx, llvm::TargetMachine** targetMachine) {
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();
    std::string targetErr;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, targetErr);
    if (!target) {
        return invalidTarget;
    }
    *targetMachine =
        target->createTargetMachine(targetTriple, "generic", "", {}, {});
    
    return 0;
}

extern "C" int32_t emitModule(llvm::Module* llvmModule, llvm::TargetMachine* targetMachine, char const* name, uint32_t name_len) {
    llvmModule->print(llvm::outs(), nullptr);
    
    llvm::legacy::PassManager passManager{};

    std::error_code errCode;
    llvm::raw_fd_ostream outputFile{llvm::StringRef(name, name_len), errCode, llvm::sys::fs::OF_None};
    if (errCode) {
        return cannotOpenFile;
    }
    if (targetMachine->addPassesToEmitFile(
            passManager, outputFile, nullptr, llvm::CGFT_ObjectFile
        )) {
        return cannotEmit;
    }

    passManager.run(*llvmModule);
    outputFile.flush();

    return 0;
}

extern "C" llvm::Module* getMod(llvm::LLVMContext* ctx, llvm::TargetMachine* targetMachine) {
    auto mod = new llvm::Module("main", *ctx);
    mod->setDataLayout(targetMachine->createDataLayout());
    mod->setTargetTriple(targetMachine->getTargetTriple().str());
    return mod;
}

extern "C" void destroyMod(llvm::Module* mod) {
    delete mod;    
}

extern "C" llvm::IRBuilder<>* getBuilder(llvm::LLVMContext* ctx) {
    return new llvm::IRBuilder<>(*ctx);
}

extern "C" void destroyBuilder(llvm::IRBuilder<>* builder) {
    delete builder;    
}


extern "C" llvm::LLVMContext* getCtx() {
    return new llvm::LLVMContext();
}

extern "C" void destroyCtx(llvm::LLVMContext* ctx) {
    delete ctx;    
}

extern "C" void phiAddIncomming(llvm::PHINode* phi, llvm::BasicBlock* block, llvm::Value* val) {
    phi->addIncoming(val, block);
}

extern "C" llvm::Constant* ctxGetInt(llvm::LLVMContext* ctx, uint64_t val) {
    return llvm::ConstantInt::get(*ctx, llvm::APInt(32, val));
}

extern "C" llvm::Constant* ctxGetBool(llvm::LLVMContext* ctx, bool val) {
    return llvm::ConstantInt::get(*ctx, llvm::APInt(1, val));
}

extern "C" llvm::Type* ctxGetTyInt(llvm::LLVMContext* ctx) {
    return llvm::Type::getInt32Ty(*ctx);
}

extern "C" llvm::Type* ctxGetTyBool(llvm::LLVMContext* ctx) {
    return llvm::Type::getInt1Ty(*ctx);
}

extern "C" llvm::Type* ctxGetTyPtr(llvm::LLVMContext* ctx) {
    return llvm::PointerType::get(*ctx, 0);
}

extern "C" llvm::Type* ctxCreateStructTy(
        llvm::LLVMContext* ctx,
        char const* name,
        uint32_t name_len,
        llvm::Type* const* types,
        uint32_t types_len
    ) {
        return llvm::StructType::create(*ctx, llvm::ArrayRef(types, types_len), llvm::StringRef(name, name_len));
    }


extern "C" llvm::Type* ctxGetStructTy(
        llvm::LLVMContext* ctx,
        llvm::Type* const* types,
        uint32_t types_len
    ) {
        return llvm::StructType::get(*ctx, llvm::ArrayRef(types, types_len));
    }

extern "C" llvm::Type* ctxGetArrayTy(llvm::LLVMContext* _, llvm::Type* elem_ty, uint64_t size) {
    return llvm::ArrayType::get(elem_ty, size);
}

extern "C" llvm::Constant* ctxArrayConstant(
        llvm::LLVMContext* ctx,
        llvm::Type* elem_ty,
        llvm::Constant* const* vals,
        uint32_t vals_len
    ) {
        return llvm::ConstantArray::get(llvm::ArrayType::get(elem_ty, vals_len), llvm::ArrayRef(vals, vals_len));
    }

extern "C" llvm::BasicBlock* ctxCreateBlock(
        llvm::LLVMContext* ctx
    ) {
        return llvm::BasicBlock::Create(*ctx);
    }

extern "C" llvm::Type* valueType(llvm::Value* val) {
    return val->getType();
}

extern "C" llvm::Function* modCreateFunc(
        llvm::Module* module,
        char const* name,
        uint32_t name_len,
        llvm::Type* const* types,
        uint32_t types_len,
        llvm::Type* ret_ty
    ) {
        auto funcType =
            llvm::FunctionType::get(ret_ty, llvm::ArrayRef(types, types_len), false);
        auto llvmFunc = llvm::Function::Create(
            funcType, llvm::Function::ExternalLinkage, llvm::StringRef(name, name_len), module
        );
        return llvmFunc;
    }

extern "C" uint64_t modAllocSizeOf(llvm::Module* mod, llvm::Type* ty) {
    return mod->getDataLayout().getTypeAllocSize(ty);
}

extern "C" llvm::GlobalVariable* modGetOrCreateGlobal(
        llvm::Module* mod,
        char const* name,
        uint32_t name_len,
        llvm::Type* ty
    ) {
        auto nameRef = llvm::StringRef(name, name_len);
        mod->getOrInsertGlobal(nameRef, ty);
        return mod->getNamedGlobal(nameRef);
    }


extern "C" bool globalHasInitializer(llvm::GlobalVariable* global) {
    return global->hasInitializer();
}
extern "C" void globalSetInitializer(llvm::GlobalVariable* global, llvm::Constant* init) {
    return global->setInitializer(init);
}

extern "C" llvm::PHINode* builderPhi(llvm::IRBuilder<>* builder, llvm::Type* ty) {
    return builder->CreatePHI(ty, 0);
}

extern "C" llvm::Value* builderAdd(
        llvm::IRBuilder<>* builder,
        llvm::Value* lhs,
        llvm::Value* rhs
    ) {
        return builder->CreateAdd(lhs, rhs);
    }
extern "C" llvm::Value* builderEq(
        llvm::IRBuilder<>* builder,
        llvm::Value* lhs,
        llvm::Value* rhs
    ) {
        return builder->CreateICmpEQ(lhs, rhs);
    }
extern "C" llvm::Value* builderCall(
        llvm::IRBuilder<>* builder,
        llvm::Function* func,
        llvm::Value* const* args,
        uint32_t args_len
    ) {
        return builder->CreateCall(func, llvm::ArrayRef(args, args_len));
    }
extern "C" void builderCondBr(
        llvm::IRBuilder<>* builder,
        llvm::Value* cond,
        llvm::BasicBlock* yes,
        llvm::BasicBlock* no
    ) {
        builder->CreateCondBr(cond, yes, no);
    }
extern "C" llvm::Value* builderLoad(
        llvm::IRBuilder<>* builder,
        llvm::Value* ptr,
        llvm::Type* ty
    ) {
        return builder->CreateLoad(ty, ptr);
    }
extern "C" void builderStore(llvm::IRBuilder<>* builder, llvm::Value* ptr, llvm::Value* src) {
    builder->CreateStore(src, ptr);
}
extern "C" void builderBr(llvm::IRBuilder<>* builder, llvm::BasicBlock* target) {
    builder->CreateBr(target);
}

extern "C" void builderRet(llvm::IRBuilder<>* builder, llvm::Value* val) {
    builder->CreateRet(val);
}
extern "C" llvm::Value* builderGep(
        llvm::IRBuilder<>* builder,
        llvm::Type* ty,
        llvm::Value* base,
        llvm::Value* const* offsets,
        uint32_t offsets_len
    ) {
        return builder->CreateInBoundsGEP(ty, base, llvm::ArrayRef(offsets, offsets_len));
    }
extern "C" llvm::BasicBlock* builderCurrentBlock(llvm::IRBuilder<>* builder) {
    return builder->GetInsertBlock();
}
extern "C" llvm::Value* builderCreateAlloca(llvm::IRBuilder<>* builder, llvm::Type* ty) {
        auto& entryBlock =
            builder->GetInsertBlock()->getParent()->getEntryBlock();
        llvm::IRBuilder<> tempBuilder(&entryBlock, entryBlock.begin());
        return tempBuilder.CreateAlloca(ty);
}

extern "C" void builderSetInsertPoint(llvm::IRBuilder<>* builder, llvm::BasicBlock* block) {
    builder->SetInsertPoint(block);
}

extern "C" void functionAppendBlock(llvm::Function* func, llvm::BasicBlock* block) {
    func->insert(func->end(), block);
}

extern "C" bool functionVerify(llvm::Function* func) {
    return llvm::verifyFunction(*func, &llvm::errs());
}

extern "C" uint64_t functionArgSize(llvm::Function* func) {
    return func->arg_size();
}

extern "C" llvm::Value* functionArgAt(llvm::Function* func, uint32_t idx) {
    return func->getArg(idx);
}

struct FunctionOptManager {
    llvm::FunctionAnalysisManager fam;
    // llvm::CGSCCAnalysisManager cam;
    // llvm::LoopAnalysisManager lam;
    llvm::FunctionPassManager fpm;
};

extern "C" FunctionOptManager* getFunctionOptManager(llvm::TargetMachine* machine) {
    auto fom = new FunctionOptManager {
        llvm::FunctionAnalysisManager(),
        // llvm::CGSCCAnalysisManager(),
        // llvm::LoopAnalysisManager(),
        llvm::FunctionPassManager(),
    };
    fom->fpm.addPass(llvm::PromotePass());
    auto pm = llvm::PassBuilder(machine);
    pm.registerFunctionAnalyses(fom->fam);
    return fom;
}

extern "C" void destroyFunctionOptManager(FunctionOptManager* funOptManager) {
    delete funOptManager;
}

extern "C" void functionOptManagerOptimize(
    FunctionOptManager* funOptManager,
    llvm::Function* func
) {
    funOptManager->fpm.run(*func, funOptManager->fam);
}
