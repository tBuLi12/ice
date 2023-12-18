#include <corecrt_malloc.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>

static int32_t* gen_table = nullptr;
static std::vector<int32_t*>* frees = nullptr;

extern "C" void rt_init() {
    printf("try init\n");
    if (gen_table == nullptr) {
        printf("rt_init\n");
        gen_table = (int32_t*)malloc(512);
        frees = new std::vector<int32_t*>{};
        for (int i = 0; i < 128; i++) {
            gen_table[i] = 0;
            frees->push_back(&gen_table[i]);
        }
    }
}

extern "C" void rt_invalidate(int32_t** gen) {
    printf("rt_invalidate\n");
    if (*gen != nullptr) {
        printf("do invalidate\n");
        **gen += 1;
        frees->push_back(*gen);
        *gen = nullptr;
    }
}

extern "C" void rt_gen_alloc(int32_t** gen) {
    printf("rt_gen_alloc\n");
    if (*gen == nullptr) {
        if (frees->size() == 0) {
            printf("too many gen refs\n");
            exit(1);
        }
        *gen = frees->back();
        frees->pop_back();
    }
}

extern "C" void rt_validate(int32_t* gen, int32_t val) {
    printf("rt_validate\n");
    if (*gen != val) {
        printf("invalid reference\n");
        exit(1);
    }
}
