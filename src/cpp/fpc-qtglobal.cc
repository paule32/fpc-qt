// ---------------------------------------------------------------------------
// File:   fpc-qtglobal.cc
// Author: (c) 2024 Jens Kallup - paule32
// All rights reserved
// ---------------------------------------------------------------------------
# include "fpc-qt.h"

namespace qvc {

uint8_t PascalCompiler;

extern "C" {
DLL_EXPORT void
SetPascalCompiler(uint8_t pc) {
    qvc::PascalCompiler = pc;
}

DLL_EXPORT uint8_t
GetPascalCompiler(void) {
    return qvc::PascalCompiler;
}

};  // extern "C"
}   // namespace: qvc
