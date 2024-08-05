// ---------------------------------------------------------------------------
// File:   fpc-qtchar.cc
// Author: (c) 2024 Jens Kallup - paule32
// All rights reserved
// ---------------------------------------------------------------------------
# include "fpc-qt.h"

namespace qvc {

extern "C" {
bool CreateQChar(wchar_t* p_name) {
    /*
    ReturnResult * rr = new ReturnResult();
    
    auto * qc = new QChar();
    symbolVariant sv = qc;
    
    rr->set(std::wstring(p_name));
    rr->set(symbolVariantEnum::svQChar);
    
    symbol_map[ p_name ] = rr;
    */
    return true;
}   // CreateQChar(void)

};  // extern "C"
}   // namespace: qvc
