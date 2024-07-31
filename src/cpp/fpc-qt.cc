// ---------------------------------------------------------------------------
// File:   fpc-qt.cc
// Author: (c) 2024 Jens Kallup - paule32
// All rights reserved
// ---------------------------------------------------------------------------
# include <windows.h>
# include <iostream>
# include <string>
# include <map>

#ifdef _WIN64
    #define DLL_EXPORT __declspec(dllexport)
#else
    #define DLL_EXPORT
#endif

enum symbolType {
    stFunction,
    stProcedure
};

class symbolHandler {
public:
    symbolType   symType;
    std::wstring symName;
};

std::map< std::wstring, symbolHandler* > symbol_map;

extern "C" {
DLL_EXPORT void TestFunction(wchar_t* p_name) {
    auto* sym = new symbolHandler;
    sym->symType = symbolType::stProcedure;
    sym->symName = p_name;
    
    symbol_map[ p_name ] = sym;
    
    for (const auto& [key, value] : symbol_map) {
        if (key.compare( p_name) == 0) {
            std::wcout << "found: ";
            std::wcout << p_name << std::endl;
            if (value->symType == symbolType::stProcedure) {
                std::wcout << "is a procedure" << std::endl;
            }   else {
                std::wcout << "is not a procedure" << std::endl;
            }
            break;
        }
    }
}
};
