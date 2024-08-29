// ---------------------------------------------------------------------------
// \file       fpc-qt.cc
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
# include "fpc-qt.h"

namespace qvc {
std::map<std::wstring, std::unique_ptr<TypeTypes>> symbol_map;

uint64_t current_ptr = 0;
std::vector< qvc::QChar* > map_QChar;

/**
 * \brief  Fügt ein neues Symbol, das mit p_name angegeben wurde, zu der intern
 *         genutzten Symbol-Table hinzu.
 *         e_value ist ein Aufzählungstyp für das jeweilige Symbol.
 * \param  p_sname - std::wstring  => Der Symbol-Name.
 * \param  e_value - enum          => Der Aufzählungstyp für das Symbol.
 */
uint64_t
Iaddsymbol(const std::wstring& p_sname, uint32_t value, uint64_t addr)
{
    current_ptr = 0;
    std::cout << "uint32_t:  " << value << std::endl;

    if (value == 1) {
        symbol_map[p_sname] = std::make_unique<TypeTypes>(value);
    }   else if (value == symbolTypeEnum::stQChar) {
        char ch = 32;
        qvc::QChar* qc = new qvc::QChar(ch);
        map_QChar.push_back(qc);
        symbol_map[p_sname] = std::make_unique<TypeTypes>(qc);
    }   else if (value == symbolTypeEnum::stQChar_Byte) {
        uint8_t ch = '1';
        std::cout << "memvar: " << std::hex << addr << std::endl;
        std::cout << "memval: " << std::dec << reinterpret_cast<uint64_t>(addr) << std::endl;
        qvc::QChar* qc = new qvc::QChar(ch);
        map_QChar.push_back(qc);
        symbol_map[p_sname] = std::make_unique<TypeTypes>(qc);
    }
    else if (value == symbolTypeEnum::stQChar_AnsiChar) {
        wchar_t * c = new wchar_t[16];
        wcscpy(c, reinterpret_cast<wchar_t*>(addr));
        uint8_t  ch = _wtoi(c);
        delete c;
        std::wcout << L"ansiChar: " << ch << std::endl;
        qvc::QChar* qc = new qvc::QChar(ch);
        uint64_t    qu = reinterpret_cast<uint64_t>((void*)qc);
        current_ptr    = qu;
        std::wcout << L"qc: " << std::hex << qu << std::endl;
        return current_ptr;
    }
    else if (value == symbolTypeEnum::stQChar_WideChar) {
        wchar_t * c = new wchar_t[16];
        wcscpy(c, reinterpret_cast<wchar_t*>(addr));
        uint16_t  ch = _wtoi(c);
        delete c;
        std::wcout << L"wideChar: " << ch << std::endl;
        qvc::QChar* qc = new qvc::QChar(ch);
        uint64_t    qu = reinterpret_cast<uint64_t>((void*)qc);
        current_ptr    = qu;
        std::wcout << L"qc: " << std::hex << qu << std::endl;
        return current_ptr;
    }
    else if (value == symbolTypeEnum::stQChar_Word) {
        WORD ch = 32;
        qvc::QChar* qc = new qvc::QChar((WORD)ch);
        qc->setType(ch);
        map_QChar.push_back(qc);
        symbol_map[p_sname] = std::make_unique<TypeTypes>(qc);
    }   else if (value == symbolTypeEnum::stQChar_ShortInt) {
        short ch = 32;
        qvc::QChar* qc = new qvc::QChar((short)ch);
        qc->setType(ch);
        map_QChar.push_back(qc);
        symbol_map[p_sname] = std::make_unique<TypeTypes>(qc);
    }
    return current_ptr;
}

/**
 * \brief  Sucht nach einen internen Symbol, welches als WideString über den
 *         Parameter p_sname angegeben wird. Wurde kein entsprechendes Symbol
 *         gefunden, wird false zurück gegeben; ansonsten true.
 * \param  p_sname - std::wstring&&  => Das zu suchende Symbolwort.
 * \return bool - ein Boolscher Wert => False oder True.
 */
bool
Igetsymbol(std::wstring&& p_sname) {
    auto it = symbol_map.find(p_sname);
    if (it != symbol_map.end()) {
        const auto& value = it->second->value;

        std::visit([](auto&& arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, uint32_t>) {
                std::wcout << L"Integer: " << arg << std::endl;
                return true;
            }   else if constexpr (std::is_same_v<T, float>) {
                std::wcout << L"Float: " << arg << std::endl;
                return true;
            }   else if constexpr (std::is_same_v<T, std::wstring>) {
                std::wcout << L"String: " << arg << std::endl;
                return true;
            }   else if constexpr (std::is_same_v<T, qvc::QChar*>) {
                current_ptr = reinterpret_cast<uint64_t>(&arg);
                return true;
            }
        }, value);
        
        return false;
    }   else {
        std::wstring part1 = L"Symbol not found: ";
        std::wstring part2 = p_sname;
        std::wstring title = L"Inforamtion";
        
        std::wstring&& part2_rvalue = std::move(part2);
        std::wstring   result       = std::wstring(part1) + part2_rvalue;
        
        const wchar_t * input = result.c_str();
        size_t size   = (wcslen(input) + 1) * sizeof(wchar_t);
        char * buffer = new char[size];
        
        #ifdef FPC
            std::wcstombs(buffer, input, size);
            ErrorMessage(buffer);
        #else
            ErrorMessage(result.c_str());
        #endif
        
        return false;
    }   return false;
}

extern "C" {
/**
 * \brief   Intern genutzte Funktion zum hinzufügen eines Symbols zu der internen
 *          Symboltabelle.
 *
 * \param   p_name - wchar_t* => Ein WideChar Typ.
 *
 * \version 0.0.1
 * \see addSymbol(), getSymbol()
 */
DLL_EXPORT void
addsymbol(wchar_t* p_name, uint32_t cc)
{
    Iaddsymbol(p_name, 1, 0);
    Igetsymbol(p_name);
}
};

}   // namespace: qvc
// ---------------------------------------------------------------------------
// E O F  -  End Of File.
// ---------------------------------------------------------------------------
