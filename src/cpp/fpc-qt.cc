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
void
Iaddsymbol(const std::wstring& p_sname, uint32_t value)
{
    if (value == 1) {
        symbol_map[p_sname] = std::make_unique<TypeTypes>(value);
    }   else if (value == symbolTypeEnum::stQChar) {
        char ch = 32;
        qvc::QChar* qc = new qvc::QChar(ch);
        map_QChar.push_back(qc);
        symbol_map[p_sname] = std::make_unique<TypeTypes>(qc);
    }   else if (value == symbolTypeEnum::stQChar_Byte) {
        uint8_t ch = 32;
        qvc::QChar* qc = new qvc::QChar(ch);
        map_QChar.push_back(qc);
        symbol_map[p_sname] = std::make_unique<TypeTypes>(qc);
    }   else if (value == symbolTypeEnum::stQChar_AnsiChar) {
        uint8_t ch = 32;
        qvc::QChar* qc = new qvc::QChar(ch);
        map_QChar.push_back(qc);
        symbol_map[p_sname] = std::make_unique<TypeTypes>(qc);
    }   else if (value == symbolTypeEnum::stQChar_WideChar) {
        uint16_t ch = 32;
        qvc::QChar* qc = new qvc::QChar(ch);
        map_QChar.push_back(qc);
        symbol_map[p_sname] = std::make_unique<TypeTypes>(qc);
    }   else if (value == symbolTypeEnum::stQChar_Word) {
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
        
        QString qstr = QString::fromWCharArray(
            result.c_str(),
            result.size()
        );
        
        ErrorMessage(qstr);
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
    Iaddsymbol(p_name, 1);
    Igetsymbol(p_name);
}
};

}   // namespace: qvc
// ---------------------------------------------------------------------------
// E O F  -  End Of File.
// ---------------------------------------------------------------------------
