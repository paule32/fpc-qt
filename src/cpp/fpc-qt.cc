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
        //char ch = 32;
        qvc::QChar* qc = new qvc::QChar();
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
            }   else if constexpr (std::is_same_v<T, QChar*>) {
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
        
        int res = MessageBoxW(
            NULL,
            result.c_str(),
            title .c_str(),
            MB_OK | MB_ICONINFORMATION
        );
        
        return false;
    }   return false;
}

qvc::QChar::QChar(void      ) { }
qvc::QChar::QChar(char     t) { }
qvc::QChar::QChar(uint8_t  t) { }
qvc::QChar::QChar(uint16_t t) { }
qvc::QChar::QChar(uint32_t t) { }
qvc::QChar::QChar(wchar_t  t) { }

void qvc::QChar::setType(char     t) { qchar_types = static_cast<char    >( t ); }
void qvc::QChar::setType(uint8_t  t) { qchar_types = static_cast<uint8_t >( t ); }
void qvc::QChar::setType(uint16_t t) { qchar_types = static_cast<uint16_t>( t ); }
void qvc::QChar::setType(uint32_t t) { qchar_types = static_cast<uint32_t>( t ); }
void qvc::QChar::setType(wchar_t  t) { qchar_types = static_cast<wchar_t >( t ); }

symbolTypeEnum
qvc::QChar::getType(void) const
{
    symbolTypeEnum result = stUnknown;
    
    if (std::holds_alternative<char    >(qchar_types)) result = stQChar_Char; else
    if (std::holds_alternative<uint8_t >(qchar_types)) result = stQChar_Char; else
    if (std::holds_alternative<uint16_t>(qchar_types)) result = stQChar_Char; else
    if (std::holds_alternative<uint32_t>(qchar_types)) result = stQChar_Char; else
    if (std::holds_alternative<wchar_t >(qchar_types)) result = stQChar_Char; else {
        MessageBoxW(
            NULL,
            L"Error: type out of bounds.",
            L"Error",
            MB_ICONINFORMATION | MB_OK
        );
        ExitProcess(1);
    }
    return result;
}

bool
qvc::QChar::isDigit()
{
    bool result = true;
    return result;
}

qvc::QChar::~QChar(void) {
    std::wcout << L"dtor: QChar" << std::endl;
}

extern "C" {
DLL_EXPORT uint64_t
ctor_QChar(wchar_t* p_name, uint32_t sym_type)
{
    Iaddsymbol(p_name, sym_type);
    Igetsymbol(p_name);
    
    std::wcout
    << L"create: QChar: 0x" << std::hex
    << current_ptr
    << std::endl;
    
    return current_ptr;
}
DLL_EXPORT void
dtor_QChar(uint64_t addr)
{
    QChar* objptr = reinterpret_cast<QChar*>(addr);
    delete objptr;
}

DLL_EXPORT bool
isDigit_QChar(uint64_t addr)
{
    bool   result = false;
    QChar* objptr = reinterpret_cast<QChar*>(addr);

    if (objptr->isDigit())
    result = true;

    return result;
}
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
