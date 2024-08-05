// ---------------------------------------------------------------------------
// \file       fpc-qt.cc
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
# include "fpc-qt.h"

namespace qvc {
std::map<std::wstring, std::unique_ptr<TypeTypes>> symbol_map;

/**
 * \brief  Fügt ein neues Symbol, das mit p_name angegeben wurde, zu der intern
 *         genutzten Symbol-Table hinzu.
 *         e_value ist ein Aufzählungstyp für das jeweilige Symbol.
 * \param  p_sname - std::wstring  => Der Symbol-Name.
 * \param  e_value - enum          => Der Aufzählungstyp für das Symbol.
 */
void
addSymbol(
    const std::wstring& p_sname,
    symbolVariantEnum   e_value) {
    symbol_map[p_sname] =
        std::make_unique<TypeTypes>(
        symbolVariantEnum(e_value)
        );
}

/**
 * \brief  Sucht nach einen internen Symbol, welches als WideString über den
 *         Parameter p_sname angegeben wird. Wurde kein entsprechendes Symbol
 *         gefunden, wird false zurück gegeben; ansonsten true.
 * \param  p_sname - std::wstring&&  => Das zu suchende Symbolwort.
 * \return bool - ein Boolscher Wert => False oder True.
 */
bool
getSymbol(std::wstring&& p_sname) {
    auto it = symbol_map.find(p_sname);
    if (it != symbol_map.end()) {
        const auto& value = it->second->value;

        std::visit([](auto&& arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, int>) {
                std::wcout << L"Integer: " << arg << std::endl;
                return true;
            } else if constexpr (std::is_same_v<T, float>) {
                std::wcout << L"Float: " << arg << std::endl;
                return true;
            } else if constexpr (std::is_same_v<T, std::wstring>) {
                std::wcout << L"String: " << arg << std::endl;
                return true;
            }
        }, value);
    }   else {
        std::wcout << L"Symbol not found: " << p_sname << std::endl;
        return false;
    }
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
addInternalSymbol(wchar_t* p_name, size_t cc) {
    addSymbol(p_name);
    getSymbol(p_name);
}
};

}   // namespace: qvc
// ---------------------------------------------------------------------------
// E O F  -  End Of File.
// ---------------------------------------------------------------------------
