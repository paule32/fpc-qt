// ---------------------------------------------------------------------------
// \file       fpc-qt.cc
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
# include "fpc-qt.h"

namespace qvc {
uint64_t current_ptr = 0;

/**
 * \brief  Fügt ein neues Symbol, das mit p_name angegeben wurde, zu der intern
 *         genutzten Symbol-Table hinzu.
 *         e_value ist ein Aufzählungstyp für das jeweilige Symbol.
 * \param  p_sname - std::wstring  => Der Symbol-Name.
 * \param  e_value - enum          => Der Aufzählungstyp für das Symbol.
 */
uint64_t
Iaddsymbol(char* p_sname, struct qvc::ResultVHelper *addr)
{
    current_ptr = 0;
    if (addr->VType2.VType == symbolTypeEnum::stQChar) {
        auto   * qc = new qvc::QChar<char>();
        qc->ptr_val = addr;
        current_ptr = reinterpret_cast<uint64_t>((void*)qc);
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
    return false;
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
    //Iaddsymbol(p_name, 1, 0);
    //Igetsymbol(p_name);
}
};

}   // namespace: qvc
// ---------------------------------------------------------------------------
// E O F  -  End Of File.
// ---------------------------------------------------------------------------
