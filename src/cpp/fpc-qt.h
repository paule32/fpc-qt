// ---------------------------------------------------------------------------
// File:   fpc-qt.h
// Author: (c) 2024 Jens Kallup - paule32
// All rights reserved
// ---------------------------------------------------------------------------
# pragma once
#ifndef  __FPC_QT_H__
# define __FPC_QT_H__
# include <windows.h>

# include <iostream>
# include <string>
# include <variant>
# include <map>
# include <memory>
# include <cwchar>

#ifdef _WIN64
# define DLL_EXPORT __declspec(dllexport)
#else
# define DLL_EXPORT
#endif

/**
 * \brief  Diese Bibliothek nutzt das grafische Qt5 GUI-Framework.
 */
# include <QtCore/QChar>

/**
 * \namespace qvc
 * \brief     Ein Namespace, um Seiteneffekte mit anderen Frameworks zu vermeiden.
 * \details   QVC steht für dieses Projekt für "Qt Visual Components" als Ansammlung
 *            von Klassen, die mit dem FPC (Free Pascal Compiler) wie auch mit dem
 *            GNU C++ Compiler genutzt werden können um eine einheitliche Code-Basis
 *            zu erhalten.
 * \since     Version 0.0.1
 */
namespace qvc
{

/**
 * \enum    symbolType
 * \brief   Aufzählungstyp für unterstützte Eigenschaften
 * \details In der Symbol-Tabelle wird der Name eines jeden Symbols abgelegt.
 *          Damit jedem Symbol eine Bedeutung zugeordnet werden kann, wird ein
 *          eindeutiger Typ bereit gestellt.
 */
enum symbolTypeEnum
{
    /// Symboltype ist nicht bekannt.
    stUnknown = -1,
    /// ist eine Funktion
    stFunction = 1,
    /// ist eine Prozedur
    stProcedure = 2,
    /// entspricht C/C++ void
    stVOID = 3,
    /// entspricht einen WideChar (2 Byte)
    stWideChar = 4,
    /// Qt Klasse - QChar
    stQChar = 100
};

/**
 * \struct  TypeTypes
 * \brief   Intern verwendete Struktur für das Speichern von Datentypen, um die
 *          Auswahl der entsprechenden Aktionen auf Grundlage des gegebenen Typs
 *          zu verarbeiten.
 * \details Intern werden für jedes Symbol ein Name sowie ein Typ in einer std::map
 *          vorrätig gehalten, um eine spätere Auswertung zu gewährleisten wenn
 *          Typen zwischen Pascal und C++ ausgetauscht werden.
 * \since   Version 0.0.1
 */
struct TypeTypes
{
    /** Container für mögliche Typen für ein Symbol */
    std::variant<
    int,
    float,
    std::wstring,
    QChar> value;
    
    /// repräsentiert einen Integer Wert
    TypeTypes(int v) : value(v) {}
    /// repräsentiert eine Fließkomma Zahl
    TypeTypes(float v) : value(v) {}
    /// repräsentiert einen WideString
    TypeTypes(const std::wstring& v) : value(v) {}
    TypeTypes(const QChar& v): value(v) { }
};

/**
 * \brief   Diese Symbol-Zuordnungs-Tabelle hält die Symbole mit entsprechenden Typ.
 * \details Der Typ den jedes Symbol annehmen kann, wird mit eines TypeTypes-Eintrag
 *          festgelegt. Für jedes Symbol kann immer nur ein Typ vergeben werden.
 */
extern std::map<std::wstring, std::unique_ptr<TypeTypes>> symbol_map;

/**
 * \class   symbolHandler
 * \brief   Behandelt ein Symbol mit erweiterten Funktionen.
 * \details Wenn es nötig erscheint einen Symbol-Typen weitere Funktionen mit
 *          zugeben, dient die Klasse symbolHandler als konkrete Anlaufstelle.
 */
class symbolHandler {
public:
    symbolTypeEnum   symType;
    std::wstring     symName;
};

extern "C" {
/**
 * \defgroup internFunctions interne Funktionen
 * @{
 */
/**
 * \defgroup qcharclass QChar
 * \ingroup  internFunctions
 * \brief    Interne Funktionen für die Klasse QChar
 */
/**
 * \ingroup  qcharclass CreateQChar
 * \brief    Erstellt eine Inztanz einer QChar Klasse
 */
bool CreateQChar(wchar_t* p_name);


/**
 * \defgroup qstringclass QString
 * \ingroup  internFunctions
 * \brief    Interne Funktionen für die Klasse QString
 */
/**
 * \ingroup  qstringclass CreateQString
 * \brief    Erstellt eine Inztanz einer QString Klasse
 */
bool CreateQString(wchar_t* p_name);
/** @} */  // Ende: internFunctions

};      // ectern:    "C"
}       // namespace: qvc
#endif  // header:    __FPC_QT_H__
