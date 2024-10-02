// ---------------------------------------------------------------------------
// File:   fpc-qt.h
// Author: (c) 2024 Jens Kallup - paule32
// All rights reserved
// ---------------------------------------------------------------------------
# pragma once
#ifndef  __FPC_QT_H__
# define __FPC_QT_H__

#ifdef WINDOWS
# include <windows.h>
#endif

# include <stdio.h>
# include <wchar.h>         // wideChar: wscpy

# include <iostream>
# include <string>
# include <variant>
# include <map>
# include <vector>
# include <memory>
# include <cwchar>
# include <exception>
# include <locale>
# include <codecvt>

#ifndef WINDOWS
typedef uint16_t WORD;
typedef uint32_t DWORD;
#endif

#ifdef _WIN64
# define DLL_EXPORT __declspec(dllexport)
#else
# define DLL_EXPORT
#endif

/**
 * \brief  Diese Bibliothek nutzt das grafische Qt5 GUI-Framework.
 */
# include <QtCore/QChar>
# include <QtCore/QString>
# include <QtWidgets/QApplication>
# include <QtWidgets/QMessageBox>

/**
 * \brief  Benutzerdefinierte Exception-Klasse mit Konvertierung von std::wstring
 *         in std::string
 */
class MyBaseException : public std::exception {
private:
    std::wstring wErrorMessage;
    std::string errorMessage; // Konvertierte std::string Version

    // Helferfunktion zur Konvertierung von std::wstring zu std::string
    std::string wstringToString(const std::wstring& wstr) const {
        std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> converter;
        return converter.to_bytes(wstr);
    }

public:
    // Konstruktor mit std::wstring
    MyBaseException(const std::string& message)
        : errorMessage(message)
        {}
    MyBaseException(const std::wstring& message)
        : wErrorMessage(message), errorMessage(wstringToString(message))
        {}

    // Überschreibe what() für std::string Rückgabe
    virtual const char* what() const noexcept override {
        return errorMessage.c_str();
    }

    // Methode für std::wstring Rückgabe
    const std::wstring& wwhat() const noexcept {
        return wErrorMessage;
    }
};

class ERangeError: public MyBaseException {
public:
    ERangeError(const std::wstring& message) : MyBaseException(message) {}
    ERangeError(const std:: string& message) : MyBaseException(message) {}
};

class ETypeError: public MyBaseException {
public:
    ETypeError(const std::wstring& message) : MyBaseException(message) {}
    ETypeError(const std:: string& message) : MyBaseException(message) {}
};


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

struct ClassVHelper {
    uint32_t VType  ;
    
    uint8_t  Value_u1;   // unsigned:   8-bit
    uint16_t Value_u2;   // unsigned:  16-bit
    uint32_t Value_u3;   // unsigned:  32-bit
    uint64_t Value_u4;   // unsigned:  64-bit
    
    int8_t   Value_s1;   // signed  :   8-bit
    int16_t  Value_s2;   // signed  :  16-bit
    int32_t  Value_s3;   // signed  :  32-bit
    int64_t  Value_s4;   // signed  :  64-bit
    //
    uint64_t NLength;
    uint64_t SLength;
    //
    char   * NName  ;
    char   * SName  ;
};

/**
 * \enum    symbolType
 * \brief   Aufzählungstyp für unterstützte Eigenschaften
 * \details In der Symbol-Tabelle wird der Name eines jeden Symbols abgelegt.
 *          Damit jedem Symbol eine Bedeutung zugeordnet werden kann, wird ein
 *          eindeutiger Typ bereit gestellt.
 */
enum symbolTypeEnum : uint32_t
{
    /// Symboltype ist nicht bekannt.
    stUnknown = 0,
    /// ist eine Funktion
    stFunction = 1,
    /// ist eine Prozedur
    stProcedure = 2,
    /// entspricht C/C++ void
    stVOID = 3,
    /// entspricht einen WideChar (2 Byte)
    stWideChar = 4,
    /// Qt Klasse - QChar
    stQChar = 100,
};

class QChar {
public:
    std::variant<char, uint8_t, uint16_t, uint32_t, wchar_t, short> qchar_types;
public:
    // constructor
    QChar(void);
    QChar(char      c);  // char
    QChar(uint8_t   c);  // byte/ansichar
    QChar(uint16_t  c);  // Word
    QChar(uint32_t  c);  // dword
    QChar(wchar_t   c);  // widechar
    QChar(short     c);  // short
    
    // destructor
    ~QChar(void);
    
    bool isAscii          () const;
    bool isDigit          () const;
    bool isLetter         () const;
    bool isLetterOrNumber () const;
    bool isLower          () const;
    bool isMark           () const;
    bool isNonCharacter   () const;
    bool isNull           () const;
    bool isNumber         () const;
    bool isPrint          () const;
    bool isPunct          () const;
    bool isSpace          () const;
    bool isSurrogate      () const;
    bool isSymbol         () const;
    bool isTitleCase      () const;
    bool isUpper          () const;
    
    char toAscii  () const;
    char toLatin1 () const;
       
    ::QChar toLower      () const;
    ::QChar toTitleCase  () const;
    ::QChar toUpper      () const;
    
    // setter
    void setType(char     t);
    void setType(uint8_t  t);
    void setType(uint16_t t);
    void setType(uint32_t t);
    void setType(wchar_t  t);
    void setType(short    t);
    
    // getter
    symbolTypeEnum getType  (void) const;
    ::QChar *      getOrigin(void) const;
    ::QChar *      origin;
    
    struct ClassVHelper * ptr_val = nullptr;
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
    uint32_t,
    float,
    std::wstring,
    qvc::QChar*> value;
    
    /// repräsentiert einen Integer Wert
    TypeTypes(uint32_t v) : value(v) {}
    /// repräsentiert eine Fließkomma Zahl
    TypeTypes(float v) : value(v) {}
    /// repräsentiert einen WideString
    TypeTypes(const std::wstring& v) : value(v) {}
    TypeTypes(qvc::QChar* v): value(v) { }
};

/**
 * \brief   Diese Symbol-Zuordnungs-Tabelle hält die Symbole mit entsprechenden Typ.
 * \details Der Typ den jedes Symbol annehmen kann, wird mit eines TypeTypes-Eintrag
 *          festgelegt. Für jedes Symbol kann immer nur ein Typ vergeben werden.
 */
extern std::map<std::wstring, std::unique_ptr<TypeTypes>> symbol_map;

extern uint64_t Iaddsymbol(const std::wstring&  p_sname, struct ClassVHelper *addr);
extern bool     Igetsymbol(      std::wstring&& p_sname);

extern uint64_t current_ptr;

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
#ifdef FPC
extern void ErrorMessage(const char    * text);
#else
extern void ErrorMessage(const wchar_t * text);
#endif

extern void    SetPascalCompiler(uint8_t pc);
extern uint8_t GetPascalCompiler(void);

/**
 * \defgroup qcharclass QChar
 * \ingroup  internFunctions
 * \brief    Interne Funktionen für die Klasse QChar
 */
/**
 * \ingroup  qcharclass CreateQChar
 * \brief    Erstellt eine Inztanz einer QChar Klasse
 * \param    p_name - wchar_t* der Name der Klasse
 * \return   uint64_t - ein 64-Bit Type der die Adresse der erstellten Klasse zurückgibt.
 */
uint64_t ctor_QChar(wchar_t* p_name, uint64_t addr);

/**
 * \defgroup qstringclass QString
 * \ingroup  internFunctions
 * \brief    Interne Funktionen für die Klasse QString
 */
/**
 * \ingroup  qstringclass CreateQString
 * \brief    Erstellt eine Inztanz einer QString Klasse
 */
bool ctor_QString(wchar_t* p_name);
/** @} */  // Ende: internFunctions

};      // ectern:    "C"
}       // namespace: qvc
#endif  // header:    __FPC_QT_H__
