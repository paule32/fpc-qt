/**
 * \file      string.hpp
 * \namespace std
 * \cond      german
 * \brief     Der STL (Standard Template Library) std Namensbereich.
 *
 * Die \colorText{yellow,STL} ist eine hochflexible Sammlung von Template-Klassen.
 * Leider bringt diese Flexibilität eine Komplexität mit sich,
 * die es dem Anfänger schwer macht, die STL zu verstehen.
 * Das wird vermutlich der Grund sein, warum sie von manchen C++-Programmierern
 * nicht benutzt wird und von einigen C++-Büchern vollständig ignoriert wird.
 * \endcond
 *
 * \cond english
 * \brief The STL (Standard Template Library) std namespace
 *
 * The STL is a highly flexible Collection of Template Classes.
 * But they bring a very complexity, that make it hard to understand for youngsters.
 * That maybe the reason that many C++ Programmers don't use it, or they would be
 * ignore completly by modern C++ books.
 * \endcond
 */
namespace std
{
    /**
     * \brief   Eine Beispiel-Spezialisierung der std::char_traits Struktur.
     * \details Die Klasse char_traits ist eine Traits-Klassenvorlage, die grundlegende
     *          Zeichen- und String-Operationen für einen bestimmten Zeichentyp vereinheitlicht.
     *          Die definierte Operationsmenge ist so beschaffen, dass generische Algorithmen
     *          fast immer mit ihr implementiert werden können.
     *          Es ist daher möglich, solche Algorithmen mit fast jedem möglichen Zeichen- oder
     *          Stringtyp zu verwenden, indem man einfach eine angepasste char_traits-Klasse liefert.
     *
     *          Die Klassenvorlage char_traits dient als Grundlage für explizite Instanziierungen.
     *          Der Benutzer kann eine Spezialisierung für beliebige benutzerdefinierte Zeichentypen
     *          bereitstellen.
     *          Für die Standard-Zeichentypen sind mehrere explizite Spezialisierungen vorgesehen
     *          (siehe unten), andere Spezialisierungen sind nicht erforderlich, um die Anforderungen
     *          von CharTraits zu erfüllen.
     *
     * | Trait                      | Charakter-Eigenschaften von:   |
     * | :------------------------- | :----------------------------- |
     * | std::char_traits<char>     | char                 |
     * | std::char_traits<wchar_t>  | wchar_t              |
     * | std::char_traits<char8_t>  | char8_t  ( C++ 20 )  |
     * | std::char_traits<char16_t> | char16_t ( C++ 11 )  |
     * | std::char_traits<char32_t> | char32_t ( C++ 11 )  |
     *
     * \tparam  CharT Der Zeichentyp, für den die Traits definiert sind.
     */     
    template<typename CharT>
    struct char_traits {
        /**
         * \brief Gibt die Länge des nullterminierten Zeichenarrays zurück.
         * 
         * \param s Ein Pointer auf ein nullterminiertes Zeichenarray.
         * \return Die Länge des Arrays ohne den nullterminierenden Charakter.
         */
        static size_t length(const CharT* s);
    };
	
    template<typename CharT, typename Traits = char_traits<CharT>, typename Allocator = allocator<CharT>>
    class basic_string {
        // ...
    };

    /**
     * \defgroup  stl_string string
     * @{
     * \ingroup   template_stl
     * \cond    german
     * \class   string
     * \brief   Dies ist die Hauptklasse für die String Behandlung und Manipulation von Zeichenketten.
     * \details Zeichenketten (im englischen: String's) in der OOP sind Objekte, die Sequenzen von
     *          einen Zeichen, die zusammen gesetz eine Zeichenkette bilden.
     *
     *          Die Standard string-Klasse bietet Unterstützung für solche Objekte mit einer Schnittstelle an,
     *          die der einer Standard-Containers für Bytes ähnelt, jedoch mit zusätzlichen Funktionen, die
     *          speziell für das Bearbeiten von Zeichenketten entwickelt wurde.
     *
     *          Die Klasse string ist eine Instanziierung der Klassenvorlage basic_string, die als Konstruktor
     *          mit seinen Standard char_traits und allocator-Typen als Zeichentyp verwendet wird.
     *
     * \note    Beachten Sie, dass diese Klasse Bytes unabhängig von der verwendeten Kodierung verarbeitet:
     *          Werden Sequenzen von Multi-Byte-Zeichen oder Zeichen mit variabler Länge (z. B. UTF-8) verarbeitet,
     *          arbeiten alle Mitglieder dieser Klasse (zum Beispiel: length oder size) sowie ihre Iteratoren
     *          weiterhin in Form von Bytes (und nicht in Form von tatsächlich kodierten Zeichen).
     *
     * \endcond
     *
     * \cond english
     * \class string
     * \brief This is the main class for String handling, and manipulation of string's.
     * \endcond
     */
    class string {
    };
    /** @} */    // Ende der Gruppe: std_string
    
    /**
     * \cond english
     * \brief This is a UTF-8 / Ansi String ( 1 Byte per Char ).
     * \endcond
     *
     * \cond german
     * \brief Dies ist ein UTF-8 / Ansi String ( 1 Byte pro Zeichen ).
     * \endcond
     */
    typedef basic_string<char> string;

    /**
     * \cond english
     * This is a Unicode String ( 2 Byte per Character ).
     * \endcond
     *
     * \cond german
     * \brief Dies ist ein Unicode String ( 2 Byte pro Zeichen ).
     * \endcond
     */
    typedef basic_string<wchar_t> wstring;
}

