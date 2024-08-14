// ---------------------------------------------------------------------------
// File:   fpc-qtchar.cc
// Author: (c) 2024 Jens Kallup - paule32
// All rights reserved
// ---------------------------------------------------------------------------
# include "fpc-qt.h"

namespace qvc {

qvc::QChar::QChar(void      ) { origin_obj = new ::QChar( ); }
qvc::QChar::QChar(char     t) { origin_obj = new ::QChar(t); }
qvc::QChar::QChar(uint8_t  t) { origin_obj = new ::QChar(t); }
qvc::QChar::QChar(uint16_t t) { origin_obj = new ::QChar(t); }
qvc::QChar::QChar(uint32_t t) { origin_obj = new ::QChar(t); }
qvc::QChar::QChar(wchar_t  t) { origin_obj = new ::QChar(t); }
qvc::QChar::QChar(short    t) { origin_obj = new ::QChar(t); }

void qvc::QChar::setType(char     t) { qchar_types = static_cast<char    >( t ); }
void qvc::QChar::setType(uint8_t  t) { qchar_types = static_cast<uint8_t >( t ); }
void qvc::QChar::setType(uint16_t t) { qchar_types = static_cast<uint16_t>( t ); }
void qvc::QChar::setType(uint32_t t) { qchar_types = static_cast<uint32_t>( t ); }
void qvc::QChar::setType(wchar_t  t) { qchar_types = static_cast<wchar_t >( t ); }
void qvc::QChar::setType(short    t) { qchar_types = static_cast<short   >( t ); }

symbolTypeEnum
qvc::QChar::getType(void) const
{
    symbolTypeEnum result = stUnknown;
    
    if (std::holds_alternative<char    >(qchar_types)) result = stQChar_Char;     else
    if (std::holds_alternative<uint8_t >(qchar_types)) result = stQChar_AnsiChar; else
    if (std::holds_alternative<uint16_t>(qchar_types)) result = stQChar_WideChar; else
    if (std::holds_alternative<uint32_t>(qchar_types)) result = stQChar_DWord;    else
    if (std::holds_alternative<wchar_t >(qchar_types)) result = stQChar_Word;     else
    if (std::holds_alternative<short   >(qchar_types)) result = stQChar_ShortInt; else {
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

bool qvc::QChar::isDigit () const         { return getOrigin()->isDigit ();         }
bool qvc::QChar::isLetter() const         { return getOrigin()->isLetter();         }
bool qvc::QChar::isLetterOrNumber() const { return getOrigin()->isLetterOrNumber(); }
bool qvc::QChar::isLower () const         { return getOrigin()->isLower ();         }
bool qvc::QChar::isNull  () const         { return getOrigin()->isNull  ();         }

   ::QChar *
qvc::QChar::getOrigin(void) const { return origin_obj; }

   char qvc::QChar::toLatin1   () const { return getOrigin()->toLatin1   (); }
::QChar qvc::QChar::toLower    () const { return getOrigin()->toLower    (); }
::QChar qvc::QChar::toUpper    () const { return getOrigin()->toUpper    (); }
::QChar qvc::QChar::toTitleCase() const { return getOrigin()->toTitleCase(); }

qvc::QChar::~QChar(void)
{
    #ifdef DEBUG
    std::wcout << L"cpp: QChar: dtor..." << std::endl;
    #endif
    try {
        if (nullptr != origin_obj) {
            std::wcout << L"not null" << std::endl;
            delete origin_obj;
        }
    }
    catch (std::exception &e) {
        std::wcout << L"Exception: ";
        std::wcout << e.what() << std::endl;
    }
}

bool
check_pointer(uint64_t addr, uint64_t ptr) {
    if ((addr == 0) || (ptr == 0) || (addr != ptr)) {
        MessageBoxW(
            NULL,
            L"Error: QChar Objekt not handled by ctor.",
            L"Error",
            MB_ICONINFORMATION | MB_OK
        );
        ExitProcess(1);
        return false;
    }   return true ;
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
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    delete objptr;
}

DLL_EXPORT bool
isDigit_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isDigit())
    result = true;
    return result;
}

DLL_EXPORT bool
isLetter_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isLetter())
    result = true;
    return result;
}

DLL_EXPORT bool
isLetterOrNumber_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isLetterOrNumber())
    result = true;
    return result;
}

DLL_EXPORT bool
isLower_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isLower())
    result = true;
    return result;
}

DLL_EXPORT bool
isNull_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isNull())
    result = true;
    return result;
}

DLL_EXPORT uint64_t
toLatin1_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    objptr->toLatin1();
    
    return addr;
}

DLL_EXPORT uint64_t
toLower_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    objptr->toLower();
    
    return addr;
}

DLL_EXPORT uint64_t
toUpper_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(&objptr));
    objptr->toUpper();
    
    return addr;
}

DLL_EXPORT uint64_t
toTitleCase_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    objptr->toTitleCase();
    
    return addr;
}


};  // extern "C"
}   // namespace: qvc
