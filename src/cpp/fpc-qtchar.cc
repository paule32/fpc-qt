// ---------------------------------------------------------------------------
// File:   fpc-qtchar.cc
// Author: (c) 2024 Jens Kallup - paule32
// All rights reserved
// ---------------------------------------------------------------------------
# include "fpc-qt.h"

namespace qvc {

qvc::QChar::QChar(void      ) { origin = new ::QChar( ); }
qvc::QChar::QChar(char     t) { origin = new ::QChar(t); }
qvc::QChar::QChar(uint8_t  t) { origin = new ::QChar(t); }
qvc::QChar::QChar(uint16_t t) { origin = new ::QChar(t); }
qvc::QChar::QChar(uint32_t t) { origin = new ::QChar(t); }
qvc::QChar::QChar(wchar_t  t) { origin = new ::QChar(t); }
qvc::QChar::QChar(short    t) { origin = new ::QChar(t); }

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
        ErrorMessage("Error: type out of bounds.");
        exit(1);
    }
    return result;
}

bool
qvc::QChar::isAscii() const {
    bool result = false;
    symbolTypeEnum resultype = getType();
    if (resultype == stQChar_Char) {
        char c = std::holds_alternative<char>(qchar_types);
        if (std::isalpha(c)) {
            result = true;
        }   else {
            result = false;
        }
    }   else {
        ErrorMessage("QChar is not a CHAR");
    }   return result;
}

bool
qvc::QChar::isDigit() const {
    if (nullptr != origin)
    return origin->isDigit();
    return false;
}

bool
qvc::QChar::isLetter() const {
    if (nullptr != origin)
    return origin->isLetter();
    return false;
}

bool
qvc::QChar::isLetterOrNumber() const {
    if (nullptr != origin)
    return origin->isLetterOrNumber();
    return false;
}

bool
qvc::QChar::isLower() const {
    if (nullptr != origin)
    return origin->isLower();
    return false;
}

bool
qvc::QChar::isMark() const {
    if (nullptr != origin)
    return origin->isMark();
    return false;
}

bool
qvc::QChar::isNonCharacter() const {
    if (nullptr != origin)
    return origin->isNonCharacter();
    return false;
}

bool
qvc::QChar::isNull() const {
    if (nullptr != origin)
    return origin->isNull();
    return false;
}

bool
qvc::QChar::isNumber() const {
    if (nullptr != origin)
    return origin->isNumber();
    return false;
}

bool
qvc::QChar::isPrint() const {
    if (nullptr != origin)
    return origin->isPrint();
    return false;
}

bool
qvc::QChar::isPunct() const {
    if (nullptr != origin)
    return origin->isPunct();
    return false;
}

bool
qvc::QChar::isSpace() const {
    if (nullptr != origin)
    return origin->isSpace();
    return false;
}

bool
qvc::QChar::isSurrogate() const {
    if (nullptr != origin)
    return origin->isSurrogate();
    return false;
}

bool
qvc::QChar::isSymbol() const {
    if (nullptr != origin)
    return origin->isSymbol();
    return false;
}

bool
qvc::QChar::isTitleCase() const {
    if (nullptr != origin)
    return origin->isTitleCase();
    return false;
}

bool
qvc::QChar::isUpper() const {
    if (nullptr != origin)
    return origin->isUpper();
    return false;
}

   ::QChar *
qvc::QChar::getOrigin(void) const { return origin; }

char
qvc::QChar::toAscii() const {
    return origin->toLatin1();
}

char
qvc::QChar::toLatin1() const {
    return origin->toLatin1();
}

::QChar
qvc::QChar::toLower() const {
    return origin->toLower();
}

::QChar
qvc::QChar::toTitleCase() const {
    return origin->toTitleCase();
}

::QChar
qvc::QChar::toUpper() const {
    return origin->toUpper();
}

qvc::QChar::~QChar(void)
{
}

bool
check_pointer(uint64_t addr, uint64_t ptr)
{
    if ((addr == 0) || (ptr == 0) || (addr != ptr)) {
        ErrorMessage("Error: QChar Objekt not handled by ctor.");
        exit(1);
        return false;
    }
    return true ;
}

extern "C" {

DLL_EXPORT uint64_t
ctor_QChar(wchar_t* p_name, uint32_t sym_type, uint64_t addr)
{
    //std::wcout << L"CTOR mem: 0x" << std::hex <<  addr << std::dec << std::endl;
    //std::wcout << L"CTOR mem:   " << std::dec << reinterpret_cast<wchar_t*>(addr) << std::endl;
    
    Iaddsymbol(p_name, sym_type, addr);
    Igetsymbol(p_name);
    
    return current_ptr;
}

DLL_EXPORT void
dtor_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (nullptr != objptr->origin)
    delete objptr->origin;
}

DLL_EXPORT bool
isAscii_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isAscii())
    result = true;
    return result;
}

DLL_EXPORT bool
isDigit_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    return objptr->isDigit();
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
isMark_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isMark())
    result = true;
    return result;
}

DLL_EXPORT bool
isNonCharacter_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isNonCharacter())
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

DLL_EXPORT bool
isNumber_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isNumber())
    result = true;
    return result;
}

DLL_EXPORT bool
isPrint_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isPrint())
    result = true;
    return result;
}

DLL_EXPORT bool
isPunct_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isPunct())
    result = true;
    return result;
}

DLL_EXPORT bool
isSpace_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isSpace())
    result = true;
    return result;
}

DLL_EXPORT bool
isSurrogate_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isSurrogate())
    result = true;
    return result;
}

DLL_EXPORT bool
isSymbol_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isSymbol())
    result = true;
    return result;
}

DLL_EXPORT bool
isTitleCase_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isTitleCase())
    result = true;
    return result;
}

DLL_EXPORT bool
isUpper_QChar(uint64_t addr)
{
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isUpper())
    result = true;
    return result;
}

DLL_EXPORT uint8_t
toLatin1_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr,  reinterpret_cast<uint64_t>(objptr));
    objptr->toLatin1();
    
    return reinterpret_cast<uint64_t>(&objptr);
}

DLL_EXPORT uint16_t
toLower_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    objptr->toLower();
    
    return reinterpret_cast<uint64_t>(&objptr);
}

DLL_EXPORT uint16_t
toTitleCase_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    objptr->toTitleCase();
    
    return reinterpret_cast<uint64_t>(&objptr);
}

DLL_EXPORT uint16_t
toUpper_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(&objptr));
    objptr->toUpper();
    
    return reinterpret_cast<uint64_t>(&objptr);
}

};  // extern "C"
}   // namespace: qvc
