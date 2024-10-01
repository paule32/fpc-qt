// ---------------------------------------------------------------------------
// \file   fpc-qtchar.cc
// \author (c) 2024 Jens Kallup - paule32
// \copy   All rights reserved
// ---------------------------------------------------------------------------
# include "fpc-qt.h"
#define DEBUG
namespace qvc {

qvc::QChar::QChar(void      ) { origin = new ::QChar( ); }
qvc::QChar::QChar(char     t) { origin = new ::QChar(t); }
qvc::QChar::QChar(uint8_t  t) { origin = new ::QChar(((char)(t))); }
qvc::QChar::QChar(uint16_t t) { origin = new ::QChar(t); }
qvc::QChar::QChar(uint32_t t) { origin = new ::QChar(t); }
qvc::QChar::QChar(wchar_t  t) { origin = new ::QChar(t); }
qvc::QChar::QChar(short    t) { origin = new ::QChar(t); }

bool
qvc::QChar::isAscii() const {
    bool result = false;
    return result;
}

bool
qvc::QChar::isDigit() const {
    //if (nullptr != origin)
    //return origin->isDigit();
    
    return false;
}

bool
qvc::QChar::isLetter() const {
    if (nullptr != origin)
    return origin->isLetter();
    return false;
}

bool
qvc::QChar::isLetterOrNumber() const
{
    if (!strcmp(ptr_val->NName, "smallint")) {
        std::cout << ptr_val->Value_s2 << std::endl;
        if ((ptr_val->Value_s2 >= -32768) && (ptr_val->Value_s2 <= 32767)) {
            return true;
        }   else {
            return false;
        }
    }
    
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
    if (!strcmp(ptr_val->NName, "smallint")) {
        std::cout << ptr_val->Value_s2 << std::endl;
        if ((ptr_val->Value_s2 >= -32768) && (ptr_val->Value_s2 <= 32767)) {
            return true;
        }   else {
            return false;
        }
    }
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
    if ((addr == 0) || (ptr == 0)) {
        #ifdef FPC
            ErrorMessage("Error: QChar Objekt not handled by ctor.");
        #else
            ErrorMessage(L"Error: QChar Objekt not handled by ctor.");
        #endif
        exit(1);
        return false;
    }
    return true ;
}

bool
check_origin(::QChar * addr)
{
    if (nullptr != addr) {
        return true;
    }   else {
        #ifdef FPC
            ErrorMessage("Error: QChar origin not available.");
        #else
            ErrorMessage(L"Error: QChar origin not available.");
        #endif
    }   return false;
}

extern "C" {

DLL_EXPORT uint64_t
ctor_QChar(wchar_t* p_name, uint64_t addr)
{
    #ifdef DEBUG
    ClassVHelper * vhelper = new ClassVHelper;
    
    vhelper->VType = reinterpret_cast<ClassVHelper*>(addr)->VType;
    
    vhelper->Value_s1 = reinterpret_cast<ClassVHelper*>(addr)->Value_s1;
    vhelper->Value_s2 = reinterpret_cast<ClassVHelper*>(addr)->Value_s2;
    vhelper->Value_s3 = reinterpret_cast<ClassVHelper*>(addr)->Value_s3;
    vhelper->Value_s4 = reinterpret_cast<ClassVHelper*>(addr)->Value_s4;
    //
    vhelper->Value_u1 = reinterpret_cast<ClassVHelper*>(addr)->Value_u1;
    vhelper->Value_u2 = reinterpret_cast<ClassVHelper*>(addr)->Value_u2;
    vhelper->Value_u3 = reinterpret_cast<ClassVHelper*>(addr)->Value_u3;
    vhelper->Value_u4 = reinterpret_cast<ClassVHelper*>(addr)->Value_u4;
    
    vhelper->NLength = reinterpret_cast<ClassVHelper*>(addr)->NLength;
    vhelper->SLength = reinterpret_cast<ClassVHelper*>(addr)->SLength;
    
    vhelper->NName  = new char[vhelper->NLength+1];
    vhelper->SName  = new char[vhelper->SLength+1];
    
    strcpy(vhelper->NName, reinterpret_cast<ClassVHelper*>(addr)->NName);
    strcpy(vhelper->SName, reinterpret_cast<ClassVHelper*>(addr)->SName);
    
    std::wcout << L"CTOR mem:  0x"
               << std::hex << addr
               << std::dec << std::endl;
    std::wcout << L"CTOR sym:  " << vhelper->VType << std::endl;
    std::wcout << L"CTOR str:  " << vhelper->SName << std::endl;
//  std::wcout << L"CTOR val:  " << vhelper->Value << std::endl;

    #endif
    
    current_ptr = Iaddsymbol(p_name, vhelper);
    
    #ifdef DEBUG
    std::wcout << L"curptr: " << std::hex << current_ptr << std::endl;
    #endif
    
    return current_ptr;
}

DLL_EXPORT void
dtor_QChar(uint64_t addr)
{
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (nullptr != objptr->origin)
    delete objptr->origin;
    #ifdef DEBUG
    std::cout << "dtor QChar" << std::endl;
    #endif
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
    std::cout << "uint8_t value" << std::endl;
    bool result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    //objptr->qchar_types = static_cast<uint8_t>(value);
    
    /*char buffer[200];
    sprintf(buffer,"%d", value);
    
    int  i = 0;
    for (i = 0; i < 200; i++) {
        if (isdigit(buffer[i])) {
            result = true;
        }   else {
            result = false;
            break;
        }
    }*/
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
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    check_pointer(addr,  reinterpret_cast<uint64_t>(objptr));
    
    if (objptr->isLetterOrNumber())
    return true;

    return false;
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
    return true;
    return false;
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
    bool result = false;
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
