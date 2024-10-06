// ---------------------------------------------------------------------------
// \file   fpc-qtchar.cc
// \author (c) 2024 Jens Kallup - paule32
// \copy   All rights reserved
// ---------------------------------------------------------------------------
# include "fpc-qt.h"
#define DEBUG
namespace qvc {

/**
 * \brief Standard Konstruktor (ctor) für die Klasse QChar.
 */
qvc::QChar::QChar(void      ) { origin = new ::QChar( ); }
/**
 * \brief QChar Klassen-Konstruktor (ctor) für das halten eines Zeichens
 * (Letter).
 */
qvc::QChar::QChar(char     t) { origin = new ::QChar(t); }
qvc::QChar::QChar(uint8_t  t) { origin = new ::QChar(((char)(t))); }
qvc::QChar::QChar(uint16_t t) { origin = new ::QChar(t); }
qvc::QChar::QChar(uint32_t t) { origin = new ::QChar(t); }
qvc::QChar::QChar(wchar_t  t) { origin = new ::QChar(t); }
qvc::QChar::QChar(short    t) { origin = new ::QChar(t); }

/**
 * \brief  Klassen-Funktion zum prüfen, ob der im Konstrucktor gespeicherte
 *         Wert einen ASCII-Zeichen entspricht.
 *         Standard ASCII Zeichen wären: [a-zA-Z].
 * \params keine
 * \return False, wenn kein ASCII-Zeichen; ansonsten True.
 */
bool
qvc::QChar::isAscii() const {
    if (!strcmp(ptr_val->VType2.Name, "char")) {
        if (isascii(ptr_val->VType2.Value_u1)) return true;
        if (isascii(ptr_val->VType2.Value_u2)) return true;
    }
    return false;
}

/**
 * \brief This member function checks
 */
bool
qvc::QChar::isBlank() const {
    if (!strcmp(ptr_val->VType2.Name, "char")) {
        std::cout << "c++ isblank" << std::endl;
        if ((std::isblank(static_cast<unsigned char>(ptr_val->VType2.Value_u1)))
        ||  (std::isblank(static_cast<unsigned char>(ptr_val->VType2.Value_u2)))) {
            return true;
        }
    }
    return false;
}

bool
qvc::QChar::isDigit() const {
    if (!strcmp(ptr_val->VType2.Name, "char")) {
        std::cout << "c++ isdigi" << std::endl;
        if ((ptr_val->VType2.Value_u1 >= '0') && (ptr_val->VType2.Value_u1 <= '9')) {
            return true;
        }
    }
    return false;
}

bool
qvc::QChar::isLetter() const {
    if (!strcmp(ptr_val->VType2.Name, "char")) {
        if (((ptr_val->VType2.Value_u1 >= 'a') && (ptr_val->VType2.Value_u1 <= 'z'))
        ||  ((ptr_val->VType2.Value_u1 >= 'A') && (ptr_val->VType2.Value_u1 <= 'Z'))) {
            return true;
        }
    }
    return false;
}

bool
qvc::QChar::isLetterOrNumber() const {
    if (!strcmp(ptr_val->VType2.Name, "smallint")) {
        if (isascii(ptr_val->VType2.Value_u1))
        return true;

        if ((ptr_val->VType2.Value_s2 >= -32768) && (ptr_val->VType2.Value_s2 <= 32767))
        return true;
    }   else
    if (!strcmp(ptr_val->VType2.Name, "char")) {
        if (isascii(ptr_val->VType2.Value_u1))
        return true;
    }   else
    if (!strcmp(ptr_val->VType2.Name, "arraychar")) {
        if (isascii(ptr_val->VType2.Value_u1)) return true;
        if (isascii(ptr_val->VType2.Value_u2)) return true;
        
        if (isdigit(ptr_val->VType2.Value_u1)) return true;
        if (isdigit(ptr_val->VType2.Value_u2)) return true;
    }   else
    if (!strcmp(ptr_val->VType2.Name, "arraybyte")) {
        std::cout << "array c++ byte" << std::endl;
        return true;
    }
    return false;
}

bool
qvc::QChar::isLower() const {
    if (!strcmp(ptr_val->VType2.Name, "arraychar")) {
        std::cout << "c++ loweerrr" << std::endl;
        if (islower(ptr_val->VType2.Value_u1))
        return true;
    }   return false;
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
    if (!strcmp(ptr_val->VType2.Name, "smallint")) {
        std::cout << ptr_val->VType2.Value_s2 << std::endl;
        if ((ptr_val->VType2.Value_s2 >= -32768) && (ptr_val->VType2.Value_s2 <= 32767)) {
            return true;
        }   else {
            return false;
        }
    }
    return false;
}

bool
qvc::QChar::isPrint() const {
    if (!strcmp(ptr_val->VType2.Name, "arraychar")) {
        std::cout << "c++ printerrr" << std::endl;
        if (isprint(ptr_val->VType2.Value_u1))
        return true;
    }   return false;
}

bool
qvc::QChar::isPunct() const {
    if (nullptr != origin)
    return origin->isPunct();
    return false;
}

bool
qvc::QChar::isSpace() const {
    if (!strcmp(ptr_val->VType2.Name, "arraychar")) {
        if (isspace(ptr_val->VType2.Value_u1))
        return true;
    }   return false;
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
    if (!strcmp(ptr_val->VType2.Name, "arraychar")) {
        if (isupper(ptr_val->VType2.Value_u1))
        return true;
    }
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

std::ostream& operator << (std::ostream& os, const QChar& c) {
    return os;
}

std::istream& operator >> (std::istream& is, QChar& c) {
    return is;
}

extern "C" {

DLL_EXPORT uint64_t
ctor_QChar(wchar_t* p_name, uint64_t addr)
{
    ResultVHelper * vhelper = new ResultVHelper;
    
    vhelper->VType1.VType = reinterpret_cast<ResultVHelper*>(addr)->VType1.VType;
    vhelper->VType2.VType = reinterpret_cast<ResultVHelper*>(addr)->VType2.VType;
    vhelper->VType3.VType = reinterpret_cast<ResultVHelper*>(addr)->VType3.VType;
    
    vhelper->VType1.Value_s1 = reinterpret_cast<ResultVHelper*>(addr)->VType1.Value_s1;
    vhelper->VType1.Value_s2 = reinterpret_cast<ResultVHelper*>(addr)->VType1.Value_s2;
    vhelper->VType1.Value_s3 = reinterpret_cast<ResultVHelper*>(addr)->VType1.Value_s3;
    vhelper->VType1.Value_s4 = reinterpret_cast<ResultVHelper*>(addr)->VType1.Value_s4;
    //
    vhelper->VType2.Value_s1 = reinterpret_cast<ResultVHelper*>(addr)->VType1.Value_s1;
    vhelper->VType2.Value_s2 = reinterpret_cast<ResultVHelper*>(addr)->VType2.Value_s2;
    vhelper->VType2.Value_s3 = reinterpret_cast<ResultVHelper*>(addr)->VType2.Value_s3;
    vhelper->VType2.Value_s4 = reinterpret_cast<ResultVHelper*>(addr)->VType2.Value_s4;
    //
    vhelper->VType3.Value_s1 = reinterpret_cast<ResultVHelper*>(addr)->VType3.Value_s1;
    vhelper->VType3.Value_s2 = reinterpret_cast<ResultVHelper*>(addr)->VType3.Value_s2;
    vhelper->VType3.Value_s3 = reinterpret_cast<ResultVHelper*>(addr)->VType3.Value_s3;
    vhelper->VType3.Value_s4 = reinterpret_cast<ResultVHelper*>(addr)->VType3.Value_s4;
    
    vhelper->VType1.Value_u1 = reinterpret_cast<ResultVHelper*>(addr)->VType1.Value_u1;
    vhelper->VType1.Value_u2 = reinterpret_cast<ResultVHelper*>(addr)->VType1.Value_u2;
    vhelper->VType1.Value_u3 = reinterpret_cast<ResultVHelper*>(addr)->VType1.Value_u3;
    vhelper->VType1.Value_u4 = reinterpret_cast<ResultVHelper*>(addr)->VType1.Value_u4;
    //
    vhelper->VType2.Value_u1 = reinterpret_cast<ResultVHelper*>(addr)->VType2.Value_u1;
    vhelper->VType2.Value_u2 = reinterpret_cast<ResultVHelper*>(addr)->VType2.Value_u2;
    vhelper->VType2.Value_u3 = reinterpret_cast<ResultVHelper*>(addr)->VType2.Value_u3;
    vhelper->VType2.Value_u4 = reinterpret_cast<ResultVHelper*>(addr)->VType2.Value_u4;
    //
    vhelper->VType3.Value_u1 = reinterpret_cast<ResultVHelper*>(addr)->VType3.Value_u1;
    vhelper->VType3.Value_u2 = reinterpret_cast<ResultVHelper*>(addr)->VType3.Value_u2;
    vhelper->VType3.Value_u3 = reinterpret_cast<ResultVHelper*>(addr)->VType3.Value_u3;
    vhelper->VType3.Value_u4 = reinterpret_cast<ResultVHelper*>(addr)->VType3.Value_u4;
    
    vhelper->VType1.Length  = reinterpret_cast<ResultVHelper*>(addr)->VType1.Length;
    vhelper->VType2.Length  = reinterpret_cast<ResultVHelper*>(addr)->VType2.Length;
    vhelper->VType3.Length  = reinterpret_cast<ResultVHelper*>(addr)->VType3.Length;
    
    vhelper->VType1.Name  = new char[vhelper->VType1.Length + 1];
    vhelper->VType2.Name  = new char[vhelper->VType2.Length + 1];
    vhelper->VType3.Name  = new char[vhelper->VType3.Length + 1];

    strcpy(vhelper->VType2.Name, reinterpret_cast<ResultVHelper*>(addr)->VType2.Name);
    //strcpy(vhelper->VType2.Name, reinterpret_cast<ResultVHelper*>(addr)->VType2.Name);
    //strcpy(vhelper->VType3.Name, reinterpret_cast<ResultVHelper*>(addr)->VType3.Name);
    
    #ifdef DEBUG
    std::wcout << L"CTOR mem:  0x"
               << std::hex << addr
               << std::dec << std::endl;
    std::wcout << L"CTOR sym:  " << vhelper->VType2.VType    << std::endl;
    std::wcout << L"CTOR str:  " << vhelper->VType2.Name     << std::endl;
    std::wcout << L"CTOR val:  " << vhelper->VType2.Value_u1 << std::endl;
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

// ok
DLL_EXPORT bool
isAscii_QChar(uint64_t addr) {
    bool   result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isAscii())
    result = true;
    return result;
}

DLL_EXPORT bool
isBlank_QChar(uint64_t addr) {
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isBlank())
    return true;
    return false;
}

DLL_EXPORT bool
isDigit_QChar(uint64_t addr)
{
    std::cout << "uint8_t value" << std::endl;
    bool result = false;
    qvc::QChar* objptr = reinterpret_cast<qvc::QChar*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isDigit())
    result = true;
    return result;
}

DLL_EXPORT bool
isLetter_QChar(uint64_t addr)
{
    std::cout << "letter or not" << std::endl;
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
