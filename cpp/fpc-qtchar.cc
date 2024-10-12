// ---------------------------------------------------------------------------
// \file   fpc-qtchar.cc
// \author (c) 2024 Jens Kallup - paule32
// \copy   All rights reserved
// ---------------------------------------------------------------------------
# include "fpc-qt.h"
#define DEBUG
namespace qvc {

std::wstring TObject::ClassName() const {
    std::string   str = typeid(*this).name();
    std::wstring wstr(str.begin(), str.end());
    
    wchar_t* wch = new wchar_t[wstr.size() + 1];
    std::wcscpy(wch, wstr.c_str());
    
    return wch;
}

TObject::~TObject() {
}

/**
 * \brief  Klassen-Funktion zum prüfen, ob der im Konstrucktor gespeicherte
 *         Wert einen ASCII-Zeichen entspricht.
 *         Standard ASCII Zeichen wären: [a-zA-Z].
 * \params keine
 * \return False, wenn kein ASCII-Zeichen; ansonsten True.
 */
template <typename T>
bool
qvc::QChar<T>::isAscii() const {
    if (!wcscmp(ptr_val->VType2.Name, L"char")) {
        if (isascii(ptr_val->VType2.Value_u1)) return true;
        if (isascii(ptr_val->VType2.Value_u2)) return true;
    }
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isAlpha() const {
    if (!wcscmp(ptr_val->VType2.Name, L"char")) {
        if (isalpha(ptr_val->VType2.Value_u1)) return true;
        if (isalpha(ptr_val->VType2.Value_u2)) return true;
    }
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isAlphaNumber() const {
    if (!wcscmp(ptr_val->VType2.Name, L"char")) {
        if (isalnum(ptr_val->VType2.Value_u1)) return true;
        if (isalnum(ptr_val->VType2.Value_u2)) return true;
    }
    return false;
}


/**
 * \brief This member function checks
 */
template <typename T>
bool
qvc::QChar<T>::isBlank() const {
    if (!wcscmp(ptr_val->VType2.Name, L"char")) {
        std::wcout << L"c++ isblank" << std::endl;
        if ((static_cast<unsigned char>(ptr_val->VType2.Value_u1) == L' ')
        ||  (static_cast<unsigned char>(ptr_val->VType2.Value_u1) == L'\t')) {
            if ((std::isblank(static_cast<unsigned char>(ptr_val->VType2.Value_u1)))
            ||  (std::isblank(static_cast<unsigned char>(ptr_val->VType2.Value_u2)))) {
                return true;
            }   return false;
        }
    }   else
    if (!wcscmp(ptr_val->VType2.Name, L"arraychar")) {
        std::cout << "array c++ isblank" << std::endl;
        if ((static_cast<unsigned char>(ptr_val->VType2.Value_u1) ==  L' ')
        ||  (static_cast<unsigned char>(ptr_val->VType2.Value_u2) ==  L' ')
        ||  (static_cast<unsigned char>(ptr_val->VType2.Value_u2) == L'\t')
        ||  (static_cast<unsigned char>(ptr_val->VType2.Value_u1) == L'\t')) {
            if ((std::isblank(static_cast<unsigned char>(ptr_val->VType2.Value_u1)))
            ||  (std::isblank(static_cast<unsigned char>(ptr_val->VType2.Value_u2)))) {
                return true;
            }   return false;
        }
    }
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isDigit() const {
    if (!wcscmp(ptr_val->VType2.Name, L"char")) {
        std::wcout << L"c++ isdigi" << std::endl;
        if ((ptr_val->VType2.Value_u1 >= L'0') && (ptr_val->VType2.Value_u1 <= L'9')) {
            return true;
        }
    }
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isLetter() const {
    if (!wcscmp(ptr_val->VType2.Name, L"char")) {
        if (((ptr_val->VType2.Value_u1 >= L'a') && (ptr_val->VType2.Value_u1 <= L'z'))
        ||  ((ptr_val->VType2.Value_u1 >= L'A') && (ptr_val->VType2.Value_u1 <= L'Z'))) {
            return true;
        }
    }
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isLetterOrNumber() const {
    if (!wcscmp(ptr_val->VType2.Name, L"smallint")) {
        if (isascii(ptr_val->VType2.Value_u1))
        return true;

        if ((ptr_val->VType2.Value_s2 >= -32768) && (ptr_val->VType2.Value_s2 <= 32767))
        return true;
    }   else
    if (!wcscmp(ptr_val->VType2.Name, L"char")) {
        if (isascii(ptr_val->VType2.Value_u1))
        return true;
    }   else
    if (!wcscmp(ptr_val->VType2.Name, L"arraychar")) {
        if (isascii(ptr_val->VType2.Value_u1)) return true;
        if (isascii(ptr_val->VType2.Value_u2)) return true;
        
        if (isdigit(ptr_val->VType2.Value_u1)) return true;
        if (isdigit(ptr_val->VType2.Value_u2)) return true;
    }   else
    if (!wcscmp(ptr_val->VType2.Name, L"arraybyte")) {
        std::wcout << L"array c++ byte" << std::endl;
        return true;
    }
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isLower() const {
    if (!wcscmp(ptr_val->VType2.Name, L"arraychar")) {
        std::wcout << L"c++ loweerrr" << std::endl;
        if (islower(ptr_val->VType2.Value_u1))
        return true;
    }   return false;
}

template <typename T>
bool
qvc::QChar<T>::isMark() const {
    //if (nullptr != origin)
    //return origin->isMark();
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isNonCharacter() const {
    //if (nullptr != origin)
    //return origin->isNonCharacter();
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isNull() const {
    //if (nullptr != origin)
    //return origin->isNull();
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isNumber() const {
    if (!wcscmp(ptr_val->VType2.Name, L"smallint")) {
        std::cout << ptr_val->VType2.Value_s2 << std::endl;
        if ((ptr_val->VType2.Value_s2 >= -32768) && (ptr_val->VType2.Value_s2 <= 32767)) {
            return true;
        }   else {
            return false;
        }
    }
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isPrint() const {
    if (!wcscmp(ptr_val->VType2.Name, L"arraychar")) {
        std::wcout << L"c++ printerrr" << std::endl;
        if (isprint(ptr_val->VType2.Value_u1))
        return true;
    }   return false;
}

template <typename T>
bool
qvc::QChar<T>::isPunct() const {
    //if (nullptr != origin)
    //return origin->isPunct();
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isSpace() const {
    if (!wcscmp(ptr_val->VType2.Name, L"arraychar")) {
        if (isspace(ptr_val->VType2.Value_u1))
        return true;
    }   return false;
}

template <typename T>
bool
qvc::QChar<T>::isSurrogate() const {
    //if (nullptr != origin)
    //return origin->isSurrogate();
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isSymbol() const {
    //if (nullptr != origin)
    //return origin->isSymbol();
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isTitleCase() const {
    //if (nullptr != origin)
    //return origin->isTitleCase();
    return false;
}

template <typename T>
bool
qvc::QChar<T>::isUpper() const {
    if (!wcscmp(ptr_val->VType2.Name, L"arraychar")) {
        if (isupper(ptr_val->VType2.Value_u1))
        return true;
    }
    return false;
}

template <typename T>
   ::QChar *
qvc::QChar<T>::getOrigin(void) const { return /*origin*/ nullptr; }

template <typename T>
wchar_t
qvc::QChar<T>::toAscii() const {
    //return origin->toLatin1();
    return L'\0';
}

template <typename T>
wchar_t
qvc::QChar<T>::toLatin1() const {
    //return origin->toLatin1();
    return L'\0';
}

template <typename T>
::QChar
qvc::QChar<T>::toLower() const {
    //return origin->toLower();
    return '\0';
}

template <typename T>
::QChar
qvc::QChar<T>::toTitleCase() const {
    //return origin->toTitleCase();
    return '\0';
}

template <typename T>
::QChar
qvc::QChar<T>::toUpper() const {
    //return origin->toUpper();
    return '\0';
}

template <typename U>
std::ostream& operator << (std::ostream& os, const qvc::QChar<U>& c) {
    return os;
}

template <typename U>
std::istream& operator >> (std::istream& is, qvc::QChar<U>& c) {
    return is;
}
}   // namespace: qvc

bool
check_pointer(uint64_t addr, uint64_t ptr)
{
    if ((addr == 0) || (ptr == 0)) {
        #ifdef FPC
            ::ErrorMessage("Error: QChar Objekt not handled by ctor.");
        #else
            ::ErrorMessage(L"Error: QChar Objekt not handled by ctor.");
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
            ::ErrorMessage("Error: QChar origin not available.");
        #else
            ::ErrorMessage(L"Error: QChar origin not available.");
        #endif
    }   return false;
}

extern "C" {

DLL_EXPORT uint64_t
ctor_QChar(char* p_name, uint64_t addr)
{
    std::wcout << L"just fun" << std::endl;
    std::wcout << reinterpret_cast<wchar_t*>(p_name) << std::endl;

    auto * vhelper = new qvc::ResultVHelper;
    
    vhelper->VType1.VType = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.VType;
    vhelper->VType2.VType = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType2.VType;
    vhelper->VType3.VType = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType3.VType;
    
    vhelper->VType1.Value_s1 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.Value_s1;
    vhelper->VType1.Value_s2 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.Value_s2;
    vhelper->VType1.Value_s3 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.Value_s3;
    vhelper->VType1.Value_s4 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.Value_s4;
    //
    vhelper->VType2.Value_s1 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.Value_s1;
    vhelper->VType2.Value_s2 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType2.Value_s2;
    vhelper->VType2.Value_s3 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType2.Value_s3;
    vhelper->VType2.Value_s4 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType2.Value_s4;
    //
    vhelper->VType3.Value_s1 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType3.Value_s1;
    vhelper->VType3.Value_s2 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType3.Value_s2;
    vhelper->VType3.Value_s3 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType3.Value_s3;
    vhelper->VType3.Value_s4 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType3.Value_s4;
    
    vhelper->VType1.Value_u1 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.Value_u1;
    vhelper->VType1.Value_u2 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.Value_u2;
    vhelper->VType1.Value_u3 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.Value_u3;
    vhelper->VType1.Value_u4 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.Value_u4;
    //
    vhelper->VType2.Value_u1 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType2.Value_u1;
    vhelper->VType2.Value_u2 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType2.Value_u2;
    vhelper->VType2.Value_u3 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType2.Value_u3;
    vhelper->VType2.Value_u4 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType2.Value_u4;
    //
    vhelper->VType3.Value_u1 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType3.Value_u1;
    vhelper->VType3.Value_u2 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType3.Value_u2;
    vhelper->VType3.Value_u3 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType3.Value_u3;
    vhelper->VType3.Value_u4 = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType3.Value_u4;
    
    vhelper->VType1.Length  = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType1.Length;
    vhelper->VType2.Length  = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType2.Length;
    vhelper->VType3.Length  = reinterpret_cast<qvc::ResultVHelper*>(addr)->VType3.Length;
    
    
    vhelper->VType1.Name  = new wchar_t[64]; //vhelper->VType1.Length + 1];
    vhelper->VType2.Name  = new wchar_t[64]; //vhelper->VType2.Length + 1];
    vhelper->VType3.Name  = new wchar_t[64]; //vhelper->VType3.Length + 1];
    
    // still there no problem
    
    wcscpy(vhelper->VType1.Name, reinterpret_cast<qvc::ResultVHelper*>(addr)->VType2.Name);
    // error
    
    std::wcout << p_name << std::endl;
    //wcscpy(vhelper->VType2.Name, reinterpret_cast<ResultVHelper*>(addr)->VType2.Name);
    //wcscpy(vhelper->VType3.Name, reinterpret_cast<ResultVHelper*>(addr)->VType3.Name);
std::wcout << L"p_name" << std::endl;    
    #ifdef DEBUG
    std::wcout << L"CTOR mem:  0x"
               << std::hex << addr
               << std::dec << std::endl;
    std::wcout << L"CTOR sym:  " << vhelper->VType2.VType    << std::endl;
    std::wcout << L"CTOR str:  " << vhelper->VType2.Name     << std::endl;
    std::wcout << L"CTOR val:  " << vhelper->VType2.Value_u1 << std::endl;
    #endif
    
    qvc::current_ptr = Iaddsymbol(p_name, vhelper);
    
    #ifdef DEBUG
    std::wcout << L"curptr: " << std::hex << qvc::current_ptr << std::endl;
    #endif
    
    return qvc::current_ptr;
}

DLL_EXPORT void
dtor_QChar(uint64_t addr)
{
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    
    //if (nullptr != objptr->origin)
    //delete objptr->origin;
    #ifdef DEBUG
    std::cout << "dtor QChar" << std::endl;
    #endif
}

// ok
extern "C" {
DLL_EXPORT bool
isAscii_QChar(uint64_t addr) {
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);
    
    std::wcout << L"objptr = " << objptr->ClassName() << std::endl;

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isAscii())
    result = true;
    return result;
}
};

DLL_EXPORT bool
isAlpha_QChar(uint64_t addr) {
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isAlpha())
    result = true;
    return result;
}

DLL_EXPORT bool
isAlphaNumber_QChar(uint64_t addr) {
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isAlphaNumber())
    result = true;
    return result;
}

DLL_EXPORT bool
isBlank_QChar(uint64_t addr) {
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    
    std::wcout << L"objptr = " << objptr->ClassName() << std::endl;
    
    if (objptr->isBlank())
    return true;
    return false;
}

DLL_EXPORT bool
isDigit_QChar(uint64_t addr)
{
    std::wcout << L"uint8_t value" << std::endl;
    bool result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isDigit())
    result = true;
    return result;
}

DLL_EXPORT bool
isLetter_QChar(uint64_t addr)
{
    std::wcout << L"letter or not" << std::endl;
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isLetter())
    result = true;
    return result;
}

DLL_EXPORT bool
isLetterOrNumber_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);
    
    check_pointer(addr,  reinterpret_cast<uint64_t>(objptr));
    
    if (objptr->isLetterOrNumber())
    return true;
    return false;
}

DLL_EXPORT bool
isLower_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isLower())
    result = true;
    return result;
}

DLL_EXPORT bool
isMark_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isMark())
    result = true;
    return result;
}

DLL_EXPORT bool
isNonCharacter_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isNonCharacter())
    return true;
    return false;
}

DLL_EXPORT bool
isNull_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isNull())
    result = true;
    return result;
}

DLL_EXPORT bool
isNumber_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isNumber())
    result = true;
    return result;
}

DLL_EXPORT bool
isPrint_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isPrint())
    result = true;
    return result;
}

DLL_EXPORT bool
isPunct_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isPunct())
    result = true;
    return result;
}

DLL_EXPORT bool
isSpace_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isSpace())
    result = true;
    return result;
}

DLL_EXPORT bool
isSurrogate_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isSurrogate())
    result = true;
    return result;
}

DLL_EXPORT bool
isSymbol_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isSymbol())
    result = true;
    return result;
}

DLL_EXPORT bool
isTitleCase_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isTitleCase())
    result = true;
    return result;
}

DLL_EXPORT bool
isUpper_QChar(uint64_t addr)
{
    bool   result = false;
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);

    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    if (objptr->isUpper())
    result = true;
    return result;
}

DLL_EXPORT uint8_t
toLatin1_QChar(uint64_t addr)
{
    auto * objptr = reinterpret_cast<qvc::QChar<char>*>(addr);
    
    check_pointer(addr,  reinterpret_cast<uint64_t>(objptr));
    objptr->toLatin1();
    
    return reinterpret_cast<uint64_t>(&objptr);
}

DLL_EXPORT uint16_t
toLower_QChar(uint64_t addr)
{
    #if 0
    auto * objptr = reinterpret_cast<qvc::QChar<wchar_t>>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    objptr->toLower();
    
    return reinterpret_cast<uint64_t>(&objptr);
    #endif
    return 0;
}

DLL_EXPORT uint16_t
toTitleCase_QChar(uint64_t addr)
{
    #if 0
    auto * objptr = reinterpret_cast<qvc::QChar<wchar_t>>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(objptr));
    objptr->toTitleCase();
    
    return reinterpret_cast<uint64_t>(&objptr);
    #endif
    return 0;
}

DLL_EXPORT uint16_t
toUpper_QChar(uint64_t addr)
{
    #if 0
    auto * objptr = reinterpret_cast<qvc::QChar<wchar_t>>(addr);
    
    check_pointer(addr, reinterpret_cast<uint64_t>(&objptr));
    objptr->toUpper();
    
    return reinterpret_cast<uint64_t>(&objptr);
    #endif
    return 0;
}

};  // extern "C"

