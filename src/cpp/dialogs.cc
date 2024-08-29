// ---------------------------------------------------------------------------
// \file       dialogs.cc
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
# include "fpc-qt.h"

extern "C" {

DLL_EXPORT void
#ifdef FPC
ErrorMessage(const char    * text)
#else
ErrorMessage(const wchar_t * text)
#endif
{
    char * argv[32] = {"executablename"};
    int    argc     = 1;
    QApplication app(argc, argv);
    
    #ifdef FPC
        std::string st(text);
        QString s = QString::fromStdString(st);
    #else
        QString s = QString::fromWCharArray(text);
    #endif
    
    QMessageBox msgBox;
    msgBox.setWindowTitle("Information");
    msgBox.setText(s);
    msgBox.setIcon(QMessageBox::Information);
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.setDefaultButton(QMessageBox::Ok);
    msgBox.exec();
}   // ErrorMessage
};  // extern "C"

namespace qvc {
}   // namespace qvc
