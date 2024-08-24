// ---------------------------------------------------------------------------
// \file       dialogs.cc
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
# include "fpc-qt.h"

extern "C" {

DLL_EXPORT void
ErrorMessage(QString text)
{
    char * argv[32] = {"executablename"};
    int    argc     = 1;
    QApplication app(argc, argv);
    
    QMessageBox msgBox;
    msgBox.setWindowTitle("Information");
    msgBox.setText(text);
    msgBox.setIcon(QMessageBox::Information);
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.setDefaultButton(QMessageBox::Ok);
    msgBox.exec();
}   // ErrorMessage
};  // extern "C"

namespace qvc {
}   // namespace qvc
