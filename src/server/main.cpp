#include <QCoreApplication>
#include <QTcpServer>
#include <QTcpSocket>
#include <QDebug>

class MyServer : public QTcpServer {
    Q_OBJECT

public:
    MyServer(QObject* parent = nullptr) : QTcpServer(parent) {
        // Starte den Server auf einem bestimmten Port
        if (!this->listen(QHostAddress::Any, 12345)) {
            qCritical() << "Server konnte nicht gestartet werden!";
        } else {
            qDebug() << "Server läuft auf Port" << this->serverPort();
        }
    }

protected:
    void incomingConnection(qintptr socketDescriptor) override {
        // Wird aufgerufen, wenn eine neue Verbindung eingeht
        QTcpSocket* socket = new QTcpSocket(this);
        socket->setSocketDescriptor(socketDescriptor);

        qDebug() << "Neue Verbindung von" << socket->peerAddress().toString();

        // Verbinde das ReadyRead-Signal, um Daten zu empfangen
        connect(socket, &QTcpSocket::readyRead, this, [this, socket]() {
            QByteArray data = socket->readAll();
            qDebug() << "Empfangene Daten:" << data;
            
            // Sende eine Antwort zurück
            socket->write("Daten empfangen\n");
        });

        // Verbinde das Disconnected-Signal, um die Verbindung zu schließen
        connect(socket, &QTcpSocket::disconnected, socket, &QTcpSocket::deleteLater);
    }
};

int main(int argc, char *argv[]) {
    QCoreApplication a(argc, argv);

    MyServer server;

    return a.exec();
}

#include "main.moc"
