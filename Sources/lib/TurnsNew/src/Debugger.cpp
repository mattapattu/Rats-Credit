class Debugger
{
    private:
        bool m_debug;

    public:
        Debugger();
        void setDebug(bool debug);
        void debug(const char* message);
};

Debugger::Debugger()
{
    m_debug = false;
}

void Debugger::setDebug(bool debug)
{
    m_debug = debug;
}

void Debugger::printArmaVec(std::string message, arma::vec vector)
{
    if(m_debug)
    {
        cout << message << endl;
    }
}

void Debugger::printArmamat(const char* message)
{
    if(m_debug)
    {
        cout << message << endl;
    }
}
