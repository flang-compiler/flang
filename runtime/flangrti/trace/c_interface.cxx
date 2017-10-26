#include <stdlib.h>
#include <signal.h>
#include <tchar.h>

extern "C" { #include "trace/c_interface.h" }
#include "trace/StackWalker.h"

void SignalHandler(int signal)
{
    StackWalker sw; sw.ShowCallstack();
    exit(1);
}

extern "C" void _install_win32_handlers()
{
    typedef void (*SignalHandlerPointer)(int);

    SignalHandlerPointer previousHandler;
    previousHandler = signal(SIGSEGV , SignalHandler);
}
