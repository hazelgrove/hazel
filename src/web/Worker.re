/* Web worker thread */
StdlibBaked.install();
WorkerServer.install_message_handler();
/* before the first request arrives: the worker starts with the app */
StdlibBaked.evaluate();
