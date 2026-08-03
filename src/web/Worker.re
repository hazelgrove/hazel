/* Web worker thread */
/* The worker evaluates programs (and records probe samples) in its own
   JS context; WorkerServer.install_message_handler installs its own clock
   for sample timestamps. */
WorkerServer.install_message_handler();
