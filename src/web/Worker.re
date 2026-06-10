/* Web worker thread */
/* The worker evaluates programs (and records probe samples) in its own
   JS context; WorkerServer.start installs its own clock for sample
   timestamps. */
WorkerServer.start();
