/* Web worker thread */
/* The worker evaluates programs (and records probe samples) in its own
   JS context, so it installs its own clock for sample timestamps. */
Util.TimeUtil.now_ms := Util.JsUtil.precise_timestamp;
WorkerServer.start();
