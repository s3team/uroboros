// Kernel-safe instrumentation helper for VxWorks x86-64 DKMs.
extern int logMsg(const char *fmt, ...);

void dkm_entry_log(void)
{
    logMsg("[instr] DKM entry\n");
}

