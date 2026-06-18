// Instrumentation helper for VxWorks x86-64 RTPs.
// printf is statically internal+local in the RTP; this helper is compiled to
// assembly (-S) and concatenated into final.s so the `call printf` below binds
// to the RTP's own local printf label in the same translation unit.
extern int printf(const char *fmt, ...);

void rtp_main_log(void)
{
    printf("[instr] RTP entry\n");
}

