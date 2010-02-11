
static FILE* trace_file = NULL;

inline static void vmm_trace_xptr(const char cmd, const xptr& p)
{
    layer_t layer = p.layer;
    char istmp = 'D';

    if (layer >= TMP_LAYER_STARTS_WITH) {
        layer -= TMP_LAYER_STARTS_WITH;
        istmp = 'T';
    }

    fprintf(trace_file, "%c %c %x 0x%08x\n", cmd, istmp, layer, p.addr);
}

void vmm_trace_checkp(const xptr& p)
{
    vmm_trace_xptr('R', p);
}

void vmm_trace_signal_modification(const xptr& p)
{
    vmm_trace_xptr('W', p);
}

void vmm_trace_alloc_block(const xptr& p, const xptr& s)
{
    vmm_trace_xptr('A', p);
    if (s != XNULL) { vmm_trace_xptr('S', s); }
}

void vmm_trace_unswap(const xptr& p, const xptr& s)
{
    vmm_trace_xptr('U', p);
    if (s != XNULL) { vmm_trace_xptr('S', s); }
}

void vmm_trace_delete_block(const xptr& p)
{
    vmm_trace_xptr('E', p);
}
