ADD_LIBRARY ( common 

	# Header Files
	IntHash.h IntList.h SSMMsg.h XptrHash.h argtable.h base.h
	config.h counted_ptr.h gmm.h ipc_ops.h lm_base.h
	persistent_db_data.h pping.h sedna.h sedna_ef.h
	sm_vmm_data.h sp.h tr_debug.h ugc.h utils.h version.h
	xptr.h

	# Source Files
	SSMMsg.cpp argtable.c base.cpp gmm.cpp ipc_ops.cpp
	pping.cpp sedna.c sp.c tr_debug.cpp ugc.cpp utils.cpp
	version.c xptr.cpp

	# Header Files, from errdbg
	errdbg/d_printf.h errdbg/error_codes.h errdbg/event_log.h
	errdbg/exceptions.h

	# Source Files, from errdbg
	errdbg/assert.c errdbg/d_printf.c errdbg/error_codes.c
	errdbg/event_log.c errdbg/exceptions.cpp

	# Header Files, from mmgr
	mmgr/memnodes.h mmgr/memutils.h mmgr/se_alloc.h

	# Source Files, from mmgr
	mmgr/aset.c mmgr/mcxt.c mmgr/track.c

	# Header Files, from ph
	ph/_pers_malloc.h ph/pers_heap.h

	# Source Files, from ph
	ph/pers_free.cpp ph/pers_heap.cpp ph/pers_malloc.cpp
	ph/pers_realloc.cpp

	# Header Files, from u
	u/u.h u/udl.h u/uhdd.h u/ummap.h u/umutex.h u/upipe.h
	u/uprocess.h u/usecurity.h u/usem.h u/ushm.h u/usocket.h
	u/uthread.h u/utime.h u/uutils.h

	# Source Files, from u
	u/u.c u/uhdd.c u/ummap.cpp u/umutex.c u/upipe.cpp
	u/uprocess.c u/usecurity.c u/usem.c u/ushm.c u/usocket.c
	u/uthread.c u/utime.c u/uutils.c
)

SOURCE_GROUP ( "Header Files 1" FILES
	IntHash.h IntList.h SSMMsg.h XptrHash.h argtable.h base.h
	config.h counted_ptr.h gmm.h ipc_ops.h lm_base.h
	persistent_db_data.h pping.h sedna.h sedna_ef.h
	sm_vmm_data.h sp.h tr_debug.h ugc.h utils.h version.h
	xptr.h
)

SOURCE_GROUP ( "Source Files 1" FILES
	SSMMsg.cpp argtable.c base.cpp gmm.cpp ipc_ops.cpp
	pping.cpp sedna.c sp.c tr_debug.cpp ugc.cpp utils.cpp
	version.c xptr.cpp
)

SOURCE_GROUP ( "ERRDBG" )

SOURCE_GROUP ( "ERRDBG\\Header Files 2" FILES
	errdbg/d_printf.h errdbg/error_codes.h errdbg/event_log.h
	errdbg/exceptions.h
)

SOURCE_GROUP ( "ERRDBG\\Source Files 2" FILES
	errdbg/assert.c errdbg/d_printf.c errdbg/error_codes.c
	errdbg/event_log.c errdbg/exceptions.cpp
)

SOURCE_GROUP ( "MMGR" )

SOURCE_GROUP ( "MMGR\\Header Files 3" FILES
	mmgr/memnodes.h mmgr/memutils.h mmgr/se_alloc.h
)

SOURCE_GROUP ( "MMGR\\Source Files 3" FILES
	mmgr/aset.c mmgr/mcxt.c mmgr/track.c
)

SOURCE_GROUP ( "PH" )

SOURCE_GROUP ( "PH\\Header Files 4" FILES
	ph/_pers_malloc.h ph/pers_heap.h
)

SOURCE_GROUP ( "PH\\Source Files 4" FILES
	ph/pers_free.cpp ph/pers_heap.cpp ph/pers_malloc.cpp
	ph/pers_realloc.cpp
)

SOURCE_GROUP ( "U" )

SOURCE_GROUP ( "U\\Header Files 5" FILES
	u/u.h u/udl.h u/uhdd.h u/ummap.h u/umutex.h u/upipe.h
	u/uprocess.h u/usecurity.h u/usem.h u/ushm.h u/usocket.h
	u/uthread.h u/utime.h u/uutils.h
)

SOURCE_GROUP ( "U\\Source Files 5" FILES
	u/u.c u/uhdd.c u/ummap.cpp u/umutex.c u/upipe.cpp
	u/uprocess.c u/usecurity.c u/usem.c u/ushm.c u/usocket.c
	u/uthread.c u/utime.c u/uutils.c
)

