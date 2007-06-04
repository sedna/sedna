ADD_LIBRARY ( sm 

	# Header Files
	cdb_globals.h db_utils.h sm_functions.h sm_globals.h
	trmgr.h

	# Source Files
	cdb.cpp cdb_globals.cpp db_utils.cpp ddb.cpp sm.cpp
	sm_functions.cpp sm_globals.cpp smsd.cpp trmgr.cpp

	# Header Files, from bufmgr
	bufmgr/blk_mngmt.h bufmgr/bm_core.h bufmgr/bm_functions.h
	bufmgr/bm_rcv.h

	# Source Files, from bufmgr
	bufmgr/blk_mngmt.cpp bufmgr/bm_core.cpp
	bufmgr/bm_functions.cpp bufmgr/bm_rcv.cpp

	# Header Files, from llmgr
	llmgr/llmgr.h llmgr/llmgr_core.h

	# Source Files, from llmgr
	llmgr/llmgr.cpp llmgr/llmgr_core.cpp
	llmgr/llmgr_helpers.cpp

	# Header Files, from lm
	lm/TransCB.h lm/lm_globals.h lm/lock_table.h
	lm/trans_table.h

	# Source Files, from lm
	lm/TransCB.cpp lm/lm_globals.cpp lm/lock_table.cpp
	lm/trans_table.cpp

	# Header Files, from plmgr
	plmgr/plmgr.h plmgr/plmgr_core.h plmgr/sm_plmgr.h
	plmgr/tr_plmgr.h

	# Source Files, from plmgr
	plmgr/plmgr.cpp plmgr/plmgr_core.cpp
	plmgr/plmgr_helpers.cpp plmgr/sm_plmgr.cpp
)

SOURCE_GROUP ( "Header Files 1" FILES
	cdb_globals.h db_utils.h sm_functions.h sm_globals.h
	trmgr.h
)

SOURCE_GROUP ( "Source Files 1" FILES
	cdb.cpp cdb_globals.cpp db_utils.cpp ddb.cpp sm.cpp
	sm_functions.cpp sm_globals.cpp smsd.cpp trmgr.cpp
)

SOURCE_GROUP ( "BUFMGR" )

SOURCE_GROUP ( "BUFMGR\\Header Files 2" FILES
	bufmgr/blk_mngmt.h bufmgr/bm_core.h bufmgr/bm_functions.h
	bufmgr/bm_rcv.h
)

SOURCE_GROUP ( "BUFMGR\\Source Files 2" FILES
	bufmgr/blk_mngmt.cpp bufmgr/bm_core.cpp
	bufmgr/bm_functions.cpp bufmgr/bm_rcv.cpp
)

SOURCE_GROUP ( "LLMGR" )

SOURCE_GROUP ( "LLMGR\\Header Files 3" FILES
	llmgr/llmgr.h llmgr/llmgr_core.h
)

SOURCE_GROUP ( "LLMGR\\Source Files 3" FILES
	llmgr/llmgr.cpp llmgr/llmgr_core.cpp
	llmgr/llmgr_helpers.cpp
)

SOURCE_GROUP ( "LM" )

SOURCE_GROUP ( "LM\\Header Files 4" FILES
	lm/TransCB.h lm/lm_globals.h lm/lock_table.h
	lm/trans_table.h
)

SOURCE_GROUP ( "LM\\Source Files 4" FILES
	lm/TransCB.cpp lm/lm_globals.cpp lm/lock_table.cpp
	lm/trans_table.cpp
)

SOURCE_GROUP ( "PLMGR" )

SOURCE_GROUP ( "PLMGR\\Header Files 5" FILES
	plmgr/plmgr.h plmgr/plmgr_core.h plmgr/sm_plmgr.h
	plmgr/tr_plmgr.h
)

SOURCE_GROUP ( "PLMGR\\Source Files 5" FILES
	plmgr/plmgr.cpp plmgr/plmgr_core.cpp
	plmgr/plmgr_helpers.cpp plmgr/sm_plmgr.cpp
)

