ADD_LIBRARY ( gov 

	# Header Files
	config_utils.h gov_functions.h gov_globals.h gov_table.h
	listener.h

	# Source Files
	config_utils.cpp gov.cpp gov_functions.cpp gov_globals.cpp
	gov_table.cpp listener.cpp rc.cpp stop_server.cpp
)

SOURCE_GROUP ( "Header Files 1" FILES
	config_utils.h gov_functions.h gov_globals.h gov_table.h
	listener.h
)

SOURCE_GROUP ( "Source Files 1" FILES
	config_utils.cpp gov.cpp gov_functions.cpp gov_globals.cpp
	gov_table.cpp listener.cpp rc.cpp stop_server.cpp
)

