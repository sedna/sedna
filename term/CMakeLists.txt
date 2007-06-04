ADD_LIBRARY ( term 

	# Header Files
	mainloop.h sprompt.h term_funcs.h term_globals.h term_ile.h

	# Source Files
	mainloop.cpp sprompt.cpp startup.cpp term_funcs.cpp
	term_globals.cpp term_ile.cpp
)

SOURCE_GROUP ( "Header Files 1" FILES
	mainloop.h sprompt.h term_funcs.h term_globals.h term_ile.h
)

SOURCE_GROUP ( "Source Files 1" FILES
	mainloop.cpp sprompt.cpp startup.cpp term_funcs.cpp
	term_globals.cpp term_ile.cpp
)

