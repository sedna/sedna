# try to find Latex
find_package(LATEX)

if (PDFLATEX_COMPILER)
    message(STATUS "PdfLatex compiler found -- ${PDFLATEX_COMPILER}")
else (PDFLATEX_COMPILER)
    message(STATUS "PdfLatex compiler not found -- cannot build docs")
    set(MAKE_DOC OFF)
endif (PDFLATEX_COMPILER)

# create rules to build documentation
if (MAKE_DOC)
    set(docs_SRC AdminGuide/AdminGuide.tex ProgGuide/ProgGuide.tex ProgGuide/ClientServerProtocol/ClientServerProtocol.tex QuickStart/QuickStart.tex)
    set(doc_out_pdfs)

    foreach(doc ${docs_SRC})
        get_filename_component(doc_out ${doc} NAME_WE)
        set(doc_out ${CMAKE_CURRENT_BINARY_DIR}/${doc_out}.pdf)
        list(APPEND doc_out_pdfs ${doc_out})
        file(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/${doc} doc_path)
        add_custom_command(
            OUTPUT ${doc_out}
            COMMAND ${PDFLATEX_COMPILER} ${doc_path}
            COMMAND ${PDFLATEX_COMPILER} ${doc_path}
            COMMAND ${PDFLATEX_COMPILER} ${doc_path}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        )
    endforeach(doc ${docs_SRC})

    # custom target for the driver
    add_custom_target(sedna_docs ALL DEPENDS ${doc_out_pdfs})

    # install docs
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/AdminGuide.pdf
                  ${CMAKE_CURRENT_BINARY_DIR}/ProgGuide.pdf
                  ${CMAKE_CURRENT_BINARY_DIR}/QuickStart.pdf
                  ${CMAKE_CURRENT_BINARY_DIR}/ClientServerProtocol.pdf
            DESTINATION doc
            OPTIONAL)

endif (MAKE_DOC)
