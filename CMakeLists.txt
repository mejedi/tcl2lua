cmake_minimum_required (VERSION 3.0)
project (tclmeta)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    "/tmp/usr/local/include")

add_library(tclmeta SHARED
    tclmeta.c utf8.c jim-format.c)

set_target_properties(tclmeta PROPERTIES PREFIX "")
target_link_libraries(tclmeta
    -Wl,-exported_symbols_list,${CMAKE_CURRENT_SOURCE_DIR}/exports
    -Wl,-flat_namespace
    -Wl,-undefined,suppress)
