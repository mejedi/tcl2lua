cmake_minimum_required (VERSION 3.0)
project (tcl2lua)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fdata-sections -ffunction-sections")

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    "/tmp/usr/local/include")

add_library(tclparser SHARED
    tclparser.c utf8.c jim-format.c)

set_target_properties(tclparser PROPERTIES
    PREFIX ""
    LINK_DEPENDS ${PROJECT_SOURCE_DIR}/exports)

target_link_libraries(tclparser
    -Wl,-dead_strip
    -Wl,-exported_symbols_list,${PROJECT_SOURCE_DIR}/exports
    -Wl,-flat_namespace
    -Wl,-undefined,suppress)
