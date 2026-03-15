cmake_minimum_required(VERSION 3.16)

project(dnes LANGUAGES C)

option(DNES_VENDORED "Use vendored libraries" ON)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(DNES_VENDORED)
    add_subdirectory(vendored/SDL EXCLUDE_FROM_ALL)
else()
    if(APPLE)
        # Let's assume that if we're on Apple, we use Homebrew's SDL installation.
        list(APPEND CMAKE_PREFIX_PATH "/opt/homebrew")
    endif()
    # 1. Look for a SDL3 package,
    # 2. look for the SDL3-shared component, and
    # 3. fail if the shared component cannot be found.
    find_package(SDL3 REQUIRED CONFIG REQUIRED COMPONENTS SDL3-shared)
endif()

# find all source files in the mapper/ directory
file(GLOB_RECURSE MAPPER_SOURCES "mapper/*.c")

file(GLOB_RECURSE PACK_2A03_SOURCES "pack_2a03/*.c")

file(GLOB_RECURSE FRONTEND_SDL3_SOURCES "frontend/sdl3/*.c")

add_library(dnes_core STATIC
                      ppu_2c02.c
                      ${MAPPER_SOURCES}
                      ${PACK_2A03_SOURCES}
                      cartridge.c
                      dnes.c
                      bus.c
                      ram.c
                      utils.c
                      )

target_include_directories(dnes_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_compile_options(dnes_core PRIVATE
    # $<$<C_COMPILER_ID:MSVC>: /W4 /WX>
    $<$<NOT:$<C_COMPILER_ID:MSVC>>: -Wall -Wextra -Wconversion -Wsign-conversion -Wno-unused-function>
)

add_executable(dnes ${FRONTEND_SDL3_SOURCES})

target_include_directories(dnes PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_compile_options(dnes PRIVATE
    # $<$<C_COMPILER_ID:MSVC>: /W4 /WX>
    $<$<NOT:$<C_COMPILER_ID:MSVC>>: -Wall -Wextra -Wconversion -Wsign-conversion -Wno-unused-function>
)

target_link_libraries(dnes PRIVATE dnes_core SDL3::SDL3)
