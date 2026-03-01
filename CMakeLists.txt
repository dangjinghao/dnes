cmake_minimum_required(VERSION 3.16)

project(dnes LANGUAGES C)

option(DNES_VENDORED "Use vendored libraries" ON)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(DNES_VENDORED)
    # This assumes you have added SDL as a submodule in vendored/SDL
    add_subdirectory(vendored/SDL EXCLUDE_FROM_ALL)
else()
    if(APPLE)
        list(APPEND CMAKE_PREFIX_PATH "/opt/homebrew")
    endif()
    # 1. Look for a SDL3 package,
    # 2. look for the SDL3-shared component, and
    # 3. fail if the shared component cannot be found.
    find_package(SDL3 REQUIRED CONFIG REQUIRED COMPONENTS SDL3-shared)
endif()

add_executable(dnes cpu_6502.c
                    ppu_2c02.c
                    mapper_000.c
                    cartridge.c
                    dnes.c
                    bus.c
                    ram.c
                    utils.c
                    screen.c
                    controller.c
                    apu.c
                    dma.c)


target_compile_options(dnes PRIVATE
    $<$<C_COMPILER_ID:MSVC>: /W4 /WX>
    $<$<NOT:$<C_COMPILER_ID:MSVC>>: -Wall -Wextra -Wconversion -Wsign-conversion -Wno-unused-function>
)

target_compile_definitions(dnes PRIVATE SDL_MAIN_USE_CALLBACKS)
target_link_libraries(dnes PRIVATE SDL3::SDL3)
