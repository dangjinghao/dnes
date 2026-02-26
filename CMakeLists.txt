cmake_minimum_required(VERSION 3.16)
project(dnes LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)


if(APPLE)
    list(APPEND CMAKE_PREFIX_PATH "/opt/homebrew")
endif()

set(SRC_FILES
    cpu_6502.c
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
)

add_executable(dnes ${SRC_FILES})


target_compile_options(dnes PRIVATE
    $<$<C_COMPILER_ID:MSVC>: /W4 /WX>
    $<$<NOT:$<C_COMPILER_ID:MSVC>>: -Wall -Wextra -Wconversion -Wsign-conversion -Wno-unused-function>
)

find_package(SDL3 REQUIRED CONFIG)
target_compile_definitions(dnes PRIVATE SDL_MAIN_USE_CALLBACKS)
target_link_libraries(dnes PRIVATE SDL3::SDL3)
