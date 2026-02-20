cmake_minimum_required(VERSION 3.16)
project(dnes)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=gnu11 -Wall -Wextra -Wconversion -Wsign-conversion -Wno-unused-function -g")

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
)

add_executable(dnes ${SRC_FILES})

find_package(SDL3 REQUIRED CONFIG REQUIRED COMPONENTS SDL3-shared)
target_compile_definitions(dnes PRIVATE SDL_MAIN_USE_CALLBACKS)
target_link_libraries(dnes PRIVATE SDL3::SDL3)
