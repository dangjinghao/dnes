cmake_minimum_required(VERSION 3.16)
project(dnes)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=gnu11 -Wall -Wextra -Wconversion -Wsign-conversion -Wno-unused-function -g")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(SRC_FILES
    cpu_6502.c
    ppu_2c02.c
    mapper_000.c
    cartridge.c
    dnes.c
    bus.c
    ram.c
    utils.c
)

add_executable(dnes ${SRC_FILES})
