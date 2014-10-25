cmake_minimum_required (VERSION 2.8.9)

enable_language(Fortran)

project(static_condensation)

set(CMAKE_Fortran_COMPILER "gfortran")
set(CMAKE_Fortran_FLAGS "-Wall -ffree-form -O3")

add_custom_target(output
    COMMAND main < ${CMAKE_SOURCE_DIR}/input
    DEPENDS main
    WORKING_DIRECTORY ${CMAKE_PROJECT_DIR}
)

add_custom_target(output.pdf
    COMMAND ${CMAKE_SOURCE_DIR}/multiplot.m ${CMAKE_SOURCE_DIR}/input output output.pdf
    DEPENDS output
    WORKING_DIRECTORY ${CMAKE_PROJECT_DIR}
)

add_executable(main
  main.f
  utilities.f
)
