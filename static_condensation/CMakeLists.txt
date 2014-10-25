cmake_minimum_required (VERSION 2.8.9)

enable_language(Fortran)

project(static_condensation)

set(CMAKE_Fortran_COMPILER "gfortran")
set(CMAKE_Fortran_FLAGS "-Wall -ffree-form -O3")

#set(MODULES
#  "utilities"
#)

add_executable(main
  main.f
  utilities.f
)

#foreach (p ${MODULES})
#  add_executable (${p} "${p}.f")
#endforeach (p)