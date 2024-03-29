#EOC
#------------------------------------------------------------------------------
#       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
#                      and NASA/GSFC, SIVO, Code 610.3                        !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: Makefile (in Code subdirectory)
#
# !DESCRIPTION: Makefile for the NcdfUtilities package.
#\\
#\\
# !REMARKS:
# To build the program, call "make" with the following syntax:
#
#   make TARGET [ OPTIONAL-FLAGS ]
#                                                                             .
#  To display a complete list of options, type "make help".
#                                                                             .
#  NOTE: The NcdfUtilities assumes that you are linking to a netCDF-4 
#  library by default.  If you are linking to a netCDF-3 library build,
#  then compile with the flag NETCDF3=yes.
#                                                                             .
#  Makefile uses the following variables:
#                                                                             .
#  Variable   Description
#  --------   -----------
#  BIN        Specifies the directory where the executable will be sent
#  DOC        Specifies the directory where documentation will be built
#  EXE        Specifies the name of the executable file
#  F90        Contains the default Fortran compilation commands
#  LD         Contains the command to link to libraries & make executable
#  LIB        Specifies the directory where the libNcUtils.a files will be sent
#  LINK_NC    Contains the comand to link to the HDF library
#  SHELL      Specifies the shell for "make" to use (usually SHELL=/bin/sh)
#                                                                             .
#  F90, LD, FREEFORM, and LINK_NC are included from "Makefile_header.mk"
#
# !REVISION HISTORY: 
#  04 Aug 2009 - R. Yantosca - Initial version
#  See https://github.com/geoschem/ncdfutil for complete history
#EOP
#------------------------------------------------------------------------------
#BOC

#==============================================================================
# Initialization
#==============================================================================

# Define variables
SHELL := /bin/bash
BIN   := ../bin
DOC   := ../doc
EXE   := TestNcdfUtil.x
LIB   := ../lib
MOD   := ../mod

# Include header file.  This returns variables F90, LD, LINK_NC
# as well as the default Makefile compilation rules for source code files.
include ./Makefile_header.mk

#==============================================================================
# Filename section
#
# NOTES:
# (1) The function $(wildcard m_*.F90) returns all files matching m_*.F90, etc.
# (2) The syntax $(SRC:.F90=.o) is a shorthand for the "patsubst" function. 
#==============================================================================

#-----------------
# Objects
#-----------------

# List of source files
SRC := $(wildcard *.F) $(wildcard *.F90)

# Replace .f and .f90 extensions with *.o
TMP := $(SRC:.F=.o)
OBJ := $(TMP:.F90=.o)

# Files for standalone test driver (code, object, exe)
OBJx := $(EXE:.x=.o)

#-----------------
# Dependencies
#-----------------
charpak_mod.o            : charpak_mod.F90

julday_mod.o             : julday_mod.F90

m_do_err_out.o           : m_do_err_out.F90

m_netcdf_io_close.o      : m_netcdf_io_close.F90        \
                           m_do_err_out.o

m_netcdf_io_create.o     : m_netcdf_io_create.F90       \
                           m_do_err_out.o

m_netcdf_io_define.o     : m_netcdf_io_define.F90       \
                           m_do_err_out.o

m_netcdf_io_get_dimlen.o : m_netcdf_io_get_dimlen.F90   \
                           m_do_err_out.o

m_netcdf_io_handle_err.o : m_netcdf_io_handle_err.F90   \
                           m_do_err_out.o

m_netcdf_io_open.o       : m_netcdf_io_open.F90         \
                           m_do_err_out.o

m_netcdf_io_read.o       : m_netcdf_io_read.F90         \
                           m_do_err_out.o

m_netcdf_io_readattr.o   : m_netcdf_io_readattr.F90     \
                           m_do_err_out.o

m_netcdf_io_write.o      : m_netcdf_io_write.F90        \
                           m_do_err_out.o

ncdf_mod.o               : ncdf_mod.F90                 \
                           m_netcdf_io_open.o           \
                           m_netcdf_io_get_dimlen.o     \
                           m_netcdf_io_read.o           \
                           m_netcdf_io_readattr.o       \
                           m_netcdf_io_close.o          \
                           m_netcdf_io_create.o         \
                           m_netcdf_io_define.o         \
                           m_netcdf_io_write.o          \
                           m_netcdf_io_checks.o         \
                           julday_mod.o                 \
                           charpak_mod.o

TestNcdfUtilities.o      : TestNcdfUtil.F90             \
                           m_do_err_out.o               \
                           m_netcdf_io_handle_err.o     \
                           m_netcdf_io_create.o         \
                           m_netcdf_io_define.o         \
                           m_netcdf_io_write.o          \
                           m_netcdf_io_open.o           \
                           m_netcdf_io_get_dimlen.o     \
                           m_netcdf_io_read.o           \
                           m_netcdf_io_readattr.o       \
                           m_netcdf_io_close.o

#==============================================================================
# Makefile targets
#==============================================================================

.PHONY: all doc docclean clean realclean help test

all: lib check

lib: $(OBJ)
	$(AR) crs libNcUtils.a $(OBJ)
	mv libNcUtils.a $(LIB)

check: $(OBJx)
	$(LD) $(OBJx) -L$(LIB) -lNcUtils $(NC_LINK_CMD) -o $(EXE)
	./$(EXE) 

doc:
	$(MAKE) -C $(DOC) doc

clean:
	rm -f *.a *.mod *.o *.nc *.x 

distclean:
	$(MAKE) clean
	rm -f $(BIN)/*.x
	rm -f $(LIB)/*.a
	rm -f $(MOD)/*.mod

test: check

#==============================================================================
# Help Screen
#==============================================================================

help:
	@echo 'Usage: make TARGET [ OPTIONAL-FLAGS ]'
	@echo ''
	@echo 'TARGET may be one of the following:'
	@echo 'lib        Compiles code and builds the libNcUtils.a library file'
	@echo 'check      Compiles and runs a test program to check the libNcUtils.a library'
	@echo 'test       Synonym for "check"'
	@echo 'all        Synonym for "lib check" (default target)'
	@echo 'clean      Removes *.o *.mod and executable files in Code subdirectory'
	@echo 'realclean  Makes clean, then removes contents of bin, lib, doc subdirectories'
	@echo 'doc        Builds documentation files (*.ps, *.pdf) in doc subdirectory'
	@echo 'docclean   Removes documentation files from doc subdirectory'
	@echo 'help       Displays this help screen'
	@echo ''
	@echo 'OPTIONAL-FLAGS may be:'
	@echo 'COMPILER=___   Options: ifort pgi sun xlf (default is ifort)'
	@echo 'NETCDF3=yes    Specifies that we are using a netCDF-3 build'
	@echo '                (default is to assume netCDF-4)'
	@echo 'DEBUG=yes      Compiles GEOS-Chem for use w/ a debugger'
	@echo 'BOUNDS=yes     Turns on subscript-array checking (for debugging purposes)'
	@echo 'TRACEBACK=yes  Compiles with -traceback flag (ifort only)'

#
#EOC
