#------------------------------------------------------------------------------
#       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
#                      and NASA/GSFC, SIVO, Code 610.3                        !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: Makefile (main-level)
#
# !DESCRIPTION: Makefile for the NetCDF Utilities.
#\\
#\\
# !REMARKS:
# To build the program, call "make" with the following syntax:
#
#   make TARGET [ OPTIONAL-FLAGS ]
#
# To display a complete list of options, type "make help".
#
# !REVISION HISTORY: 
#  03 Aug 2009 - R. Yantosca - Initial version
#EOP
#------------------------------------------------------------------------------
#BOC

#==============================================================================
# Initialization
#==============================================================================

# Define variables
SHELL = /bin/bash
DIR   = Code

#==============================================================================
# Makefile targets
#==============================================================================

.PHONY: all lib check clean realclean doc docclean help

all: 
	$(MAKE) -C $(DIR) all

lib:
	$(MAKE) -C $(DIR) lib

check: 
	$(MAKE) -C $(DIR) check

clean:
	$(MAKE) -C $(DIR) clean

distclean:
	$(MAKE) -C $(DIR) realclean

help:
	$(MAKE) -C $(DIR) help

#EOC
