#EOC
#------------------------------------------------------------------------------
#       NcdfUtilities: by Harvard Atmospheric Chemistry Modeling Group        !
#                      and NASA/GSFC, SIVO, Code 610.3                        !
#------------------------------------------------------------------------------
#BOP
#
# !IROUTINE: Makefile_header.mk (in Code subdirectory)
#
# !DESCRIPTION: This sub-makefile defines the variables which specify
# compilation options for the different compiler/platform combinations.  
# Also, the default makefile compilation rules are specified here.
#\\
#\\
# !REMARKS:
#  To build the programs, call "make" with the following syntax:
#                                                                             .
#    make TARGET [ OPTIONAL-FLAGS ]
#                                                                             .
#  To display a complete list of options, type "make help".
#                                                                             .
#  NOTE: The NcdfUtilities assumes that you are linking to a netCDF-4 
#  library by default.  If you are linking to a netCDF-3 library build,
#  then compile with the flag NETCDF3=yes.
#                                                                             .
#  The following variables are accepted either as command-line options,
#  or may be defined in your ~/.cshrc or ~/.bashrc file:
#                                                                             .
#  Variable     Description
#  ----------   -----------
#  BIN_NETCDF   Specifies the path for netCDF etc. executables
#  INC_NETCDF   Specifies the path for netCDF etc. include files & modules
#  LIB_NETCDF   Specifies the path for netCDF etc. libraries
#  BIN_HDF5     Specifies the path for HDF5   etc. executables
#  INC_HDF5     Specifies the path for HDF5   etc. include files & modules
#  LIB_HDF5     Specifies the path for HDF5   etc. libraries
#                                                                             .
#  The following variables are exported to the main-level Makefile:
#                                                                             .
#  Variable     Description
#  ----------   -----------
#  F90          Contains the Fortran compilation commands
#  FREEFORM     Contains the command to force F90 "free format" compilation
#  LD           Contains the command to link to libraries & make executable
#  LINK_NC      Specifies the command to link to the netCDF libraries
#  LINK_HDF5    Specifies the command to link to the HDF5   libraries
#                                                                             .
#  NOTE: The HDF5 library is only required if you are using netCDF-4.
#
# !REVISION HISTORY: 
#  04 Aug 2009 - R. Yantosca - Initial version
#  01 Apr 2010 - R. Yantosca - Modified for netCDF-4.0.1 compiled for MPI
#                              as built with the NASA baselibs.
#  24 Jan 2012 - R. Yantosca - ifort is now the default compiler.  We will
#                              invoke mpif90 at a later date.
#  30 Jan 2012 - R. Yantosca - Now create/read modules in the $MOD directory
#  30 Apr 2012 - R. Yantosca - Now set include & link paths 
#  30 Apr 2012 - R. Yantosca - Now use -mcmodel=medium option for IFORT, PGI
#  11 May 2012 - R. Yantosca - Now attempt to use nf-config, nc-config to
#                              obtain the library linking sequence.  This will
#                              make the Makefile much more portable
#  11 May 2012 - R. Yantosca - Now use INC_NETCDF, BIN_NETCDF, LIB_NETCDF
#                              env variables to specify directory paths
#  18 Jul 2014 - R. Yantosca - Now use INC_HDF5, BIN_HDF5, LIB_HDF5
#                              env variables to specify directory paths
#   9 Nov 2015 - R. Yantosca - Now set COMPILER=mpifort when MPI=yes
#EOP
#------------------------------------------------------------------------------
#BOC

#==============================================================================
# Initialization
#==============================================================================

# If MPI=yes, then the compiler should be mpifort
REGEXP :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(MPI)" =~ $(REGEXP) ]] && echo true),true)
 COMPILER :=mpifort
endif

# Default compiler setting
ifndef COMPILER
 COMPILER := $(FC)
endif

###############################################################################
###                                                                         ###
###  Set linker commands for local and external libraries (incl. netCDF)    ###
###                                                                         ###
###############################################################################

# netCDF Library include path.  
NC_INC_CMD           := -I$(INC_NETCDF)

# Get the version number (e.g. "4130"=netCDF 4.1.3; "4200"=netCDF 4.2, etc.)
NC_VERSION           :=$(shell nc-config --version)
NC_VERSION           :=$(shell echo "$(NC_VERSION)" | sed 's|netCDF ||g')
NC_VERSION           :=$(shell echo "$(NC_VERSION)" | sed 's|\.||g')
NC_VERSION_LEN       :=$(shell perl -e "print length $(NC_VERSION)")
ifeq ($(NC_VERSION_LEN),3)
 NC_VERSION          :=$(NC_VERSION)0
endif
ifeq ($(NC_VERSION_LEN),2) 
 NC_VERSION          :=$(NC_VERSION)00
endif

# Test if we have at least netCDF 4.2.0.0
AT_LEAST_NC_4200     :=$(shell perl -e "print ($(NC_VERSION) ge 4200)")

ifeq ($(AT_LEAST_NC_4200),1) 

  #-------------------------------------------------------------------------
  # netCDF 4.2 and higher:
  # Use "nf-config --flibs" and "nc-config --libs"
  # Test if a separate netcdf-fortran path is specified
  #-------------------------------------------------------------------------
  NC_LINK_CMD        := $(shell nf-config --flibs)
  NC_LINK_CMD        += $(shell nc-config --libs)

else

  #-----------------------------------------------------------------------
  # Prior to netCDF 4.2:
  # Use "nc-config --flibs"
  #-----------------------------------------------------------------------
  NC_LINK_CMD        := $(shell nc-config --flibs)

endif

#=============================================================================
#%%%%% FIX FOR USE WITH THE GEOS-Chem-Libraries (bmy, 1/13/15)
#%%%%% 
#%%%%% If your GEOS-Chem-Libraries netCDF/HDF5 package was built in one 
#%%%%% directory and then moved somewhere else, then nf-config and nc-config 
#%%%%% may not return the proper link directory path.  
#%%%%% 
#%%%%% To avoid this error, we shall test if the $GC_LIB environment variable 
#%%%%% contains the text "GEOS-Chem-Libraries".  (Recall that $GC_LIB is 
#%%%%% defined in either your .bashrc or .cshrc file depending on which Unix 
#%%%%% shell you use.)  If we find the text "GEOS-Chem-Libraries" in $GC_LIB, 
#%%%%% then we shall override the library path returned by nf-config and 
#%%%%% nc-config with the path specified by $GC_LIB.  This will ensure that 
#%%%%% we point to the location where the GEOS-Chem-Libraries are installed.
#%%%%%
#%%%%% NOTE: This fix should work for most users.  If it does not work, then
#%%%%% contact the GEOS-Chem Support Team (geos-chem-support@as.harvard.edu).
#%%%%%
REGEXP               :="GEOS-Chem-Libraries"
ifeq ($(shell [[ "$(LIB_NETCDF)" =~ $(REGEXP) ]] && echo true),true)
  NC_LINK_CMD        := $(filter -l%,$(NC_LINK_CMD))
  NC_LINK_CMD        :=-L$(LIB_NETCDF) $(NC_LINK_CMD)
endif
#=============================================================================


###############################################################################
###                                                                         ###
###  Define settings for the INTEL FORTRAN COMPILER (aka ifort)             ###
###                                                                         ###
###############################################################################

ifeq ($(COMPILER),ifort) 

# Pick correct options for debug run or regular run 
ifdef DEBUG
FFLAGS = -cpp -w -noalign -convert big_endian -g -traceback -mcmodel=medium
else
FFLAGS = -cpp -w -O2 -auto -noalign -convert big_endian -openmp -mcmodel=medium
endif

# Add option for "array out of bounds" checking
ifdef BOUNDS
FFLAGS += -CB
endif

# Also add traceback option
ifdef TRACEBACK
FFLAGS += -traceback
endif

# Look for F90 module files in the $(MOD) directory
FFLAGS += -module $(MOD)
F90      = ifort $(FFLAGS) $(NC_INC_CMD)
LD       = ifort $(FFLAGS)
FREEFORM = -free

endif

###############################################################################
###                                                                         ###
###  Define settings for the INTEL FORTRAN COMPILER WITH MPI (aka mpifort)  ###
###                                                                         ###
###############################################################################

ifeq ($(COMPILER),mpifort) 

# Pick correct options for debug run or regular run 
ifdef DEBUG
FFLAGS = -cpp -w -noalign -convert big_endian -g -traceback -mcmodel=medium
else
FFLAGS = -cpp -w -O2 -auto -noalign -convert big_endian -openmp -mcmodel=medium
endif

# Add option for "array out of bounds" checking
ifdef BOUNDS
FFLAGS += -CB
endif

# Also add traceback option
ifdef TRACEBACK
FFLAGS += -traceback
endif

# Look for F90 module files in the $(MOD) directory
FFLAGS += -module $(MOD)

F90      = mpifort $(FFLAGS) $(NC_INC_CMD)
LD       = mpifort $(FFLAGS)
FREEFORM = -free

endif

###############################################################################
###                                                                         ###
###  Define settings for the PORTLAND GROUP COMPILER (aka "pgfortran")      ###
###                                                                         ###
###############################################################################

ifeq ($(COMPILER),pgfortran) 

# Pick correct options for debug run or regular run 
ifdef DEBUG
FFLAGS = -Kieee -byteswapio -Mpreprocess -fast -mcmodel=medium
else
FFLAGS = -Kieee -byteswapio -Mpreprocess -fast -mp -Mnosgimp -mcmodel=medium
endif

# Add option for "array out of bounds" checking
ifdef BOUNDS
FFLAGS += -C
endif

# Look for F90 module files in the $(MOD) directory
FFLAGS += -module $(MOD)

F90      = pgfortran $(FFLAGS) $(NC_INC_CMD)
LD       = pgfortran $(FFLAGS)
FREEFORM = -Mfree

endif

###############################################################################
###                                                                         ###
###  Specify pattern rules for compiliation                                 ###
###  (i.e. tell "make" how to compile files w/ different extensions)        ###
###                                                                         ###
###############################################################################

.%.o : %.f
	$(F90) -c $<
%.o : %.F
	$(F90) -c $<
%.o : %.f90
	$(F90) -c $(FREEFORM) $<
%.o : %.F90
	$(F90) -c $(FREEFORM) $<
%.o : %.c
	$(CC) -c $*.c

###############################################################################
###                                                                         ###
###  Export global variables so that the main Makefile will see these       ###
###                                                                         ###
###############################################################################
 
export F90
export FREEFORM
export LD
export NC_LINK_CMD

#EOC

###############################################################################
###                                                                         ###
###  Debug print output.  Normally you will leave the following lines       ###
###  commented out.  Uncomment these lines for testing.                     ###
###                                                                         ###
###############################################################################

#headerinfo:
#	@echo '####### in Makefile_header.mk ########' 
#	@echo "compiler: $(COMPILER)"
#	@echo "debug   : $(DEBUG)"
#	@echo "bounds  : $(BOUNDS)"
#	@echo "f90     : $(F90)"
#	@echo "ld      : $(LD)"
#	@echo "inc_nc  : $(NC_INC_CMD)"
#	@echo "link_nc : $(NC_LINK_CMD)"
#	@echo "cc      : $(CC)"

