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

# Otherwise, make ifort the default compiler
ifndef COMPILER
 COMPILER := ifort
endif

# Library include path
INC_NC    := -I$(INC_NETCDF) -I$(INC_HDF5)

# Library link path: first try to get the list of proper linking flags
# for this build of netCDF with nf-config and nc-config. 
LINK_NC   := $(shell $(BIN_NETCDF)/nf-config --flibs)
LINK_NC   += $(shell $(BIN_NETCDF)/nc-config --libs)
LINK_NC   := $(filter -l%,$(LINK_NC))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%% NOTE TO NcdfUtilities USERS: If you do not have netCDF-4.2 installed
#%%%% Then you can add/modify the linking sequence here.  (This sequence
#%%%% is a guess, but is probably good enough for other netCDF builds.)
ifeq ($(LINK_NC),) 
LINK_NC   := -lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lm -lz
endif
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Prepend the library directory path to the linking sequence
LINK_NC   := -L$(LIB_NETCDF) -L$(LIB_HDF5) $(LINK_NC)

#==============================================================================
# IFORT compilation options (default)
#==============================================================================
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
F90      = ifort $(FFLAGS) $(INC_NC)
LD       = ifort $(FFLAGS)
FREEFORM = -free

endif

#==============================================================================
# MPIF90 compilation options
#==============================================================================
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

F90      = mpifort $(FFLAGS) $(INC_NC)
LD       = mpifort $(FFLAGS)
FREEFORM = -free

endif

#==============================================================================
# Portland Group (PGI) compilation options
#==============================================================================
ifeq ($(COMPILER),pgi) 

# Pick correct options for debug run or regular run 
ifdef DEBUG
FFLAGS = -byteswapio -Mpreprocess -fast -Bstatic -mcmodel=medium
else
FFLAGS = -byteswapio -Mpreprocess -fast -mp -Mnosgimp -DHE4 -Bstatic -mcmodel=medium
endif

# Add option for "array out of bounds" checking
ifdef BOUNDS
FFLAGS += -C
endif

# Look for F90 module files in the $(MOD) directory
FFLAGS += -module $(MOD)

F90      = pgf90 $(FFLAGS) $(INC_NC)
LD       = pgf90 $(FFLAGS)
FREEFORM = -Mfree

endif

#==============================================================================
# SunStudio compilation options
#==============================================================================
ifeq ($(COMPILER),sun) 

# Default compilation options
# NOTE: -native builds in proper options for whichever chipset you have!
FFLAGS = -fpp -fast -stackvar -xfilebyteorder=big16:%all -native -DHE4

# Additional flags for parallel run
ifndef DEBUG
FFLAGS += -openmp=parallel
endif

# Add option for "array out of bounds" checking
ifdef BOUNDS
FFLAGS += -C
endif

#---------------------------------------------------------------
# If your compiler is under the name "f90", use these lines!
#F90      = f90 $(FFLAGS) $(INC_NC)
#LD       = f90 $(FFLAGS)
#---------------------------------------------------------------
# If your compiler is under the name "sunf90", use these lines!
F90      = sunf90 $(FFLAGS) $(INC_NC)
LD       = sunf90 $(FFLAGS)
##---------------------------------------------------------------
FREEFORM = -free

endif

#==============================================================================
# IBM/XLF compilation options
# NOTE: someone who runs on IBM compiler should check this !!!
#==============================================================================
ifeq ($(COMPILER),xlf) 

# Default compilation options
FFLAGS = -bmaxdata:0x80000000 -bmaxstack:0x80000000 -qfixed -qsuffix=cpp=f -q64

# Add optimization options
FFLAGS += -O3 -qarch=auto -qtune=auto -qcache=auto -qmaxmem=-1 -qstrict -DHE4

# Add more options for parallel run
ifndef DEBUG
FFLAGS += -qsmp=omp:opt -WF,-Dmultitask -qthreaded
endif

# Add option for "array out of bounds" checking
ifdef BOUNDS
FFLAGS += -C
endif

# Look for F90 module files in the $(MOD) directory
FFLAGS += -moddir=$(MOD) -M$(MOD)

F90      = xlf90_r $(FFLAGS) $(INC_NC)
LD       = xlf90_r $(FFLAGS)
FREEFORM = -qrealsize=8

endif

#==============================================================================
# Default compilation rules for *.f, *.f90, *.F, *.F90 and *.c files
#==============================================================================
.SUFFIXES: .f .F .f90 .F90 .c
.f.o:                   ; $(F90) -c $*.f
.F.o:                   ; $(F90) -c $*.F
.f90.o:                 ; $(F90) -c $(FREEFORM) $*.f90 
.F90.o:                 ; $(F90) -c $(FREEFORM) $*.F90 

#==============================================================================
# Export global variables so that the main Makefile will see these
#==============================================================================
export F90
export FREEFORM
export LD
export LINK_NC
#EOC

#==============================================================================
# Print variables for testing/debugging purposes (uncomment if necessary)
#==============================================================================
#headerinfo:
#	@echo '####### in Makefile_header.mk ########' 
#	@echo "compiler: $(COMPILER)"
#	@echo "debug   : $(DEBUG)"
#	@echo "bounds  : $(BOUNDS)"
#	@echo "f90     : $(F90)"
#	@echo "ld      : $(LD)"
#	@echo "link_nc : $(LINK_NC)"
#	@echo "cc      : $(CC)"

