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
#  See https://github.com/geoschem/ncdfutil for complete history
#EOP
#------------------------------------------------------------------------------
#BOC

###############################################################################
###                                                                         ###
###  Determine the compiler that is being used                              ###
###                                                                         ###
###############################################################################

# Test if we are using mpif90 or mpifort (assume Intel)
REGEXP :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(HPC)" =~ $(REGEXP) ]] && echo true),true)
 IS_HPC           :=1
 COMPILE_CMD      :=mpif90
else
 IS_HPC           :=0
 COMPILE_CMD      :=$(FC)
endif

# Default setting for COMPILER_FAMILY
COMPILER_FAMILY   :=undefined

# Test if we are using mpif90 or mpifort (assume Intel)
REGEXP :=(^[Mm][Pp][Ii])
ifeq ($(shell [[ "$(FC)" =~ $(REGEXP) ]] && echo true),true)
 COMPILER_FAMILY  :=Intel
endif

# Test if we are using the Intel Fortran Compiler
REGEXP :=(^[Ii][Ff][Oo][Rr][Tt])
ifeq ($(shell [[ "$(FC)" =~ $(REGEXP) ]] && echo true),true)
 COMPILER_FAMILY  :=Intel
endif

# Test if we are using the GNU Fortran Compiler
REGEXP :=(^[Gg][Ff][Oo][Rr][Tt][Rr][Aa][Nn])
ifeq ($(shell [[ "$(FC)" =~ $(REGEXP) ]] && echo true),true)
 COMPILER_FAMILY  :=GNU
endif

# Test if we are using the Portland Group compiler
REGEXP :=(^[Pp][Gg][Ff]|^[Pp][Gg][Ii])
ifeq ($(shell [[ "$(FC)" =~ $(REGEXP) ]] && echo true),true)
 COMPILER_FAMILY  :=PGI
endif

# Exit with error if the compiler is not one of the above
ifeq ($(COMPILER_FAMILY),undefined) 
  $(error 'Unknown Fortran compiler; check your FC environment variable!' )
endif

###############################################################################
###                                                                         ###
###  Set linker commands for local and external libraries (incl. netCDF)    ###
###                                                                         ###
###############################################################################

ifdef NETCDF_INCLUDE
 NC_INC_CMD           := -I$(NETCDF_INCLUDE)
endif

ifdef NETCDF_FORTRAN_INCLUDE
 NC_INC_CMD           += -I$(NETCDF_FORTRAN_INCLUDE)
endif

#-------------------------------------------------------------------------
# netCDF 4.2 and higher:
# Use "nf-config --flibs" and "nc-config --libs"
# Test if a separate netcdf-fortran path is specified
#-------------------------------------------------------------------------
NC_LINK_CMD  := $(shell $(NETCDF_FORTRAN_HOME)/bin/nf-config --flibs)
NC_LINK_CMD  += $(shell $(NETCDF_HOME)/bin/nc-config --libs)

###############################################################################
###                                                                         ###
###  Test if the netCDF library was built with compression enabled          ###
###                                                                         ###
###############################################################################

# Test if the "nf_def_var_deflate" function is defined in netcdf.inc
# Look for netcdf.inc where the netCDF-Fortran library is located
ifdef NETCDF_FORTRAN_INCLUDE
  GREP :=$(strip $(shell grep nf_def_var_deflate $(NETCDF_FORTRAN_INCLUDE)/netcdf.inc))
else
  GREP :=$(strip $(shell grep nf_def_var_deflate $(NETCDF_INCLUDE)/netcdf.inc))
endif

# Look for the second word of the combined search results
WORD                 :=$(word 2,"$(GREP)")

# If it matches "nf_def_var_deflate", then define Cpp flag NC_HAS_COMPRESSION 
ifeq ($(WORD),nf_def_var_deflate)
  USER_DEFS          += -DNC_HAS_COMPRESSION
endif

###############################################################################
###                                                                         ###
###  Define settings for the INTEL FORTRAN COMPILER (aka ifort)             ###
###                                                                         ###
###############################################################################

ifeq ($(COMPILER_FAMILY),Intel) 

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
F90      = $(COMPILE_CMD) $(FFLAGS) $(NC_INC_CMD)
LD       = $(COMPILE_CMD) $(FFLAGS)
FREEFORM = -free

endif

###############################################################################
###                                                                         ###
###  Define settings for the PORTLAND GROUP COMPILER (aka "pgfortran")      ###
###                                                                         ###
###############################################################################

ifeq ($(COMPILER_FAMILY),PGI) 

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

F90      = $(COMPILE_CMD) $(FFLAGS) $(NC_INC_CMD)
LD       = $(COMPILE_CMD) $(FFLAGS)
FREEFORM = -Mfree

endif

###############################################################################
###                                                                         ###
###  Define settings for the GNU FORTRAN COMPILER (aka gfortran)            ###
###                                                                         ###
###############################################################################

ifeq ($(COMPILER_FAMILY),GNU) 

  # Base set of compiler flags
  FFLAGS             :=-cpp -w -std=legacy -fautomatic -fno-align-commons
  FFLAGS             += -fconvert=big-endian
  FFLAGS             += -fno-range-check

  # Default optimization level for all routines (-O2)
  ifndef OPT
    # Options of interest
    #  -limf                Intel math libraries - machine must have them
    #  -O3                  Highest safe optimization level
    #  -march=native        Make the binary machine-specific. If in doubt, 
    #                        use a specific architecture, eg...
    #  -march=corei7-avx    Binary uses optimizations for 
    #                        Intel Sandy-Bridge Xeon (e.g. E5-2680)
    #  -mfpmath=sse         Use SSE extensions
    #  -funroll-loops       Enable loop unrolling
    OPT              := -O3 -funroll-loops
    #OPT              := -O3 -march=corei7-avx -mfpmath=sse -funroll-loops
  endif

  # Pick compiler options for debug run or regular run 
  #-fcheck=all would be more comprehensive but would force bounds checking
  REGEXP             := (^[Yy]|^[Yy][Ee][Ss])
  ifeq ($(shell [[ "$(DEBUG)" =~ $(REGEXP) ]] && echo true),true)
#    FFLAGS           += -g -O0 -gdwarf-3 -gstrict-dwarf 
    FFLAGS           += -g -O0
    FFLAGS           += -Wall -Wextra -Wconversion
    FFLAGS           += -Warray-temporaries -fcheck-array-temporaries
    TRACEBACK        := yes
    USER_DEFS        += -DDEBUG
  else
    FFLAGS           += $(OPT)
  endif

  # Prevent any optimizations that would change numerical results
  #GFORTRAN_BAD#FFLAGS             += -fp-model source

  # Turn on OpenMP parallelization
  REGEXP             :=(^[Yy]|^[Yy][Ee][Ss])
  ifeq ($(shell [[ "$(OMP)" =~ $(REGEXP) ]] && echo true),true)
    FFLAGS           += -fopenmp
  endif

  # Get Operating System (Linux = Linux; Darwin = MacOSX)
  ifndef UNAME
    UNAME            :=$(shell uname)
  endif

  # OSX compilation options
  ifeq ($(UNAME),Darwin)
    # This has not yet been tested
    $(error $(ERR_OSCOMP))
  #  FFLAGS           += -Wl,-stack_size,0x2cb410000  # 12 GB of stack space
  #  ifdef DEBUG
  #    FFLAGS         += -g0 -debug -save-temps -fpic -Wl,-no_pie
  #  endif
  endif

  # Add options for medium memory model.  This is to prevent G-C from 
  # running out of memory at hi-res, especially when using netCDF I/O.
  ifneq ($(UNAME),Darwin)
    #GFORTRAN_BAD#FFLAGS           += -mcmodel=medium -shared-intel
    FFLAGS           += -mcmodel=medium
  endif

  # Turn on checking for floating-point exceptions
  # These are approximately equivalent to -fpe0 -ftrapuv in IFORT
  REGEXP             :=(^[Yy]|^[Yy][Ee][Ss])
  ifeq ($(shell [[ "$(FPE)" =~ $(REGEXP) ]] && echo true),true)
    FFLAGS           += -ffpe-trap=invalid,zero,overflow -finit-real=snan
  endif
  ifeq ($(shell [[ "$(FPEX)" =~ $(REGEXP) ]] && echo true),true)
    FFLAGS           += -ffpe-trap=invalid,zero,overflow -finit-real=snan
  endif

  # Add option for "array out of bounds" checking
  REGEXP             := (^[Yy]|^[Yy][Ee][Ss])
  ifeq ($(shell [[ "$(BOUNDS)" =~ $(REGEXP) ]] && echo true),true)
    FFLAGS           += -fbounds-check
  endif

  # Also add traceback option
  REGEXP             :=(^[Yy]|^[Yy][Ee][Ss])
  ifeq ($(shell [[ "$(TRACEBACK)" =~ $(REGEXP) ]] && echo true),true)
    FFLAGS           += -fbacktrace
    ifndef DEBUG
       FFLAGS += -g
    endif
  endif

  # Loosen KPP tolerances upon non-convergence and try again
  REGEXP             :=(^[Yy]|^[Yy][Ee][Ss])
  ifeq ($(shell [[ "$(KPP_SOLVE_ALWAYS)" =~ $(REGEXP) ]] && echo true),true)
    USER_DEFS        += -DKPP_SOLVE_ALWAYS
  endif

  # Add flexible precision declaration
  ifeq ($(PRECISION),8)
    USER_DEFS        += -DUSE_REAL8
  endif

  # Add timers declaration
  ifeq ($(TIMERS),1)
    USER_DEFS        += -DUSE_TIMERS
  endif

  # Append the user options in USER_DEFS to FFLAGS
  FFLAGS             += $(USER_DEFS)

  # Include options (i.e. for finding *.h, *.mod files)
  INCLUDE :=-J$(MOD) $(NC_INC_CMD)

  # Do not append the ESMF/MAPL/FVDYCORE includes for ISORROPIA, because it 
  # will not compile.  ISORROPIA is slated for removal shortly. (bmy, 11/21/14)
  INCLUDE_ISO        :=$(INCLUDE)

  # Append the ESMF/MAPL/FVDYCORE include commands
  ifeq ($(HPC),yes)
    INCLUDE          += $(MAPL_INC) $(ESMF_MOD) $(ESMF_INC) $(FV_INC)
  endif

  # Set the standard compiler variables
  F90                :=$(COMPILE_CMD) $(FFLAGS) $(NC_INC_CMD)
  LD                 :=$(COMPILE_CMD) $(FFLAGS)
  FREEFORM           := -ffree-form -ffree-line-length-none

endif

###############################################################################
###                                                                         ###
###  Specify pattern rules for compiliation                                 ###
###  (i.e. tell "make" how to compile files w/ different extensions)        ###
###                                                                         ###
###############################################################################

%.o : %.f
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
#	@echo "THE_COMPILER : $(THE_COMPILER)"
#	@echo "DEBUG        : $(DEBUG)"
#	@echo "BOUNDS       : $(BOUNDS)"
#	@echo "F90          : $(F90)"
#	@echo "LD           : $(LD)"
#	@echo "NC_INC_CMD   : $(NC_INC_CMD)"
#	@echo "NC_LINK_CMD  : $(NC_LINK_CMD)"
#	@echo "CC           : $(CC)"
