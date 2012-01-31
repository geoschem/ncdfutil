#EOC
#------------------------------------------------------------------------------
#          Harvard University Atmospheric Chemistry Modeling Group            !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: Makefile (in doc subdirectory)
#
# !DESCRIPTION: Makefile for building the documentation (in PDF and 
#  PostScript formats) for the NcdfUtilities source code and Makefiles.
#  and Perl scripts.
#\\
#\\
# !REMARKS:
# To build the documentation, call "make" with the following syntax:
#
#   make TARGET [ OPTIONAL-FLAGS ]
#
# To display a complete list of options, type "make help".
#
# You must have the LaTeX utilities (latex, dvips, dvipdf) installed
# on your system in order to build the documentation.
#
# !REVISION HISTORY: 
#  04 Aug 2009 - R. Yantosca - Initial version
#  25 Jan 2012 - R. Yantosca - Now add m_netcdf_io_readattr.F90
#  31 Jan 2012 - R. Yantosca - Now create auto doc for Perl scripts
#EOP
#------------------------------------------------------------------------------
#BOC

#==============================================================================
# Initialization
#==============================================================================

# Define variables
SHELL   := /bin/sh
CODEDIR := ../Code
DOCDIR  := ./
PERLDIR := ../perl
ROOTDIR := ../

.PHONY: doc clean

doc: srcdoc makedoc perldoc

clean:
	rm -f *.tex *.ps *.pdf

#==============================================================================
# Make documentation for source code
#==============================================================================

# Files for library build (code & objects)
SRC1 :=                               \
$(DOCDIR)/src.intro                   \
$(CODEDIR)/m_do_err_out.F90           \
$(CODEDIR)/m_netcdf_io_checks.F90     \
$(CODEDIR)/m_netcdf_io_close.F90      \
$(CODEDIR)/m_netcdf_io_create.F90     \
$(CODEDIR)/m_netcdf_io_define.F90     \
$(CODEDIR)/m_netcdf_io_get_dimlen.F90 \
$(CODEDIR)/m_netcdf_io_handle_err.F90 \
$(CODEDIR)/m_netcdf_io_open.F90       \
$(CODEDIR)/m_netcdf_io_read.F90       \
$(CODEDIR)/m_netcdf_io_readattr.F90   \
$(CODEDIR)/m_netcdf_io_write.F90      \
$(CODEDIR)/TestNcdfUtilities.F90

# LaTeX etc. files
TEX1 := NcdfUtilities.tex
DVI1 := NcdfUtilities.dvi
PDF1 := NcdfUtilities.pdf
PS1  := NcdfUtilities.ps

srcdoc:
	rm -f $(TEX1)
	protex -sf $(SRC1) > $(TEX1)
	latex $(TEX1)
	latex $(TEX1)
	latex $(TEX1)
	dvipdf $(DVI1) $(PDF1)
	dvips $(DVI1) -o $(PS1)
	rm -f *.aux *.dvi *.log *.toc

#==============================================================================
# Make documentation for Makefiles
#==============================================================================

# Files for library build (code & objects)
SRC2 :=                        \
$(DOCDIR)/make.intro           \
$(ROOTDIR)/Makefile            \
$(CODEDIR)/Makefile            \
$(CODEDIR)/Makefile_header.mk  \
$(DOCDIR)/Makefile               

# LaTeX etc. files
TEX2 := NcdfUtilities_Makefiles.tex
DVI2 := NcdfUtilities_Makefiles.dvi
PDF2 := NcdfUtilities_Makefiles.pdf
PS2  := NcdfUtilities_Makefiles.ps

makedoc:
	rm -f $(TEX2)
	protex -fS $(SRC2) > $(TEX2)
	latex $(TEX2)
	latex $(TEX2)
	latex $(TEX2)
	dvipdf $(DVI2) $(PDF2)
	dvips $(DVI2) -o $(PS2)
	rm -f *.aux *.dvi *.log *.toc

#==============================================================================
# Make documentation for Perl scripts
#==============================================================================

# Files for library build (code & objects)
SRC3 =                       \
$(DOCDIR)/perl.intro         \
$(PERLDIR)/StrTrim.pm        \
$(PERLDIR)/ncCodeDef         \
$(PERLDIR)/ncCodeWrite       \
$(PERLDIR)/ncCodeRead        \
$(PERLDIR)/nc_definitions.rc

# LaTeX etc. files
TEX3 := NcdfUtilities_perl.tex
DVI3 := NcdfUtilities_perl.dvi
PDF3 := NcdfUtilities_perl.pdf
PS3  := NcdfUtilities_perl.ps

perldoc:
	rm -f $(TEX3)
	protex -sfS $(SRC3) > $(TEX3)
	latex $(TEX3)
	latex $(TEX3)
	latex $(TEX3)
	dvipdf $(DVI3) $(PDF3)
	dvips $(DVI3) -o $(PS3)
	rm -f *.aux *.dvi *.log *.toc

#==============================================================================
#  Help Screen
#==============================================================================

help:
	@echo 'Usage: make TARGET [ OPTIONAL-FLAGS ]'
	@echo ''
	@echo 'TARGET may be one of the following:'
	@echo 'doc    Builds documentation (*.ps, *.pdf) for NcdfUtilities'
	@echo 'clean  Removes all *.tex, *.ps, *.pdf files'
	@echo 'help   Displays this help screen'
#EOC