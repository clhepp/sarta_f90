# Makefile for SARTA.  Most of the work is done by make_sarta.

#
# Makefile for SARTA v1.08.  Most of the work is either by
# make_sarta_pclsam (sarta_cloudy)
# or by
# make_sarta (sarta clear only)
#
# Default is sarta_cloudy named m140_icebaumGHM_waterdrop_desertdust

#------------------------------------------------------------------------------------
# Builds clear
#------------------------------------------------------------------------------------

# ------------------------------------------------
# AIRS L1C Versions
# -----------------------------------------------
# this for testing new build with jpl tuning
airs_nn:
	make -f make_sarta_nn
	mv a.out ../bin/airs_nn

#	rm -f *.o incFTC.f
#	ln -s incFTC_airs_l1c_p2022jul2022_xnte_v02.f incFTC.f
#------------------------------------------------------------------------------------
# File management
#------------------------------------------------------------------------------------

clean:
	rm -f *.o
	rm -f *_genmod.f90 *_genmod.mod
