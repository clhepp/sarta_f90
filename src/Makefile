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
	rm -f *.o incFTC.f90
	ln -s incFTC_airs_2834.f90 incFTC.f90
	make -f make_sarta_nn
	mv a.out ../bin/airs_nn


# IASI
iasi_nn:
	rm -f *.o incFTC.f90
	ln -sf incFTC_iasi_p2022jul22_dev.f90 incFTC.f90
	make -f make_sarta_nn
	mv a.out ../bin/iasi_nn

#------------------------------------------------------------------------------------
# File management
#------------------------------------------------------------------------------------

clean:
	rm -f *.o
	rm -f *_genmod.f90 *_genmod.mod
