FC=ifort
NCDF=/opt/software/occigen/libraries/netcdf/4.4.0_fortran-4.4.2/hdf5/1.8.17/intel/17.0/openmpi/intel/2.0.1
HDF=/opt/software/occigen/libraries/hdf5/1.8.17/intel/17.0/openmpi/intel/2.0.1

FFLAGS= -O3 -traceback -I$(NCDF)/include -L$(NCDF)/lib -lnetcdf -lnetcdff -L$(HDF)/lib -lhdf5_hl -lhdf5 -ldl -lm -lz
#FFLAGS= -O -I$(NCDF)/include -L$(NCDF)/lib -lnetcdf -lnetcdff -L$(HDF)/lib -lhdf5_hl -lhdf5 -ldl -lm -lz

mktmask: mktmask.f90
	$(FC) $(FFLAGS) mktmask.f90 -o mktmask -lnetcdf
repeat_bdy: repeat_bdy.f90
	$(FC) $(FFLAGS) repeat_bdy.f90 -o repeat_bdy -lnetcdf

