#!/bin/ksh


# gridTS
for f in bdy_NATL60-GLORYS2_east_TS_*.nc ; do
   ncks -h -O --fix_rec_dmn time_counter $f f0.nc
   ncecat -h  f0.nc f1.nc
   ncks -h -O --fix_rec_dmn record f1.nc f2.nc
   ncpdq -h -a deptht,record f2.nc f3.nc
   ncpdq -h -a time_counter,deptht f3.nc f4.nc
   ncks -h --mk_rec_dmn time_counter f4.nc ${f}.ok
   ncrename -h -d record,yt -d y,xt  ${f}.ok
   rm f[1234]*.nc
done

# gridU
for f in bdy_NATL60-GLORYS2_east_U_*.nc ; do
   ncks -h -O --fix_rec_dmn time_counter $f f0.nc
   ncecat -h f0.nc f1.nc
   ncks -h -O --fix_rec_dmn record  f1.nc f2.nc
   ncpdq -h -a depthu,record f2.nc f3.nc
   ncpdq -h -a time_counter,depthu f3.nc f4.nc
   ncks -h --mk_rec_dmn time_counter f4.nc ${f}.ok
   ncrename -h -d record,yt -d y,xt  ${f}.ok
   rm f[1234]*.nc
done

# gridV
for f in bdy_NATL60-GLORYS2_east_V_*.nc ; do
   ncks -h -O --fix_rec_dmn time_counter $f f0.nc
   ncecat -h f0.nc f1.nc
   ncks -h -O --fix_rec_dmn  record f1.nc f2.nc
   ncpdq -h -a depthv,record f2.nc f3.nc
   ncpdq -h -a time_counter,depthv f3.nc f4.nc
   ncks -h --mk_rec_dmn time_counter f4.nc ${f}.ok
   ncrename -h -d record,yt -d y,xt  ${f}.ok
   rm f[01234]*.nc
done





exit

        :history = "Thu Apr 28 12:47:02 2016: ncrename -d record,yt -d y,xt coco.nc\n",
            "Thu Apr 28 12:46:08 2016: ncks --mk_rec_dmn time_counter cucu.nc coco.nc\n",
            "Thu Apr 28 12:45:26 2016: ncpdq -a time_counter,deptht tutu.nc cucu.nc\n",
            "Thu Apr 28 12:44:44 2016: ncpdq -a deptht,record titi.nc tutu.nc\n",
            "Thu Apr 28 12:41:01 2016: ncks -O --fix_rec_dmn record toto.nc titi.nc\n",
            "Thu Apr 28 12:40:32 2016: ncecat wtmp_no_unlim.nc toto.nc\n",
            "Thu Apr 28 10:24:08 2016: ncks -O --fix_rec_dmn time_counter bdy_NATL60-GLORYS2_east_TS_y2004m00.nc wtmp_no_unlim.nc\n",
