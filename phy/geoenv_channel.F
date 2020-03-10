      subroutine geoenv_channel
c
c --- ------------------------------------------------------------------
c --- Define bathymetry, grid specification and Coriolis parameter for
c --- test case [cm s]
c --- ------------------------------------------------------------------
c
      use mod_xc
c
      implicit none
c
#include "common_blocks.h"
#include "common_geo.h"
c
      intrinsic random_seed, random_number,atan,tanh

      integer i,j,l
      real :: r
c      real, parameter :: PI0=4.*atan(1.0)
      real, parameter :: cwidth=75.E5  !slope width
      real, parameter :: swidth=175.E5 !slope mid-point distance 
c
c
      call random_seed()

c --- initialize depth to 0 everywhere     
      depths=0.
c
c --- Number of wet points (southern and northern most rows are land)
      nwp=jtdm*itdm-2.*itdm !jj*ii-2.*ii
c --- the latitudes need to be set, but actually the values don't matter it seems      
      qlon=0.
      qlat=0.
      plon=0.
      plat=0.
      ulon=0.
      ulat=0.
      vlon=0.
      vlat=0.
      qclon=0.
      qclat=0.
      pclon=0.
      pclat=0.
      uclon=0.
      uclat=0.
      vclon=0.
      vclat=0.
c --- these are the grid dimensions (in cm!)
      scqx=2.e5
      scqy=2.e5
      scpx=2.e5
      scpy=2.e5
      scux=2.e5
      scuy=2.e5
      scvx=2.e5
      scvy=2.e5
c --- square of the grid dimensions
      scq2=scqx*scqy
      scp2=scpx*scpy
      scu2=scux*scuy
      scv2=scvx*scvy
c --- here we just set a f-plane (beta=0) 
      angle=0.
      corioq=1.e-4
      coriop=1.e-4
      betafp=0.
      cosang=1.
      sinang=0.
c
c --- this is now a tanh slope that will have the same shape
c --- independent of the grid size - i.e. depends on grid
c --- dimensions
c --- 
c$OMP PARALLEL DO PRIVATE(i,r)
      do j=1,jj
        if (j0+j.gt.1) then
        if (j0+j.lt.jtdm) then
          do i=1,ii
             call random_number(r)
             if ((scpy(i,j)*(j0+j)).lt.(swidth+cwidth)) then
               depths(i,j)=
     .         250.+10.*r+1.e3*(1+tanh(pi*scpy(i,j)*(j0+j)/
     .         cwidth-(swidth/cwidth)*pi))
             elseif ((jtdm-(j0+j))*scpy(i,j).lt.(swidth+cwidth)) then
               depths(i,j)=
     .         250.+10.*r+1.e3*(1+tanh(pi*scpy(i,j)*(jtdm-(j0+j))/
     .         cwidth-(swidth/cwidth)*pi))
             else
               depths(i,j)=250.+2.e3+10.*r
             endif
          enddo
        endif
        endif
      enddo
c$OMP END PARALLEL DO

      return
      end
