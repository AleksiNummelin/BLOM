      subroutine forfun_channel
c
c --- ------------------------------------------------------------------
c --- Specify forcing functions for channel
c --- ------------------------------------------------------------------
c
      use mod_xc
c
      implicit none
c
#include "common_forc.h"
c
      integer i,j,l,k
c
      do j=1,jj
        do l=1,isp(j)
        do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
          do k=1,12
            sstclm(i,j,k)=5.+15.*j/jj    
            sssclm(i,j,k)=35.+1.*j/jj
          enddo
        enddo
        enddo
        do l=1,isu(j)
        do i=max(1,ifu(j,l)),min(ii,ilu(j,l))
          ztx(i,j)=1.0e-2
        enddo
        enddo
        do l=1,isv(j)
        do i=max(1,ifv(j,l)),min(ii,ilv(j,l))
          mty(i,j)=0.
        enddo
        enddo
      enddo
c
c relaxation timescales
      trxday=5.
      srxday=5.
c
      return
      end
