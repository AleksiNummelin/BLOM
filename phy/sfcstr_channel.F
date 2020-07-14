      subroutine sfcstr_channel(m,n,mm,nn,k1m,k1n)
c
c --- ------------------------------------------------------------------
c --- compute the surface stress - sea ice is commented out for now
c --- ------------------------------------------------------------------
c
      use mod_xc
c
      implicit none
c
      integer m,n,mm,nn,k1m,k1n
c
#include "common_blocks.h"
#include "common_forc.h"
c
      integer i,j,l
c
c$OMP PARALLEL DO PRIVATE(l,i)
      do j=1,jj
c
      do l=1,isu(j)
      do i=max(1,ifu(j,l)),min(ii,ilu(j,l))
        taux(i,j)=10.*ztx(i,j)
      enddo
      enddo
c
      do l=1,isv(j)
      do i=max(1,ifv(j,l)),min(ii,ilv(j,l))
        tauy(i,j)=10.*mty(i,j)
      enddo
      enddo
      enddo
c$OMP END PARALLEL DO
c
      if (csdiag) then
        if (mnproc.eq.1) then
          write (lp,*) 'sfcstr:'
        endif
        call chksummsk(taux,iu,1,'taux')
        call chksummsk(tauy,iv,1,'tauy')
      endif
c
      return
      end
