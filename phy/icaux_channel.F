      subroutine icaux_channel
c
c --- ------------------------------------------------------------------
c --- THIS IS AN EMPTY FUNCTION - COULD BE USED TO SETUP THE SEA ICE
c --- ------------------------------------------------------------------
c
      use mod_xc
      use mod_ben02, only: ntda
c
      implicit none
c
#include "common_blocks.h"
#include "common_forc.h"
c#include "common_seaice.h"
c
      ntda=0
      if (mnproc.eq.1) then
          write(lp,*) 'ntda setup done!'
      endif
c
      return
      end
