      SUBROUTINE READSOLG

C-kb This subroutine was created by Katy Browning (UCL) in August 2023
C-kb to extend the existing Clima calculation and output of solar fluxes
C-kb the total solar spectrum to any one or more user-specified
C-kb wavelength ranges within the solar spectrum. Additionally, the
C-kb option to calculate photosynthetic rates for any of the user-
C-kb specified spectral wavelength ranges has been introduced.

C-kb The totality of the code changes for this new functionality
C-kb comprises edits to the main Clima program and the following
C-kb existing subroutines:
C-kb READSOL, PICKSTAR, SOLAR, SOLARM, SOLAROX and SOLARMOX.
C-kb The following new subroutines have also been created:
C-kb READSOLG (called from the READSOL subroutine), and
C-kb CONVSOLG, CALCSOLG and WRITESOLG (called from the main program).

C-kb All code is Fortran77/90 compliant.


      PARAMETER (NSOL=38)
      PARAMETER (NSPECMX=10)
      PARAMETER (NTOPBOT=2)
      PARAMETER (NPCONDMX=10)

      CHARACTER*3 GNAME
	  
      COMMON /SOLARBLK2/ NSPEC, FSOLI(NSOL),
     &                   FDNSOLI(NSOL,NTOPBOT), FUPSOLI(NSOL,NTOPBOT)

      COMMON /SOLARBLK3/ WAVLL(NSOL), WAVLU(NSOL)

      COMMON /SOLARBLK4/ GNAME(NSPECMX)

      COMMON /SOLARBLK5/ GWAVLL(NSPECMX),  GWAVLU(NSPECMX),
     &                   NGWAVLL(NSPECMX), NGWAVLU(NSPECMX),
     &                   GLSCALE(NSPECMX), GUSCALE(NSPECMX)

      COMMON /SOLARBLK7/ NPITR, IPGSPEC(NSPECMX),
     &                   PALPHA, PBETA, PGAMMA, PTOPT, PTMAX,
     &                   NPCOND, PCOND(NPCONDMX), PRRATE(NPCONDMX),
     &                   PIRATE(NSPECMX,NPCONDMX),
     &                   PITRATE(NSPECMX,NPCONDMX)

      READ (4,*,IOSTAT=NERROR) NSPEC
      IF ( NERROR.EQ.0 .AND. NSPEC.GT.0 .AND. NSPEC.LE.NSPECMX ) THEN
         DO 110 ISPEC = 1, NSPEC
            READ (4,*) GNAME(ISPEC), GWAVLL(ISPEC), GWAVLU(ISPEC)
  110    CONTINUE
      ELSE
         NSPEC = 0         
      END IF
      READ (4,*,IOSTAT=NERROR) NPITR
      IF ( NERROR.EQ.0 .AND. NPITR.GT.0 .AND. NPITR.LE.NSPECMX ) THEN
         READ (4,*) (IPGSPEC(IPITR), IPITR = 1, NPITR)
         READ (4,*) PALPHA, PBETA, PGAMMA
         READ (4,*) PTOPT, PTMAX
         READ (4,*) NPCOND
         READ (4,*) (PCOND(IPCOND), IPCOND = 1, NPCOND)			
      ELSE
         NPITR = 0         
      END IF

      END
