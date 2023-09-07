      SUBROUTINE WRITESOLG

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

C-kb This subroutine writes fluxes and photosynthetic rates calculated 
C-kb in CALCSOLG to pre-existing output file clima_allout.tab.

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

      COMMON /SOLARBLK6/ FSODI(NSOL), FSOLG(NSPECMX),FSODG(NSPECMX),
     &                   FDNSOLG(NSPECMX,NTOPBOT),
     &                   FUPSOLG(NSPECMX,NTOPBOT),
     &                   PDNSOLI(NSOL,NTOPBOT), PATTENI(NSOL),
     &                   PDNSOLG(NSPECMX,NTOPBOT), PATTENG(NSPECMX),
     &                   PDNSODI(NSOL,NTOPBOT),PDNSODG(NSPECMX,NTOPBOT),
     &                   NPMAXDI(NSOL),NPMAXDG(NSPECMX) 
 
      COMMON /SOLARBLK7/ NPITR, IPGSPEC(NSPECMX),
     &                   PALPHA, PBETA, PGAMMA, PTOPT, PTMAX,
     &                   NPCOND, PCOND(NPCONDMX), PRRATE(NPCONDMX),
     &                   PIRATE(NSPECMX,NPCONDMX),
     &                   PITRATE(NSPECMX,NPCONDMX)

      WRITE (98,405)
      WRITE (98,415)  		   
      DO 410 I = 1, NSOL
         WRITE (98,425) I, WAVLL(I),     WAVLU(I),
     &                     FSOLI(I),     FSODI(I),
     &                     FDNSOLI(I,1), FUPSOLI(I,1),
     &                     FDNSOLI(I,2), FUPSOLI(I,2),
     &                     PDNSOLI(I,1), PDNSOLI(I,2),
     &                     PATTENI(I),   PDNSODI(I,2) 
  410 CONTINUE
      WRITE (98,*)
      DO 420 ISPEC = 1, NSPEC
         WRITE (98,435) GNAME(ISPEC),
     &                  GWAVLL(ISPEC),    GWAVLU(ISPEC),
     &                  FSOLG(ISPEC),     FSODG(ISPEC),
     &                  FDNSOLG(ISPEC,1), FUPSOLG(ISPEC,1),
     &                  FDNSOLG(ISPEC,2), FUPSOLG(ISPEC,2),
     &                  PDNSOLG(ISPEC,1), PDNSOLG(ISPEC,2),
     &                  PATTENG(ISPEC),   PDNSODG(ISPEC,2)
  420 CONTINUE             

  405 FORMAT( /2X,"I",1X,"   WAVLL  ",1X,"   WAVLU  ",
     &                2X,"   FSOL   ",2X,"   FSOD   ",
     &                2X,"FDNSOL(1) ",2X,"FUPSOL(1) ",
     &                2X,"FDNSOL(2) ",2X,"FUPSOL(2) ",
     &                2X,"PDNSOL(1) ",2X,"PDNSOL(2) ",
     &                2X,"  PATTEN  ",2X,"PDNSOD(2) ",
     &                2X,"          " )
  415 FORMAT(  2X,"#",1X,"   mu.m   ",1X,"   mu.m   ",
     &                2X,"erg/cm2/s ",2X,"W/m2/mu.m ",
     &                2X,"erg/cm2/s ",2X,"erg/cm2/s ",
     &                2X,"erg/cm2/s ",2X,"erg/cm2/s ",
     &                2X,"mumol/m2/s",2X,"mumol/m2/s",
     &                2X," fraction ",2X,"mol/m2/s/m",
     &                2X,"          " )
  425 FORMAT (I3,2(1X,F10.7),8(1X,1PE11.4),(1X,0PF11.8),2(1X,1PE11.4))
  435 FORMAT (A3,2(1X,F10.7),8(1X,1PE11.4),(1X,0PF11.8),2(1X,1PE11.4))

      IF (NPITR .NE. 0) THEN
         DO 430 IPITR = 1, NPITR
            WRITE(98,*)
            WRITE(98,445) GNAME(IPGSPEC(IPITR))
            WRITE(98,455)
            WRITE(98,465)
            DO 440 IPCOND = 1, NPCOND
			   WRITE(98,475) PCOND(IPCOND), PIRATE(IPITR,IPCOND),
     &                                      PITRATE(IPITR,IPCOND)
  440       CONTINUE
  430    CONTINUE
      END IF
      WRITE (98,*)
      WRITE(98,485) PALPHA, PBETA, PGAMMA
      WRITE(98,495) PTOPT, PTMAX

  445 FORMAT ( 1X,"PHOTOSYNTHETIC RATE ('",A3,"' Spectrum)")
  455 FORMAT (/9X," PCOND  ",9X,"    PIRATE     ",9X,"    PITRATE    ")
  465 FORMAT ( 9X,"fraction",9X,"mumol_O2/mg2/hr",9X,"mumol_O2/mg2/hr")         
  475 FORMAT ( 10X,  F6.4  ,11X,    1PE11.4     ,14X,      E11.4      )
  485 FORMAT ( "Constants: PALPHA = ",1PE11.4,", PBETA = ",E11.4,
     &                  ", PGAMMA = ",E11.4 )
  495 FORMAT ( "            PTOPT = ",F7.2," K  , PTMAX = ",F7.2," K")

      END
