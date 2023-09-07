      SUBROUTINE CALCSOLG(T)

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

C-kb This subroutine converts

      PARAMETER (ND=101)
      PARAMETER (NSOL=38)
      PARAMETER (NSPECMX=10)
      PARAMETER (NTOPBOT=2)
      PARAMETER (NPCONDMX=10)

      PARAMETER (HCCONST = 1.98646E-25)                                 ! Product of Plank's constant (h) and speed of light (c), units J.m
      PARAMETER (AVOCONST = 6.02214E23)                                 ! Avogadro's constant, number of photons in a mole
	  
      DIMENSION T(ND)
	  
      COMMON /SOLARBLK2/ NSPEC, FSOLI(NSOL),
     &                   FDNSOLI(NSOL,NTOPBOT), FUPSOLI(NSOL,NTOPBOT)

      COMMON /SOLARBLK3/ WAVLL(NSOL), WAVLU(NSOL)

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

      F2FMCONV =  1.0E-7 * 1.0E4                                        ! conversion factor energy flux erg/cm2/s to W/m2 (J/m2/s)   
      F2PMCONV =  F2FMCONV / AVOCONST                                   ! conversion factor energy flux erg/cm2/s to photon flux mol/m2/s
      F2PCONV =   F2PMCONV * 1.0E6                                      ! conversion factor energy flux erg/cm2/s to photon flux mu.mol/m2/s

      DO 310 I = 1, NSOL
         WAVLD = WAVLU(I) - WAVLL(I)                                    ! wavelength increment (mu.m)   
         FSODI(I) = F2FMCONV * FSOLI(I) / WAVLD                         ! normal energy flux density (W/m2/mu.m)
  310 CONTINUE

      DO 320 ISPEC = 1, NSPEC
        DO 330 JJ = 1, NTOPBOT
          IF ( NGWAVLL(ISPEC) .EQ. NGWAVLU(ISPEC) ) THEN
            IF (JJ.EQ.1) FSOLG(ISPEC) = FSOLI(NGWAVLL(ISPEC))
     &                          *(GLSCALE(ISPEC)+GUSCALE(ISPEC)-1)
            FDNSOLG(ISPEC,JJ) = FDNSOLI(NGWAVLL(ISPEC),JJ)
     &                          *(GLSCALE(ISPEC)+GUSCALE(ISPEC)-1)
            FUPSOLG(ISPEC,JJ) = FUPSOLI(NGWAVLL(ISPEC),JJ)
     &                          *(GLSCALE(ISPEC)+GUSCALE(ISPEC)-1)
            PDNSOLG(ISPEC,JJ) = F2PCONV*FDNSOLG(ISPEC,JJ)*1E-6
     &                          *(GWAVLL(ISPEC)+GWAVLU(ISPEC)) 
     &                          /(2.0*HCCONST) 
          ELSE
            IF (JJ.EQ.1) FSOLG(ISPEC) = FSOLI(NGWAVLL(ISPEC))
     &                                 *GLSCALE(ISPEC)
     &                                + FSOLI(NGWAVLU(ISPEC))
     &                                 *GUSCALE(ISPEC)
            FDNSOLG(ISPEC,JJ) = FDNSOLI(NGWAVLL(ISPEC),JJ)
     &                                 *GLSCALE(ISPEC)
     &                        + FDNSOLI(NGWAVLU(ISPEC),JJ)
     &                                 *GUSCALE(ISPEC)
            FUPSOLG(ISPEC,JJ) = FUPSOLI(NGWAVLL(ISPEC),JJ)
     &                                 *GLSCALE(ISPEC)
     &                        + FUPSOLI(NGWAVLU(ISPEC),JJ)
     &                                 *GUSCALE(ISPEC)
            PDNSOLG(ISPEC,JJ) = F2PCONV*FDNSOLI(NGWAVLL(ISPEC),JJ)
     &                          *GLSCALE(ISPEC)*1E-6
     &                          *(GWAVLL(ISPEC)+WAVLU(NGWAVLL(ISPEC))) 
     &                          /(2.0*HCCONST) 
     &                        + F2PCONV*FDNSOLI(NGWAVLU(ISPEC),JJ)
     &                          *GUSCALE(ISPEC)*1E-6
     &                          *(WAVLL(NGWAVLU(ISPEC))+GWAVLU(ISPEC)) 
     &                          /(2.0*HCCONST) 
          END IF       
          DO 340 I = 1, NSOL
            WAVLM = 0.5 * (WAVLL(I) + WAVLU(I)) * 1.0E-6                ! effective bin wavelength (m)   
            PDNSOLI(I,JJ) = F2PCONV * FDNSOLI(I,JJ) * WAVLM / HCCONST   ! bin photon flux down (mu.mol/m2/s)
            IF (JJ.EQ.2) PATTENI(I) = PDNSOLI(I,2) / PDNSOLI(I,1)
            WAVLD = WAVLU(I) - WAVLL(I)                                 ! bin wavelength interval (mu.m)
            PDNSODI(I,JJ) = PDNSOLI(I,JJ) / WAVLD                       ! bin photon flux density down (mu.mol/m2/s/mu.m = mol/m2/s/m)
            IF ( I.GT.NGWAVLL(ISPEC) .AND. I.LT.NGWAVLU(ISPEC) ) THEN
              IF (JJ.EQ.1) FSOLG(ISPEC) = FSOLG(ISPEC) + FSOLI(I)
              FDNSOLG(ISPEC,JJ) = FDNSOLG(ISPEC,JJ) + FDNSOLI(I,JJ)     ! group energy flux down (erg/cm2/s)
              FUPSOLG(ISPEC,JJ) = FUPSOLG(ISPEC,JJ) + FUPSOLI(I,JJ)     ! group energy flux up (erg/cm2/s)
              PDNSOLG(ISPEC,JJ) = PDNSOLG(ISPEC,JJ) + PDNSOLI(I,JJ)     ! group photon flux down (mu.mol/m2/s)
            END IF
  340     CONTINUE
          PDNSODG(ISPEC,JJ) = PDNSOLG(ISPEC,JJ)                         ! group photon flux density down (mu.mol/m2/s/mu.m = mol/m2/s/m)
     &                          / (GWAVLU(ISPEC)-GWAVLL(ISPEC))
  330   CONTINUE
        FSODG(ISPEC) = F2FMCONV * FSOLI(ISPEC)                          ! group normal energy flux density (W/m2/mu.m)
     &                          / (GWAVLU(ISPEC)-GWAVLL(ISPEC))
        PATTENG(ISPEC) = PDNSOLG(ISPEC,2) / PDNSOLG(ISPEC,1)            ! group photon attenuation top of atmosphere to planet surface (fraction) 
  320 CONTINUE

C-kb  Determination of photosynthetic rate

      IF (NPITR .NE. 0) THEN
        PFTEMP = ((PTMAX-T(101))/(PTMAX-PTOPT))
     &             *((T(101)/PTOPT)**(PTOPT/(PTMAX-PTOPT)))
        DO 350 IPITR = 1, NPITR 
          DO 360 IPCOND = 1, NPCOND
            PRRATE(IPCOND) = (PCOND(IPCOND)/(PCOND(IPCOND)+1))
     &                             / (PBETA+(4.0*PALPHA*PGAMMA)**0.5)
            PIRATE(IPITR,IPCOND) = 1.0 /
     &                (PALPHA * PDNSOLG(IPGSPEC(IPITR),2)
     &                 + PBETA + PGAMMA / PDNSOLG(IPGSPEC(IPITR),2))
     &            - PRRATE(IPCOND)
            PITRATE(IPITR,IPCOND) = PFTEMP * PIRATE(IPITR,IPCOND)
  360     CONTINUE
  350   CONTINUE  
      END IF 

      END
