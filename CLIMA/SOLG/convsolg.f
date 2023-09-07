      SUBROUTINE CONVSOLG

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

C-kb This subroutine converts user-specified wavelengths defining the
C-kb start and end of one or more sub-spectra of the atmos/Clima full 
C-kb solar spectrum into indices and fractions that correspond to 
C-kb equivalent start and end positions in the 38 discrete intervals
C-kb (or "bins") of the full solar spectrum.

C-kb Inputs:
C-kb  GWAVLL(NSPECMX), GWAVLU(NSPECMX) - user-defined wavelengths (mu.m)
C-kb   WAVLL(NSOL),       WAVLU(NSOL)  - atmos spectrum wavelengths (mu.m)

C-kb Outputs:
C-kb  NGWAVLL(NSPECMX), NGWAVLU(NSPECMX)  - spectrum interval number
C-kb  GLSCALE(NSPECMX), GUSCALE(NSPECMX)  - position fraction

      PARAMETER (NSOL=38)
      PARAMETER (NSPECMX=10)
      PARAMETER (NTOPBOT=2)

      COMMON /SOLARBLK2/ NSPEC, FSOLI(NSOL),
     &                   FDNSOLI(NSOL,NTOPBOT), FUPSOLI(NSOL,NTOPBOT)

      COMMON /SOLARBLK3/ WAVLL(NSOL), WAVLU(NSOL)

      COMMON /SOLARBLK5/ GWAVLL(NSPECMX),  GWAVLU(NSPECMX),
     &                   NGWAVLL(NSPECMX), NGWAVLU(NSPECMX),
     &                   GLSCALE(NSPECMX), GUSCALE(NSPECMX)

      DO 210 ISPEC = 1, NSPEC  
         IF (  GWAVLL(ISPEC) .LT. GWAVLU(ISPEC)
     &   .AND. GWAVLL(ISPEC) .LT. WAVLU(NSOL)
     &   .AND. GWAVLU(ISPEC) .GT. WAVLU(1) ) THEN
            IF ( GWAVLL(ISPEC) .LT. WAVLL(1) ) THEN
               GWAVLL(ISPEC) = WAVLL(1)
            END IF  
            IF ( GWAVLU(ISPEC) .GT. WAVLU(NSOL) ) THEN
               GWAVLU(ISPEC) = WAVLU(NSOL)
            END IF  
            DO 220 I =1, NSOL
               IF (  GWAVLL(ISPEC) .GE. WAVLL(I)
     &         .AND. GWAVLL(ISPEC) .LT. WAVLU(I) ) THEN			
                  NGWAVLL(ISPEC) = I
                  GLSCALE(ISPEC) = ( WAVLU(I) - GWAVLL(ISPEC) )
     &                             / ( WAVLU(I) - WAVLL(I) )
               END IF	 
               IF (  GWAVLU(ISPEC) .GT. WAVLL(I)
     &         .AND. GWAVLU(ISPEC) .LE. WAVLU(I) ) THEN			
                   NGWAVLU(ISPEC) = I
                   GUSCALE(ISPEC) = ( GWAVLU(ISPEC) - WAVLL(I) )
     &                              / ( WAVLU(I) - WAVLL(I) )
               END IF	 
  220       CONTINUE
         ELSE
            NSPEC = 0
         END IF			  
  210 CONTINUE

      END
