#!/bin/bash

clear

echo "PhotoChem/Clima Coupled Model Control File"
echo "Written by Katy Browning (UCL) 25/07/2023"
echo

###---------------- START of Section 0: Define directory paths and names --------------------------

folder_path1='OutputStorage'
folder_path2='PHOTOCHEM/OUTPUT'
folder_path3='CLIMA/IO'
folder_path4='PHOTOCHEM/INPUTFILES'
i_folder='INPUT'
op_folder='PHOTOCHEM_OUTPUT'
oc_folder='CLIMA_OUTPUT'

###---------------- END of Section 0 --------------------------

###---------------- START of Section 1: Choose run name and set up repositories for run files --------------------------

### Prompt for run name <run_name>
echo -n "Enter the model run name: "
   read run_name
echo
### Check that directory called <run_name> doesn't already exist 
if [ ! -d ./$folder_path1/$run_name ]
then
#  If directory doesn't exist, create directory called ./<folder_path1>/<run_name> plus three input/output sub-directories     
   mkdir -p ./$folder_path1/$run_name
   mkdir ./$folder_path1/$run_name/$i_folder
   mkdir ./$folder_path1/$run_name/$op_folder
   mkdir ./$folder_path1/$run_name/$oc_folder
   echo "A new directory and 3 sub-directories have been created to store saved files for this run."
else
#  If directory does exist, print warning message with abort option then ask if want to delete existing files in directory or keep
   echo "WARNING: A model run with this run name already exists."
   echo "Any existing output files for this run will be overwritten."   
   echo -n "Are you sure you want to continue (y/n)?: "
      read continue
   if [ "$continue" == "y" -o "$continue" == "Y" ]
   then
      echo
      echo "Continuing with model run."
	  echo
#	  echo -n "Do you want to delete any existing saved files for this run (y/n)? [RECOMMEND Y AT MOMENT]: "
#         read delete_existing_files
##     If choose delete, delete all files and sub-directories in directory then recreate the sub-directories
#      if [ "$delete_existing_files" == "y" -o "$delete_existing_files" == "Y" ]
#      then
         rm -rf ./$folder_path1/$run_name/*
         mkdir ./$folder_path1/$run_name/$i_folder
         mkdir ./$folder_path1/$run_name/$op_folder
         mkdir ./$folder_path1/$run_name/$oc_folder	  
#	     echo "Any existing saved files for this run have been deleted."
#      else	  
##     If choose not delete, if any of the required sub-directories don't exist (perhaps because they were previously named something different) create them 
#         if [ ! -d ./$folder_path1/$run_name/$i_folder ]
#         then
#            mkdir ./$folder_path1/$run_name/$i_folder
#   	     echo "WARNING: ${i_folder} sub-directory did not exist, so it has been created for this run"
#         fi
#         if [ ! -d ./$folder_path1/$run_name/$op_folder ]
#         then
#            mkdir ./$folder_path1/$run_name/$op_folder
#            echo "WARNING: ${op_folder} sub-directory did not exist, so it has been created for this run"
#         fi
#         if [ ! -d ./$folder_path1/$run_name/$oc_folder ]
#         then
#            mkdir ./$folder_path1/$run_name/$oc_folder
#            echo "WARNING: ${oc_folder} sub-directory did not exist, so it has been created for this run"
#         fi
#      fi
   else
      echo
	  echo "Run aborted."
	  echo
      exit 1
	  
   fi	  
fi

###---------------- END of Section 1 --------------------------

###---------------- START of Section 2: First run iteration using RunModels.sh --------------------------

# This variable counts the number of coupled run iterations completed
declare -i count=0

echo
#echo -n "Would you like to run the RunModels.sh script (y/n)?: "
#   read run
#echo
#if [ "$run" == "y" -o "$run" == "Y" ]
#then
   ./RunModels.sh
   let count=count+1
   echo "RunModels.sh has run"
  
###---------------- START of Section 2.1: Copy initial input files and corresponding output files to ./<folder_path1>/<run_name>/ directory ------------------------

# INPUT FILES: <i_folder> sub-directory
cp "$folder_path3/input_clima.dat" "$folder_path1/$run_name/$i_folder/${run_name}_${count}_input_clima.dat" && echo "Copied input_clima.dat to ${run_name}/${i_folder} sub-directory"
cp "$folder_path4/input_photchem.dat" "$folder_path1/$run_name/$i_folder/${run_name}_${count}_input_photchem.dat" && echo "Copied input_photchem.dat to ${run_name}/${i_folder} sub-directory"
cp "$folder_path4/parameters.inc" "$folder_path1/$run_name/$i_folder/${run_name}_${count}_parameters.inc" && echo "Copied parameters.inc to ${run_name}/${i_folder} sub-directory"
cp "$folder_path4/params.dat" "$folder_path1/$run_name/$i_folder/${run_name}_${count}_params.dat" && echo "Copied params.dat to ${run_name}/${i_folder} sub-directory"
cp "$folder_path4/PLANET.dat" "$folder_path1/$run_name/$i_folder/${run_name}_${count}_PLANET.dat" && echo "Copied PLANET.dat to ${run_name}/${i_folder} sub-directory"
cp "$folder_path4/reactions.rx" "$folder_path1/$run_name/$i_folder/${run_name}_${count}_reactions.rx" && echo "Copied reactions.rx to ${run_name}/${i_folder} sub-directory"
cp "$folder_path4/species.dat" "$folder_path1/$run_name/$i_folder/${run_name}_${count}_species.dat" && echo "Copied species.dat to ${run_name}/${i_folder} sub-directory"
# PHOTOCHEM OUTPUT FILES: <op_folder> sub-directory
cp "$folder_path2/out.out" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.out" && echo "Copied out.out to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/PTZ_mixingratios_out.dist" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_PTZ_mixingratios_out.dist" && echo "Copied PTZ_mixingratios_out.dist to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/hcaer.out" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_hcaer.out" && echo "Copied hcaer.out to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/hcaer2.out" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_hcaer2.out" && echo "Copied hcaer2.out to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/int.rates.out" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_int.rates.out" && echo "Copied int.rates.out to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/nsteps.out" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_nsteps.out" && echo "Copied nsteps.out to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.aersol" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.aersol" && echo "Copied out.aersol to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.chem" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.chem" && echo "Copied out.chem to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.converge" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.converge" && echo "Copied out.converge to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.cl" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.cl" && echo "Copied out.cl to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.densities" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.densities" && echo "Copied out.densities to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.dist" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.dist" && echo "Copied out.dist to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.finalden" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.finalden" && echo "Copied out.finalden to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.error" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.error" && echo "Copied out.error to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.flow" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.flow" && echo "Copied out.flow to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.flux" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.flux" && echo "Copied out.flux to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.gridw" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.gridw" && echo "Copied out.gridw to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.gridz" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.gridz" && echo "Copied out.gridz to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.NOprates" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.NOprates" && echo "Copied out.NOprates to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.O2prates" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.O2prates" && echo "Copied out.O2prates to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.od" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.od" && echo "Copied out.od to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.params" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.params" && echo "Copied out.params to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.prod" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.prod" && echo "Copied out.prod to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.rad" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.rad" && echo "Copied out.rad to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.raingc" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.raingc" && echo "Copied out.raingc to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.rates" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.rates" && echo "Copied out.rates to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.redox" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.redox" && echo "Copied out.redox to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.rp" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.rp" && echo "Copied out.rp to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.so2" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.so2" && echo "Copied out.so2 to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.tau" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.tau" && echo "Copied out.tau to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.strctr" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.strctr" && echo "Copied out.strctr to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.tim" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.tim" && echo "Copied out.tim to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.time" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.time" && echo "Copied out.time to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.terse" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.terse" && echo "Copied out.terse to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.tridag" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.tridag" && echo "Copied out.tridag to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.trs" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.trs" && echo "Copied out.trs to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.xsec" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.xsec" && echo "Copied out.xsec to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/profile.pt" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_profile.pt" && echo "Copied profile.pt to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/PTZ_out.flux" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_PTZ_out.flux" && echo "Copied PTZ_out.flux to ${run_name}/${op_folder} sub-directory"
# CLIMA OUTPUT FILES: <oc_folder> sub-directory
cp "$folder_path3/clima_allout.tab" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_clima_allout.tab" && echo "Copied clima_allout.tab to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/clima_last.tab" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_clima_last.tab" && echo "Copied clima_last.tab to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/FTIR.dat" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_FTIR.dat" && echo "Copied FTIR.dat to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/FTSO.dat" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_FTSO.dat" && echo "Copied FTSO.dat to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/hcaer.out" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_hcaer.out" && echo "Copied hcaer.out to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/hcaer_nohaze.out" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_hcaer_nohaze.out" && echo "Copied hcaer_nohaze.out to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/IR_wavelength_grid.tab" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_IR_wavelength_grid.tab" && echo "Copied IR_wavelength_grid.tab to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/mixing_ratios.dat" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_mixing_ratios.dat" && echo "Copied mixing_ratios.dat to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/SolarHeating.tab" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_SolarHeating.tab" && echo "Copied SolarHeating.tab to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/TempIn.dat" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_TempIn.dat" && echo "Copied TempIn.dat to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/TempOut.dat" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_TempOut.dat" && echo "Copied TempOut.dat to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/weight_factors.txt" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_weight_factors.txt" && echo "Copied weight_factors.txt to to ${run_name}/${oc_folder} sub-directory"

###---------------- END of Section 2.1 -------------------------- 

#else
#   echo "RunModels.sh has not been run"
#fi

###---------------- END of Section 2 -------------------------- 

###---------------- START of Section 3: Additional run iterations using photo.run and clima.run --------------------------

echo
echo -n "Would you like to run PhotoChem and Clima for additional iterations (y/n)?: "
   read rerun
echo
if [ "$rerun" == "y" -o "$rerun" == "Y" ]
then
#   echo
#   echo -n "Would you like to set ICOUPLE = 1 in the PhotoChem input file (y/n)?: "
#      read couple
#   if [ "$couple" == "y" -o "$couple" == "Y" ]
#   then
      /bin/sed -i 's|ICOUPLE=   0|ICOUPLE=   1|' ./$folder_path4/input_photchem.dat
	  echo "ICOUPLE parameter set to 1 in PhotoChem input data"
#   fi

## Option to change NSTEPS value from 400 to 200 in input_clima.dat (doesn't do anything if value not 400)
##   (would prefer to reset to 200 before first iteration but that means modifying RunModels.sh, consider doing this in a later modification) 
#   echo
#   echo -n "Would you like to set NSTEPS = 200 in the Clima input file if you haven't already changed it manually (y/n)?: "
#      read steps
#   if [ "$steps" == "y" -o "$steps" == "Y" ]
#   then
#      /bin/sed -i 's|NSTEPS=    400|NSTEPS=    200|' ./$folder_path3/input_clima.dat
#   fi
   
# loop to allow repeat of coupled run a specified number of times   
   echo
   echo -n "How many additional iterations would you like to run (min 1, max 8)?: "
      read iters
   echo	  
   if [ $iters -le 0 ]
   then
      echo "Chosen value < 1 so PhotoChem and Clima have not been run again"
   fi	  
   while [ $count -le $iters ]
   do	  
      ./Photo.run
      ./Clima.run
      let count=count+1
      echo "Coupled run iteration #${count} of PhotoChem/Clima completed."

###---------------- START of Section 3.1: Copy new output files to ./<folder_path1>/<run_name>/ directory ------------------------

# PHOTOCHEM OUTPUT FILES: <op_folder> sub-directory
cp "$folder_path2/out.out" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.out" && echo "Copied out.out to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/PTZ_mixingratios_out.dist" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_PTZ_mixingratios_out.dist" && echo "Copied PTZ_mixingratios_out.dist to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/hcaer.out" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_hcaer.out" && echo "Copied hcaer.out to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/hcaer2.out" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_hcaer2.out" && echo "Copied hcaer2.out to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/int.rates.out" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_int.rates.out" && echo "Copied int.rates.out to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/nsteps.out" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_nsteps.out" && echo "Copied nsteps.out to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.aersol" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.aersol" && echo "Copied out.aersol to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.chem" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.chem" && echo "Copied out.chem to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.converge" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.converge" && echo "Copied out.converge to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.cl" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.cl" && echo "Copied out.cl to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.densities" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.densities" && echo "Copied out.densities to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.dist" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.dist" && echo "Copied out.dist to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.finalden" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.finalden" && echo "Copied out.finalden to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.error" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.error" && echo "Copied out.error to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.flow" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.flow" && echo "Copied out.flow to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.flux" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.flux" && echo "Copied out.flux to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.gridw" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.gridw" && echo "Copied out.gridw to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.gridz" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.gridz" && echo "Copied out.gridz to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.NOprates" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.NOprates" && echo "Copied out.NOprates to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.O2prates" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.O2prates" && echo "Copied out.O2prates to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.od" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.od" && echo "Copied out.od to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.params" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.params" && echo "Copied out.params to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.prod" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.prod" && echo "Copied out.prod to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.rad" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.rad" && echo "Copied out.rad to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.raingc" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.raingc" && echo "Copied out.raingc to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.rates" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.rates" && echo "Copied out.rates to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.redox" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.redox" && echo "Copied out.redox to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.rp" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.rp" && echo "Copied out.rp to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.so2" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.so2" && echo "Copied out.so2 to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.tau" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.tau" && echo "Copied out.tau to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.strctr" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.strctr" && echo "Copied out.strctr to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.tim" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.tim" && echo "Copied out.tim to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.time" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.time" && echo "Copied out.time to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.terse" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.terse" && echo "Copied out.terse to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.tridag" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.tridag" && echo "Copied out.tridag to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.trs" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.trs" && echo "Copied out.trs to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/out.xsec" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_out.xsec" && echo "Copied out.xsec to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/profile.pt" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_profile.pt" && echo "Copied profile.pt to ${run_name}/${op_folder} sub-directory"
cp "$folder_path2/PTZ_out.flux" "$folder_path1/$run_name/$op_folder/${run_name}_${count}_PTZ_out.flux" && echo "Copied PTZ_out.flux to ${run_name}/${op_folder} sub-directory"
# CLIMA OUTPUT FILES: <oc_folder> sub-directory
cp "$folder_path3/clima_allout.tab" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_clima_allout.tab" && echo "Copied clima_allout.tab to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/clima_last.tab" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_clima_last.tab" && echo "Copied clima_last.tab to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/FTIR.dat" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_FTIR.dat" && echo "Copied FTIR.dat to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/FTSO.dat" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_FTSO.dat" && echo "Copied FTSO.dat to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/hcaer.out" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_hcaer.out" && echo "Copied hcaer.out to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/hcaer_nohaze.out" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_hcaer_nohaze.out" && echo "Copied hcaer_nohaze.out to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/IR_wavelength_grid.tab" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_IR_wavelength_grid.tab" && echo "Copied IR_wavelength_grid.tab to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/mixing_ratios.dat" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_mixing_ratios.dat" && echo "Copied mixing_ratios.dat to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/SolarHeating.tab" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_SolarHeating.tab" && echo "Copied SolarHeating.tab to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/TempIn.dat" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_TempIn.dat" && echo "Copied TempIn.dat to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/TempOut.dat" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_TempOut.dat" && echo "Copied TempOut.dat to to ${run_name}/${oc_folder} sub-directory"
cp "$folder_path3/weight_factors.txt" "$folder_path1/$run_name/$oc_folder/${run_name}_${count}_weight_factors.txt" && echo "Copied weight_factors.txt to to ${run_name}/${oc_folder} sub-directory"

###---------------- END of Section 3.1 --------------------------

   done
else
   echo "PhotoChem and Clima have not been run again"
fi

###---------------- END of Section 3 -------------------------- 

###---------------- START of Section 4: Reset ICOUPLE parameter value to 0 in the PHOTOCHEM input data and end script ------------------------

echo
#echo -n "Would you like to set ICOUPLE = 0 in the PHOTOCHEM input file (y/n)?: "
#   read couple
#if [ "$couple" == "y" -o "$couple" == "Y" ]
#then
   /bin/sed -i 's|ICOUPLE=   1|ICOUPLE=   0|' ./$folder_path4/input_photchem.dat
   echo "ICOUPLE parameter reset back to 0 in PhotoChem input data"
#fi

echo
echo "Script completed."

###---------------- END of Section 4 -------------------------- 
