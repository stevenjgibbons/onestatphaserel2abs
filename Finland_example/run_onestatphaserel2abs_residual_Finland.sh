#!/bin/sh
awk '{print $5, $6}' Finland_allcctimes.txt | sort | uniq > station_phase_only_Finland.txt
#---------------------------------------------------
outfile=Finland_residual1.txt
if test -r $outfile
then
  rm $outfile
fi
touch $outfile
while read line
do
  set $line
  station=$1
  phase=$2
  onestatphaserel2abs $station $phase Residual < Finland_allcctimes.txt > tmp.txt
  awk '$5 == STATION && $6 == PHASE' STATION=$station PHASE=$phase tmp.txt >> $outfile
done < station_phase_only_Finland.txt
awk '$8 > -0.025 && $8 < 0.025' $outfile > clean.txt
#---------------------------------------------------
outfile=Finland_residual2.txt
if test -r $outfile
then
  rm $outfile
fi
touch $outfile
while read line
do
  set $line
  station=$1
  phase=$2
  onestatphaserel2abs $station $phase Residual < clean.txt > tmp.txt
  awk '$5 == STATION && $6 == PHASE' STATION=$station PHASE=$phase tmp.txt >> $outfile
done < station_phase_only_Finland.txt
awk '$8 > -0.015 && $8 < 0.015' $outfile > clean.txt
#---------------------------------------------------
outfile=Finland_residual3.txt
if test -r $outfile
then
  rm $outfile
fi
touch $outfile
while read line
do
  set $line
  station=$1
  phase=$2
  onestatphaserel2abs $station $phase Residual < clean.txt > tmp.txt
  awk '$5 == STATION && $6 == PHASE' STATION=$station PHASE=$phase tmp.txt >> $outfile
done < station_phase_only_Finland.txt
awk '$8 > -0.010 && $8 < 0.010' $outfile > clean.txt
#---------------------------------------------------
outfile=Finland_clean_CC.txt
if test -r $outfile
then
  rm $outfile
fi
touch $outfile
while read line
do
  set $line
  station=$1
  phase=$2
  onestatphaserel2abs $station $phase Replace < clean.txt > tmp.txt
  awk '$5 == STATION && $6 == PHASE' STATION=$station PHASE=$phase tmp.txt >> $outfile
done < station_phase_only_Finland.txt
#---------------------------------------------------
outfile=Finland_clean_allCC.txt
if test -r $outfile
then
  rm $outfile
fi
touch $outfile
while read line
do
  set $line
  station=$1
  phase=$2
  onestatphaserel2abs $station $phase Allpairs < clean.txt > tmp.txt
  awk '$5 == STATION && $6 == PHASE' STATION=$station PHASE=$phase tmp.txt >> $outfile
done < station_phase_only_Finland.txt
