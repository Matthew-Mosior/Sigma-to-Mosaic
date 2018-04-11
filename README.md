# Sigma-to-Mosaic: File Format Converter from sigma_out.gvector.txt to mosaic.txt

## Introduction

File format converters are essential for a host of data conversion needs, from low level instructions to OS-level file format 
conversions.  This shell script is a format converter used to transform the output of [SigmaW](http://sigma.omicsbio.org/) (Strain-level Inference of Genomes from Metagenomic Analysis), **sigma_out.gvector.txt**, into the format accepted for the Mosaic Community 
Challenge: Strains ([MOSAIC Community Challenge: Strains](https://platform.mosaicbiome.com/challenges/1)).  

## Usage

This script is very easy to use, it takes **sigma_out.gvector.txt** as command line arguments.  Keep in mind, the Strains community
challenge has four datasets:<br/>
`Simulated_Low_Complexity`<br/>
`Simulated_Medium_Complexity`<br/>
`Simulated_High_Complexity`<br/>
`RealData (Mouse fecal samples)`<br/>
Each of these datasets contains four sets of paired-end sequencing reads, so in reality:<br/>
`Simulated_Low_Complexity`<br/>
`-sim_low_S1_PE1.fq`<br/>
`-sim_low_S1_PE2.fq`<br/>
`-sim_low_S2_PE1.fq`<br/>
`-sim_low_S2_PE2.fq`<br/>
`-sim_low_S3_PE1.fq`<br/>
`-sim_low_S3_PE2.fq`<br/>
`-sim_low_S4_PE1.fq`<br/>
`-sim_low_S4_PE2.fq`<br/>
`Simulated_Medium_Complexity`<br/>
`...`<br/>
`Simulated_High_Complexity`<br/>
`...`<br/>
`RealData (Mouse fecal samples)`<br/>
`...`<br/>
Since each set of paired-end sequencing reads (i.e. sim_low_S1_PE1.fq and sim_low_S1_PE2.fq) are run together to output a 
**single** sigma_out.gvector.txt file, you should be running the script **for each dataset** as follows:<br/>
``
