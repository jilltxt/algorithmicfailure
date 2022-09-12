# Algorithmic Failure: Code for Testing a Methodology for Using Algorithmic Mispreditions to Identify Interesting Cases for Qualitative Research

Author: Jill Walker Rettberg / jill.walker.rettberg@uib.no 

[![DOI](https://zenodo.org/badge/338047184.svg)](https://zenodo.org/badge/latestdoi/338047184)

## Description

This repository contains the code and data used in a paper that tests algorithmic failure, a methodology for using machine learning in qualitative research. The paper is forthcoming in the journal Big Data and Society, and can be cited thus:

Rettberg, Jill Walker. (2022) "Algorithmic failure as a humanities methodology: using machine learning’s mispredictions to identify rich cases for qualitative analysis in big datasets." Big Data & Society. Forthcoming.

This paper uses a simpler algorithm (kNN) and a different dataset to test the method, which was first described in

Munk, AK, Olesen, AG, & Jacomy, M (2022) The Thick Machine: Anthropological AI between explanation and explication. Big Data & Society, 9(1). DOI: 
10.1177/20539517211069891 

## How to run the code

The code is written in R using the Tidyverse and Class packages. I wrote and tested it using Rstudio 2022.07.1. 

You only need the following file to run the code:
- scripts/R_scripts_for_testing_algorithmic_failure.R

This will import two csv files that are also avilable in this repository:
- data/characters.csv
- data/situations.csv

## Dataset

The dataset used here is based on data collected in the database [Machine Vision in Art, Games and Narratives](https://machine-vision.no) as part of the [ERC-funded project Machine Vision in Everyday Life: Playful Interactions with Visual Technologies in Digital Art, Games, Narratives and Social Media](https://www.uib.no/en/machinevision/).

The dataset captures cultural attitudes towards machine vision technologies as they are expressed in art, games and narratives. It includes records of 500 creative works (including 77 digital games, 191 digital artworks and 236 movies, novels and other narratives) that use or represent machine vision technologies like facial recognition, deepfakes, and augmented reality. The dataset can be cited as 

Rettberg, Jill Walker; Kronman, Linda; Solberg, Ragnhild; Gunderson, Marianne; Bjørklund, Stein Magne; Stokkedal, Linn Heidi; de Seta, Gabriele; Jacob, Kurdin; Markham, Annette, 2022, "A Dataset Documenting Representations of Machine Vision Technologies in Artworks, Games and Narratives", https://doi.org/10.18710/2G0XKN, DataverseNO, V1

## Licence

The code is licenced under a CC-BY 4.0 licence. Please reuse or revise in any way that is useful to you. If you use it in an academic publication, please cite the paper:

Rettberg, Jill Walker. (2022) "Algorithmic failure as a humanities methodology: using machine learning’s mispredictions to identify rich cases for qualitative analysis in big datasets." Big Data & Society. Forthcoming.

This project has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation programme (grant agreement No 771800).