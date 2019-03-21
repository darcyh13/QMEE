
Data is obtained from set of experimental crosses and data collected by Andrew Victory while an undergraduate at MSU in the Dworkin lab.

readme.txt for the genetic background effects of mutations influencing vein development.

 This serves as a summary of information associated with the file
GeneticBackgroundVeinMutantInteractionsFinal.csv

column 1: background.  wild-type genetic background in which the crosses were made (2 backgrounds Ore & Sam).

column 2: gene1.    Mutations at first locus (in addition to rhomboid/rho).
column 3: gene2.    Mutations at Second locus (in addition to rhomboid/rho).
column 4: rhomboid.  Whether these individuals are homozygous for the rhomboid mutation (causing a shortening of the longitudinal veins).
column 5: rep.   replicate blocks used in the experiment (since it was large and could not all be done at one time).
column 6. sex.   The sex of the fly being measured.
column 7. individual. Just an id for the fly.
column 8:  L2 . Length of the L2 longitudinal vein.
column 9:  L3 . Length of the L3 longitudinal vein.
column 10: L4 . Length of the L4 longitudinal vein.
column 11: L5 . Length of the L5 longitudinal vein.
column 12: WL . Length of wing (as a covariate to control for overall wing size).


codes for backgrounds
Ore: Oregon-R wild-type (marked with a w{-} eye colour marker)
Sam: Samarkand wild-type (marked with a w{-} eye colour marker)

codes for genes
wt: wild-type
rho: rhomboid (rho[KG07115]) 
Egfr: Epidermal Growth Factor Receptor (Egfr[k05115])
Bs: Blistered (bs[k07909] also called DSRF - Drosophila serum response factor)
S: Star (S[k09530])
Spi: spitz (spi[s3547])
Aos:  argos (aos[W11])


Other notes from the experiment:
Ore; Bs/S; rho is empty because it was lethal in that combination. 

The baseline strains (SAM w ; SAM; SAM rho & Ore w; ORE; ORE rho ) with rep1 is the "control" for those with an additional single 2nd Chr mutant. The baseline strains labeled rep1b (or rep2b) are for double mutants on the 2nd Chromosome. 
Sam baselines reps 3 and 4 are for comparison to any Oregon reps 3 or 4, as it was crossed in march.

Rep1/Rep2: Original double and triple mutants, which now only includes SAM backgrounds (Crossed in October).
Rep1b/Rep2b: Original Sam w; Sam; rho- from the set of triple mutants (done about 3 weeks after double mutants).

Rep3/4: All Oregon double and triple mutants, and one set (in each rep) of Sam w; Sam; Sam for comparison to previous crosses (crossed in January).
Rep3b/Rep4/b: Ore w; Ore; rho- from the set of triple mutants (although done at the same time as rep 3/4 single mutants, I did this for equal sets with Sam)

Scientific questions: 

1. Distinguish what double or triple mutants have significant non-additive or epistatic effects. Meaning that double mutants have significantly shorter or longer vein length or wing length compared to a single rho mutant. 
2. Compare relationships as a function of genetic background. For example, does the type of genetic interaction or magnitude of genetic interaction change between genotypes in each background? 


Analysis Plans:

1. Multivariate analysis: Observe genentic interactions in each vein.
2. Consider overall wing length as an effect on vein length. 
