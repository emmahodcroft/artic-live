# ARTIC-live!

Nextstrain data, pipelines, and visualizations for novel pathogen outbreak as part of ARTIC-network live workshop

Once you've run the pipeline, you can push the builds to `artic-live/auspice` so that they can be viewed via Nextstrain community builds feature. 
This github has this folder populated already so that one doesn't have to run the pipelines.

**Currently these builds still has the case labelled case128, and case numbers are nonsensical.**

## Organization

You can run two different 'types' of builds with this repository: 
- `background-build` will build S and L segment builds, with the primary case - one Snakefile can run both.
- `cluster-builds` will build 3 different subsamples of the outbreak - initial cases, then with the additional 2, then with all of the outbreak cases - one Snakefile can run all 3

See each snakefile in each folder for instructions/details.


## Links for background builds

[Background for L segment, with case](https://nextstrain.org/community/emmahodcroft/artic-live/arenavirus/L)

[Background for S segment, with case](https://nextstrain.org/community/emmahodcroft/artic-live/arenavirus/S)

[Tanglegram of the two above builds](https://nextstrain.org/community/emmahodcroft/artic-live/arenavirus/L:community/emmahodcroft/artic-live/arenavirus/S)

## Links for initial clusters 

[Initial cluster (n=24)](https://nextstrain.org/community/emmahodcroft/artic-live/HMFV/initial-cluster)

[Additional cases (n=26)](https://nextstrain.org/community/emmahodcroft/artic-live/HMFV/additional-cases)

[All cases (n=51)](https://nextstrain.org/community/emmahodcroft/artic-live/HMFV/all-cases)