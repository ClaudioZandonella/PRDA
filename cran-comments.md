## Test environments
* Mac OS X 10.13 (on travis-ci):
  - release R 4.0.3
  - devel R 4.1.0 (2020-11-27 r79522)
* Ubuntu 16.04 (on travis-ci):
  - release R 4.0.2
  - devel R 4.1.0 (2020-11-26 r79513)
* win-builder:
  - release R 4.0.3
  - devel R 4.1.0 (2020-11-24 r79490)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

> * checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Claudio Zandonella Callegher <claudiozandonella@gmail.com>'
> 
> New submission

This is my first submission.

> Possibly mis-spelled words in DESCRIPTION:
>  Alto� (43:33)
>  Bertoldo (43:57)
>  Gelman (41:5)
>  al (43:42, 43:69)
>  et (43:39, 43:66)

The encoding is UTF-8. In the first case the surname is Altoè and is correctly displayed in all vignettes and documentation. In the other cases names and words are correctly spelled.

> Problems when formatting CITATION entries:
>  x:1: unexpected '}'

In the citation file my surname is enclosed in brackets "{Zandonella Callegher}" to ensure the correct use in bib files.


