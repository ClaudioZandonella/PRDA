# Todo rOpenSci Packages

- [ ] You can choose to use = over <- as long you are consistent with one choice within your package. 
- [ ] README should include: https://devguide.ropensci.org/building.html#readme
- [ ] We recommend not creating README.md directly, but from a README.Rmd file (an R Markdown file) if you have any demonstration code. (usethis::use_readme_rmd())
- [x] Add #' @noRd to internal functions.
- [x] Only use package startup messages when necessary (function masking for instance). Avoid package startup messages like “This is foobar 2.4-0” or citation guidance because they can be annoying to the user. Rely on documentation for such guidance.
- [ ] https://devguide.ropensci.org/building.html#website
- [x] Use Imports instead of Depends for packages providing functions
- [ ] use the goodpractice package (goodpractice::gp()) as a guide to improve your package, since most exceptions to it will need to be justified
- [ ] Test coverage below 75% will likely require additional tests or explanation before being sent for review. Once you’ve set up CI, use your package’s code coverage report (cf this section of our book) to identify untested lines, and to add further tests. usethis::use_coverage()
- [ ] Both test status and code coverage should be reported via badges in your package README.
- [ ] R packages should have CI for all platforms when they contain Compiled code
- [ ] https://devguide.ropensci.org/ci.html continuos integration
- [ ] We urge package maintainers to make sure they are receiving GitHub notifications, as well as making sure emails from rOpenSci staff and CRAN maintainers are not going to their spam box.
- [ ] If you would like your package to also be submitted to Journal of Open-Source Software (JOSS), it should include a paper.md file describing the package. More detail on JOSS’s requirements can be found at their website. https://joss.theoj.org/about#author_guidelines
- [ ] use repostatus.org badges (which we recommend) https://www.repostatus.org/
- [ ] name function object_action()

send

- [ ] Next, open a new issue in the software review repository and fill out the template. https://github.com/ropensci/software-review/issues/new


other:

- [ ] problema del welch's t test e correzione d di cohen

Lettto fino a 1.8 Authorship
