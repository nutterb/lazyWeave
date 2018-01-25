## Test environments
* Ubuntu 4.4.0-103-generic, R 3.4.3
* ubuntu 14.04.5 (on travis-ci), R 3.4.2
* win-builder devel (2018-01-04 r74054)

## R CMD check results
NOTES: 

There is a change of the maintainer e-mail address.  I am switching the e-mail
address from my employer's e-mail to my personal e-mail. This will make it 
easier for me to respond to e-mails regarding the package.

The previous maintainer e-mail is nutter@battelle.org.

The new maintainer e-mail is benjamin.nutter@gmail.com.

I fixed the error reported on the previous submission. 

## Downstream dependencies
The `pointblank` package is listed as a downstream dependency. Although listed
in `pointblank`'s Imports, there are no `lazyWeave` functions used within the
`pointblank` source code.  The development code for `pointblank` on GitHub
has removed `lazyWeave` as an Import.
