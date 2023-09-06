# Resubmission

This is an update version 0.1.3 (see NEWS.md)

# Test environments
* local: macOS, R-release 
* rhub: Debian Linux, R-release, GCC
* rhub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* rhub: Fedora Linux, R-devel, clang, gfortran


# R CMD check results
* There were no ERRORs, WARNINGs.
* There are two NOTEs: 
    * New maintainer. Maintainer email address has been updated. 
    * On Fedora: Skipping checking HTML validation: no command 'tidy' found.
    
The second note appears to be an issue specific to the Fedora test environment and out of my control ([rhub/rhub#548](https://github.com/r-hub/rhub/issues/548)).
