# Resubmission

This is an update version 0.1.1 (see NEWS.md)

# Test environments
* local: macOS, R-release 
* rhub: Debian Linux, R-release, GCC
* rhub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* rhub: Fedora Linux, R-devel, clang, gfortran


# R CMD check results
* There were no ERRORs or WARNINGs.
* There is one NOTE: Found the following files/directories while checking for detritus in the temp directory (Windows only):
    'lastMiKTeXException' 

Per [rhub](https://github.com/r-hub/rhub/issues/503), This is due to a bug/crash in miktex and can be ignored.
