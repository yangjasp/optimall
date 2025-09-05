# optimall 1.3.0

- New version that refers to JSS manuscript <doi:10.18637/jss.v114.i10>.

# optimall 1.2.0

## Enhancements

- Added A-optimality functionality to optimum_allocation() and allocate_wave().

# optimall 1.1.1

## Enhancements

- Added a new vignette describing estimation in studies using multi-wave sampling using `survey`.

## Bug fixes

- Revised tests to pass on OpenBLAS.

# optimall 1.1.0

## Enhancements

A number of enhancements were made to the Multiwave Object:

- set_mw() and get_mw() for easy access to slots
- Variance parameters and wave-specific sampling probabilities stored by default.
- Added arguments to apply_multiwave() and sample_strata() for storing sampling probabilities.
- Added arguments to apply_multiwave(), sample_strata(), and merge_samples() to store sampling waves.


## Bug fixes

- Updated function tests to pass CRAN check on M1Mac

# optimall 0.1.5

## Bug fixes

- Updated function tests to pass CRAN check on Windows

# optimall 0.1.4

## Enhancements

- allocate_wave() now defaults to providing a detailed summary of allocation

## Bug fixes

- Increased phantomJS timeout time for optimall_shiny() test to pass CRAN check

# optimall 0.1.3

## Enhancements

- Updated documentation for get_data()
- Added options for allocation method in allocate_wave()

## Bug fixes

- Fixed input binding error for testing shiny on windows to pass CRAN check

# optimall 0.1.2

## Bug fixes

- Updated function tests to pass CRAN check on M1mac

# optimall 0.1.1

## Bug fixes

- Fixed typos in documentation
- Updated function tests to pass CRAN check regarding all.equal.numeric()

# optimall 0.1.0

* Initial release
