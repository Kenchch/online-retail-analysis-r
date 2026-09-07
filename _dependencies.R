# Packages renv's static scan cannot see, because nothing calls them by name.
#
# svglite is the graphics device knitr renders the figures with (`dev =
# "svglite"` in the setup chunk). It is never `library()`d, so an implicit
# snapshot leaves it out of the lockfile and the rebuild fails on a clean
# machine with "there is no package called 'svglite'" -- which is exactly what
# happened in CI.
#
# This file is not sourced. It exists so the dependency is written down
# somewhere renv reads.

library(svglite)
