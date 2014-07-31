# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:

#### doRUnit.R --- Run RUnit tests
####------------------------------------------------------------------------

## ----> put the bulk of the code e.g. in  ../inst/unitTests/runTests.R :

if(require("RUnit", quietly = TRUE)) {

  ## --- Setup ---

  wd <- getwd()
  pkg <- sub("\\.Rcheck$", '', basename(dirname(wd)))

  library(package = pkg, character.only=TRUE)

  path <- system.file("unitTests", package = pkg)

  stopifnot(file.exists(path), file.info(path.expand(path))$isdir)

  source(file.path(path, "runTests.R"), echo = TRUE)
}

################################################################################

