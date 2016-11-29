library(RUnit)
base_dir<-"D:/google drive/Anträge/formas/RE_EXTREME github/RE_EXTREME/scripts"
setwd(base_dir)
source("functions_gdx_transfer.R")
source("functions_variable_time_resolution.R")

testsuite.RE_EXTREME<-defineTestSuite("RE_EXTREME",
                                      dirs=file.path(paste(base_dir,"/tests",sep="")),
                                      testFileRegexp="^test.+\\.R",
                                      testFuncRegexp = "^test.+",
                                      rngKind="Marsaglia-Multicarry",
                                      rngNormalKind="Kinderman-Ramage")


testResults <- runTestSuite(testsuite.RE_EXTREME)
printTextProtocol(testResults)

                                      