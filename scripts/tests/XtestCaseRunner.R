library(RUnit)

testsuite.RE_EXTREME<-defineTestSuite("RE_EXTREME",
                                      dirs=file.path(paste(base_dir,"/tests",sep="")),
                                      testFileRegexp="^test.+\\.R",
                                      testFuncRegexp = "^test.+",
                                      rngKind="Marsaglia-Multicarry",
                                      rngNormalKind="Kinderman-Ramage")


testResults <- runTestSuite(testsuite.RE_EXTREME)
printTextProtocol(testResults)

                                      