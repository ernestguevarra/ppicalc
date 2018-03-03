################################################################################
#                                                                              #
# Group population by wealth quintiles                                         #
#                                                                              #
################################################################################
#
# Find the quintile cutoffs for PPI
#
qCutOff <- quantile(ppi, probs = c(0.2, 0.4, 0.6, 0.8, 1))
#
# Classify households by wealth quintile
#
pQuintile <- ifelse(ppi <= qCutOff[1], 1,
                    ifelse(ppi > qCutOff[1] & ppi <= qCutOff[2], 2,
                           ifelse(ppi > qCutOff[2] & ppi <= qCutOff[3], 3,
                                  ifelse(ppi > qCutOff[3] & ppi <= qCutOff[4], 4, 5))))
#
# Concatenate PPI indicators into single data.frame
#
povertyDF <- data.frame("uniqueID" = surveyData[ , "uniqueID"],
                        ppi, pQuintile, pPoverty)
