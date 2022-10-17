This folder contains a major script in R language and two additional scripts (*.sh and *bash) to process high volumes of raw qPCR data generated by Roche LightCycler 480. The principal R script includes a preprocessing step of replicated data, with quality control based on standard statistical measures  and a normalization phase based on the use of a set of housekeeping genes. This is a custom analysis but can be easily extended while changing the thresholds used. 