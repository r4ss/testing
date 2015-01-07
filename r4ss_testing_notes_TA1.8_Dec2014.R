library(r4ss)
update_r4ss_files(local='c:/github/r4ss/R/')

devtools::install_github("r4ss/r4ss", ref="TA1.8testing")

# sablefish model
sab <- SS_output('C:/ss/Sablefish/Sablefish2011/2011 sablefish test run')
SS_plots(sab, png=TRUE, aalresids=TRUE)

#sab <- SS_output('C:/ss/sablefish/sablefish_2011_test_run',forecast=FALSE)
graphics.off(); rm(.SavedPlots)
SS_plots(sab,plot=16,fleets=5)
SS_plots(sab,plot=16,fleets=5,png=TRUE)


# undertainty vector for SSplotComparisons
dir <- 'C:/SS/R/r4ss_talk_Jan10/examples/'
s1 <- SS_output(file.path(dir, 'simple'))
s2 <- SS_output(file.path(dir, 'simple_hockey'))
s3 <- SS_output(file.path(dir, 'simple_production'))

s.summarize <- SSsummarize(list(s1,s2,s3))
SSplotComparisons(s.summarize)

SSplotComparisons(s.summarize, uncertainty=c(FALSE,TRUE,FALSE),subplot=1)
SSplotComparisons(s.summarize, uncertainty=2,subplot=1)


