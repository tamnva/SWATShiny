Sys.sleep(0.000000)
options(BatchJobs.on.slave = TRUE, BatchJobs.resources.path = 'C:/Users/nguyenta/Documents/GitHub/SWATShiny/tam-files/resources/resources_1613603281.RData')
library(BatchJobs)
res = BatchJobs:::doJob(
	reg = loadRegistry('C:/Users/nguyenta/Documents/GitHub/SWATShiny/tam-files'),
	ids = c(3L),
	multiple.result.files = FALSE,
	staged = TRUE,
	disable.mail = FALSE,
	first = 3L,
	last = 3L,
	array.id = NA)
BatchJobs:::setOnSlave(FALSE)