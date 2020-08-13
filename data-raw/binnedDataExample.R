# create binnedDataExample.rda

# load dtBcs4Em
load(here::here("data/dtBcs4Em.rda"))

# rename vars
cols2drop <- names(dtBcs4Em)
dtBcs4Em[, id := bcsid]
dtBcs4Em[, binnedData := parInc]
dtBcs4Em[, paste0(cols2drop) := NULL]

binnedDataExample <- dtBcs4Em

# save as binnedDataExample.rda
use_data(binnedDataExample)
