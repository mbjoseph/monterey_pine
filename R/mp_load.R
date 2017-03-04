# load monterey pine data

load("./rdata/fatedata_OP_feb17.rdata")
op_fate <- fatemx
op_surv <- survmx

load("./rdata/fatedata_SSC_feb17.rdata")
ssc_fate <- fatemx
ssc_surv <- survmx

load("./rdata/fatedata_EF_feb17.rdata")
ef_fate <- fatemx
ef_surv <- survmx

load("./rdata/fatedata_MSC_feb17.rdata")
msc_fate <- fatemx
msc_surv <- survmx

fate <- list(ef = ef_fate, msc = msc_fate, op = op_fate, ssc = ssc_fate)
surv <- list(ef = ef_surv, msc = msc_surv, op = op_surv, ssc = ssc_surv)

rm(list = "ef_fate","ef_surv", "msc_fate", "msc_surv", "op_fate","op_surv",
   "ssc_fate","ssc_surv", "fatemx", "survmx" )
