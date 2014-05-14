gwas.sparse.report <- function(
  input.hdfs.path,
  output.hdfs.path,
  statistic.name="p.value",
  phenotype.number.offset=0,  # in case we did batches of phenotypes
  bluesnp.code.hdfs.path="/BlueSNP/BlueSNP_core.R",  # where the BlueSNP core codebase is located
  user.code.hdfs.path="/BlueSNP/do_not_delete.R",    # where (optional) user-defined functions are located
  rhipe_map_buffsize=100,                           # number of records per batch **250MB Rhipe limit**
  minutes.until.timeout=5,                           # override Hadoop default timeout
  mapred.reduce.tasks=1
) {

  # generate list report (sparse matrix) from gwas(is.sparse.output=T)

  require(Rhipe)
  rhinit.singleton()

  shared = c(bluesnp.code.hdfs.path, user.code.hdfs.path)
  
  setup = expression({
    source(BLUESNP_CODE)
    source(USER_CODE)
    phenotype.number.offset = PHENOTYPE.NUMBER.OFFSET
    statistic.name = STATISTIC.NAME 
  })
  
  setup = expressionBuilder(setup, 
    BLUESNP_CODE=deparse(stripPath(bluesnp.code.hdfs.path)),
    USER_CODE=deparse(stripPath(user.code.hdfs.path)),
    PHENOTYPE.NUMBER.OFFSET = deparse(phenotype.number.offset),
    STATISTIC.NAME=deparse(statistic.name)
  )

  # example keys:
  # "rs564002" "n.individuals"
  # "rs564002" "df"           
  # "rs564002" "beta"         
  # "rs564002" "se"           
  # "rs564002" "t.statistic"  
  # "rs564002" "p.value"

  map = expression({
    rhcounter("PROGRESS", "_TASK_", 1)

    for (i in seq_along(map.values)) {
      rhcounter("PROGRESS", "_IN_", 1)
      key = map.keys[[i]]
      value = map.values[[i]]
      
      key1 = key[1]  # "rs564002"
      key2 = key[2]  # "p.value"
      
      if (key2==statistic.name) {  # this is a "p.value" record
      
        phenotype.number = value$phenotype.number + phenotype.number.offset
        statistic.value = value$statistic.value
      
        new.key = c(key1, as.character(phenotype.number))  # for sorting
#        new.value = c(key1, phenotype.number, statistic.value)
        new.value = c(key1, phenotype.name, statistic.value)
        
        rhcollect(new.key, new.value)
        rhcounter("PROGRESS", "_OUT_", 1)

      }
    }
  })
  
  reduce = identity.reduce

  inout = c("sequence", "text")
  ifolder = input.hdfs.path
  ofolder = output.hdfs.path
  
  jobname = "gwas.sparse.report"

  z = rhmr(
    jobname=jobname,
    setup=setup, 
    copyFiles=T,
    map=map,
    reduce=reduce, 
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    shared=shared,
    orderby="character",
    mapred=list(
      mapred.reduce.tasks=mapred.reduce.tasks,
      mapred.task.timeout=minutes(1),
      rhipe_map_buffsize=rhipe_map_buffsize,
      mapred.textoutputformat.usekey=F,
      mapred.field.separator="\t",
      mapred.textoutputformat.separator="\t"
    )
  )
  
  rhex(z, async=F)
  
}

