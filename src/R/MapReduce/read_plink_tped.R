read.plink.tped <- function(
  tped.hdfs.path,            # space-delimited tped file, or directory of tped files
  output.hdfs.path,          # where to write the output SNP records
  mapred.reduce.tasks=0,     # manually control the numnber of output files (splits)
  minutes.until.timeout=10   # override Hadoop default timeout
) {

  jobname = "read.plink.tped"
  rhinit.singleton()

  map = expression({
    rhcounter("PROGRESS", "_TASK_", 1)

    for (i in seq_along(map.values)) {  # each line becomes one SNP record
      rhcounter("PROGRESS", "_IN_", 1)
      key = map.keys[[i]]
      value = map.values[[i]]

      tokens  = strsplit(value, " +")[[1]]  # split on one ore more spaces

      chromosome = tokens[1]                # text
      rsid       = tokens[2]                # text
      distance   = as.integer(tokens[3])    # integer
      position   = as.integer(tokens[4])    # integer
      genotype = tokens[-seq(1, 4)]         # text

      # which is the minor allele?
      t = table(genotype)
      alleles = setdiff(names(table(genotype)), c("0","N"))  # ignore missing alleles
      t = t[alleles]
      minor.allele = NULL
      major.allele = NULL
      if (t[1] == t[2]) {  # tie
        minor.allele = names(t[1])
        major.allele = names(t[2])
      } else {
        minor.allele = names(which(t==min(t)))
        major.allele = names(which(t!=min(t)))
      }

      # encode genotype using 0,1,2 minor allele count 
      minor.allele.count = rowSums(matrix(genotype==minor.allele, ncol=2, byrow=T))
      major.allele.count = rowSums(matrix(genotype==major.allele, ncol=2, byrow=T))

# TODO: test sex chromosome
      # flag missing / incomplete genotypes
      if(length(intersect(chromosome, c("23", "24", "X", "Y"))))  # is sex chromosome
        is.missing.or.incomplete = (minor.allele.count + major.allele.count) < 1
      else
        is.missing.or.incomplete = (minor.allele.count + major.allele.count) < 2
      minor.allele.count[is.missing.or.incomplete] = NA

      new.value = list(
        chromosome = chromosome,
        rsid = rsid,
        distance = distance,
        position = position,
        snp.vector = minor.allele.count
      )

      # emit
      new.key = rsid
      rhcollect(new.key, new.value)
      rhcounter("PROGRESS", "_OUT_", 1)
    }
  })

  reduce = identity.reduce
  
  inout = c("text", "sequence")
  ifolder = tped.hdfs.path
  ofolder = output.hdfs.path

  z = rhmr(
    jobname=jobname,
    map=map,
    reduce=reduce,
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    mapred=list(
      mapred.reduce.tasks=mapred.reduce.tasks,
      mapred.task.timeout=minutes(minutes.until.timeout),
      rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
      rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE
    )
  )

  rhex(z, async=F)
}

