#Windows
dat=read.delim("clipboard",sep="\t",header=TRUE);source("https://stat.zanet.biz/useRs/scripts/nma-with-netmeta.R")

#Mac
dat=read.delim(pipe("pbpaste"),sep="\t",header=TRUE);source("https://stat.zanet.biz/useRs/scripts/nma-with-netmeta.R")
