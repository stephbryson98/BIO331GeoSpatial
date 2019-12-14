library(taxonomizr)
library(rBLAST)
library(ggplot2)
library(IRanges)
library(XVector)
dna <- readDNAStringSet('Ar73_R2.fastq.gz', format='fastq')
bl <- blast(db="/usr/share/data/ncbi/nt/nt.fa")

#Run BLAST query
cl <- predict(bl, dna[1:20])

cl[1:5,]
#to view first 5 hits
summary(cl)
accid = as.character(cl$SubjectID)


taxaNodes<-read.nodes.sql("/usr/share/data/taxonomizr/nodes.dmp")
taxaNames<-read.names.sql("/usr/share/data/taxonomizr/names.dmp")

#takes accession number and gets the taxonomic ID
ids<-accessionToTaxa(accid, '/usr/share/data/taxonomizr/accessionTaxa.sql')
#taxlist displays the taxonomic names from each ID #
taxlist=getTaxonomy(ids, taxaNodes, taxaNames)
cltax=cbind(cl,taxlist) #bind BLAST hits and taxonomy table
colnames(cltax)


#ggplot for top hits or percent identity of each family
ggplot(data=cltax) + 
  geom_boxplot(aes(x=family, y=Perc.Ident)) + 
  theme(axis.text.x = element_text(angle=90)) +
  ylim(c(85,100))

#take the taxonomic names that have above a 95% identity and place in new data set to manipulate
newdata <- subset(cltax, Perc.Ident >= 95, 
                  select=c(family, Perc.Ident))
#creates plot of selected dataset comparing family id and percent identity 
ggplot(data=newdata) + aes(x = family, y = Perc.Ident) +
  geom_point(alpha=0.3, color="tomato", position = "jitter") +
  geom_boxplot(alpha=0) + coord_flip()