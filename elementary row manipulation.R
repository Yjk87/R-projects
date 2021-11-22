library(data.table)
library(dplyr)
library(compare)
library(ggplot2)
library(rentrez)
library(biomartr)

norm_tissue2<-read.table(file="normal_tissue.tsv", header=T,fill=TRUE,sep="\t")
norm_tissue2<-read.table(file="normal_tissue.tsv",sep="\t",comment.char="",quote = "\"",header=T)
norm_tissue<-read.table(file="normal_tissue.tsv",sep="\t",comment.char="",quote = "\"",header=T)
norm_tissue["Level"][norm_tissue["Level"]=="Not detected"]<-0
norm_tissue["Level"][norm_tissue["Level"]=="NA"]<-0
norm_tissue["Level"][norm_tissue["Level"]=="N/A"]<-0
norm_tissue["Level"][norm_tissue["Level"]=="Not representative"]<-0

norm_tissue["Level"][norm_tissue["Level"]=="Low"]<-1
norm_tissue["Level"][norm_tissue["Level"]=="Medium"]<-2
norm_tissue["Level"][norm_tissue["Level"]=="High"]<-3


sapply(norm_tissue, class)
norm_tissue[,5]<-sapply(norm_tissue[,5],as.numeric)
uniq_norm_tissue<-setDT(norm_tissue)[,lapply(.SD,mean),by=c(names(norm_tissue)[1:2]),.SDcols=5]
uniq_norm_tissue2<-uniq_norm_tissue[,c("Gene","Level")]



patho<-read.table(file="pathology.tsv", header=T,fill=TRUE,sep="\t")
sapply(patho, class)
uniq_patho<-setDT(patho)[,lapply(.SD,sum),by=c(names(norm_tissue)[1:2]),.SDcols=4:7]
uniq_patho$Sum1<-rowSums(uniq_patho[,3:5])
uniq_patho$Sum2<-rowSums(uniq_patho[,3:6])
uniq_patho$Mean<-((uniq_patho[,7])/(uniq_patho[,8]))

uniq_patho2<-uniq_patho[,c("Gene","Mean")]
uniq_patho2<-na.omit(uniq_patho2)

uniq_comparison<-inner_join(uniq_patho2,uniq_norm_tissue2)


uniq_comparison_sorted<-uniq_comparison[order(uniq_comparison[,Ratio],decreasing=TRUE),]


for_fetch<-data.frame(uniq_comparison_sorted[1:50,1])
for_fetch<-data.frame(uniq_comparison_sorted[2046:2100,1])

fetch_test<-entrez_search(db="pubmed",term=for_fetch,use_history=TRUE)

fetched_1<-apply(for_fetch,1, function(x) entrez_fetch(db="pubmed",id=x,rettype="abstract",retmode="",retmax=3) ) 
write(fetched_1,file="fetched_1.txt")

#for( seq_start in seq(1,200,50)){
#  recs <- entrez_fetch(db="nuccore", web_history=snail_coi$web_history,
#                       rettype="fasta", retmax=50, retstart=seq_start)
#  cat(recs, file="snail_coi.fasta", append=TRUE)
#  cat(seq_start+49, "sequences downloaded\r")
#}


for( seq_start in seq(1,200,50)){
  recs <- entrez_fetch(db="pubmed",id=for_fetch, rettype="abstract", retmax=3, retstart=seq_start)
  cat(recs, file="abs_fetched.txt", append=TRUE)
  cat(seq_start+49, "sequences downloaded\r")
}

for( seq_start in seq(1,200,50)){
  recs <- entrez_fetch(db="pubmed",web_history = fetch_test$web_history, rettype="abstract", retmax=3, retstart=seq_start)
  cat(recs, file="abs_fetched.txt", append=TRUE)
  cat(seq_start+49, "sequences downloaded\r")
}

go=c("GO:0051330","GO:0000080","GO:0000114","GO:0000082")
chrom=c(17,20,"Y")
ensembl <- useEnsembl(biomart = "genes")
ensembl <- useDataset(dataset = "hsapiens_gene_ensembl", mart = ensembl)

#combining
ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")

getBM(attributes= "hgnc_symbol",
      filters=c("go","chromosome_name"),
      values=list(go, chrom), mart=ensembl)

#annotating transmembrane proteins = GO:0016021
go=c("GO:0016021")
list=uniq_comparison_sorted[2046:2100,1]
getBM(attributes= "hgnc_symbol",
      filters=c("go","chromosome_name"),
      values=list(go, list), mart=ensembl)


affyids <- c("202763_at","209310_s_at","207500_at")
getBM(attributes = c('affy_hg_u133_plus_2', 'entrezgene_id'), filters = 'affy_hg_u133_plus_2', values = affyids, mart = ensembl)
