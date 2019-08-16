if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ensembldb")
BiocManager::install("EnsDb.Hsapiens.v86")
BiocManager::install("EnsDb.Hsapiens.v75")

library(ensembldb)
library(EnsDb.Hsapiens.v86)
library(EnsDb.Hsapiens.v75)

edb <- EnsDb.Hsapiens.v86
gene <- 'MTOR'

getProteinDat <- function(gene, edb){
  
}

pd <- proteins(edb, filter = GeneNameFilter(gene),
               columns = c("tx_id", "protein_domain_id", "prot_dom_start", "prot_dom_end"),
               return.type = "AAStringSet")
xpd <- split(mcols(pd), mcols(pd)$tx_id)
names(xpd)
xpd[[1]][order(xpd[[1]]$prot_dom_start),]


#' Plot mutated protein residue annotation track
#' 
#' \code{plotAnnotTrack} renders annotation of AA sequence, amino acid, and amino acid positions 
#' 
#' @param ... wildtype | mutation (required)
#' @param ... Hugo gene name | gene identifier
#' @param ... amino acid position of mutation
#' @param ... amino acid residue of mutation
#' @param ... allele frequency of mutation
#' @param ... mutation type: Missense | Nonsense | In Frame Deletion | In Frame Insertion | Frame shift Deletion | Frame shift Insertion
#' @param ... colors default - alternating light and dark blue 
#' @param ... max AA window - maximum number of amino acids to render (max = 30, recommended for missense and nonsense mutations)
#' @param ... left-most position of amino acid residue to render (custom param)
#' @param ... right-most position of amino acid residue to render (custom param)
#' @param ... plot padding around figure
#' @param ... fontsize DNA seqeunce/codon - small
#' @param ... fontsize AA - large
#' @param ... fontsize AA position - small
#' @param ... font color annotation - black
#' @param ... font color mutation - black
#' @return ...
#' @examples 
#' 
plotAnnotTrack <- function (){

bkgcolors = RColorBrewer::brewer.pal(n=2,name = "Paired")

# Let's first check if the insertion or deletion exceeds the maximum window size for plotting
    if (length(grep ("deletion", mutation.type)) + length(grep ("insertion", mutation.type)) > 1){
      var.length <- calculateIndelLength()
      if (var.length >= max.window.size-1){
        msg = "Variant length exceeds maximum window size for zoomed view."
        print (msg)
        plotBirdsEyeView()
      }
    }
  
  # Let's plot some rectangles given the window size and colors
  
  # Total number of rectangles will always equal to the maximum window size
  # Unless given user-defined AA min and max coordinates
  n.residues = ifelse (!is.na (aa.start) & !is.na (aa.end), aa.end - aa.start , max.window.size)
  library (RColorBrewer)
  rect.width = 0.7
  rect.height = 1.0
  rect.fill = rep (bkgcolors, n.residues/2)
  
  # by default, the mutation is centered at the middle of the window 
  # if aa.max and min are provided, the mutation index will depend on the relative position it is within the 
  # provided coordinates
  
  if (wildtype | mutation.type == "Missense Mutation"){
      # For wildtype and substitution mutations, we color in all the rectangles
      rect(xleft = seq(from = 0, by = rect.width, length.out = n.residues), 
           xright = seq(from = rect.width, by = rect.width, length.out = n.residues),
           ybottom = rep (0, n.residues),
           ytop = rep (rect.height, n.residues), 
           col = rect.fill)
      
    }else if (mutation.type == "Nonsense Mutation") {
      # for Nonsense Mutation (Stop codon is introduced)
      # color up to stop color
      rect.fill[(n.residues/2):n.residues] <- "white"
      
      rect(xleft = seq(from = 0, by = rect.width, length.out = n.residues), 
           xright = seq(from = rect.width, by = rect.width, length.out = n.residues),
           ybottom = rep (0, n.residues),
           ytop = rep (rect.height, n.residues), 
           col = rect.fill)
      
    }else if (mutation.type %in% c("In Frame Deletion", "In Frame Insertion") ){
      # based on var.length, calculate which rectangles on both sides should be colored in blue
      if (grep("insertion", mutation.type, ignore.case = TRUE)){
        # color the inserted residues in red
      }
      
    }else if (mutation.type %in% c( "Frameshift Deletion", "Frameshift Insertion")) {
      # for frameshift
      # retain the AA in frame up until start of Indel
      # translate from cDNA the new codons and AA to be rendered
      # color all bases after mutation red
      if (grep("insertion", mutation.type, ignore.case = TRUE)){
        # color the inserted residues in red
      }
      
    }


  
  
  
  
}