###############################################################################
#____________________________join data for analysis___________________________#
###############################################################################

# join data according to condition  -> own script
dgpVec <- c("inter", "pwlinear", "nonlinear3")

block_size <- 10
nSamples_total <- 100

nFolders <- nSamples_total / block_size


for(idx_dgp in 1:3){
  for(idx_folder in seq_len(nFolders)) {
    for(idx_sample in 1:block_size){
      
    }
  }
}
  



# Forschungsfragen anschauen -> AV(s)? 


# anova mit sample faktoren -> delta Performanz Super learner vs. bestes Einzelmodell
# welche bedingungen sind besonders bedeutend für plots 
# generalisiertes eta quadrat
# marginale means -> post hoc tests (post hoc e means) -> HE nicht Interaktionen