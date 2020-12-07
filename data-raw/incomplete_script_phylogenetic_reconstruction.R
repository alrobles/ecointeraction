data(anoletree)
bats
library(tidyverse)

load("data-raw/dengue_list_results_1000_rep.rds")

dengue_prediction <- list_results %>%
  purrr::map( ~ .x[[5]]) %>% reduce(bind_rows) %>%
  group_by(species) %>%
  summarise(susceptible_se = sd(susceptible)/length(list_results),
            susceptible = mean(susceptible))

# mapped.states(bats)
# ecomorph<- as.factor(getStates(anoletree,"tips"))
library(phytools)
#
# trees <- make.simmap(anoletree, ecomorph,model="ER",nsim=100)
# cols<- setNames(palette()[1:6],mapped.states(anoletree))
# plot(anoletree,cols,type="fan",fsize=0.8,lwd=3,ftype="i")
# add.simmap.legend(colors=cols,x=0.9*par()$usr[1],
#                   y=0.9*par()$usr[4],prompt=FALSE,fsize=0.9)
#
#
# obj <- contMap(tree,svl,plot=FALSE)
# batsr

mammals_tree_america <- read.tree("data-raw/mammals_america.tre")

mammals_tree_america_tip_names <- mammals_tree_america$tip.label %>%
  enframe %>%
  mutate(species = str_replace(value, "_", " ")) %>%
  left_join(dengue_prediction) %>% na.exclude()
mammals_tree_america_tip_names %>% na.exclude()

mammals_tree_america <- keep.tip(mammals_tree_america, mammals_tree_america_tip_names$value)
x <- fastBM(mammals_tree_america, internal = TRUE)

susceptible <- setNames(mammals_tree_america_tip_names$susceptible, mammals_tree_america_tip_names$value)

C <- vcvPhylo(mammals_tree_america)
pp <- rep(1, mammals_tree_america$Nnode + 1)
lik <- function(pp, C, x) -sum(dmnorm(c(x, pp[3:length(pp)]), rep(pp[2], nrow(C)),
                                      pp[1] * C, log = TRUE))
tt <- setNames(c(1, rep(mean(x), mammals_tree_america$Nnode)), c("sig2", 1:mammals_tree_america$Nnode + length(mammals_tree_america$tip)))
fit <- optim(tt, lik, C = C, x = x[mammals_tree_america_tip_names$value], method = "L-BFGS-B", lower = c(1e-06,
                                                                         rep(-Inf, tree$Nnode)))
print(fit)

fit <- fastAnc(mammals_tree_america, susceptible, vars=TRUE, CI=TRUE)

obj <- contMap(mammals_tree_america, susceptible, plot=FALSE)

plot(obj, type="fan",
      #legend = FALSE,
      legend=0.7*max (nodeHeights(mammals_tree_america) ),
      sig=2,fsize=c(0.7,0.9) )

#plot(obj, type="fan")
obj <- contMap(mammals_tree_america, susceptible, fsize = c(0.00001, 1), lwd = 2, outline = FALSE)
n <- length(obj$cols)
#obj$cols[1:n] <- colorRampPalette(c("#FF0000","#0000FF"), space="Lab")(n)

# addalpha()
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}
library(RColorBrewer)

obj$cols[1:n] <- colorRampPalette(c("#999999", "lightblue", "yellow", "orange", "red"),  space="Lab")(n)
#obj$cols[1:n] <- colorRampPalette(brewer.pal(11, 'Spectral'))(n)
#obj$cols[1:n] <- addalpha(obj$cols[1:n], 0.8)


#plot(obj,  fsize = c(0.00001, 1), lwd = 5, outline = FALSE, type = "fan")
{
png(file = "/home/alrobles/Imágenes/dengue_susceptible_tree.png",
    units="in", width=5, height=5, res=300)
#plot(obj,  fsize = c(0.00001, 0.00001), lwd = 1, outline = FALSE, type = "fan", legend = FALSE)
obj <- contMap(mammals_tree_america, susceptible, fsize = c(0.00001, 1), lwd = 2, outline = FALSE)
n <- length(obj$cols)
obj$cols[1:n] <- colorRampPalette(c("#999999", "lightblue", "yellow", "orange", "red"),  space="Lab")(n)
plot.contMap(obj, lwd = 2, type = "fan",  fsize = c(0.00001, 4), outline = FALSE, legend = FALSE )

# plot.contMap(x = obj,
#              fsize = c(0.00001, 4),
#              lwd = 2,
#              type="fan",
#              legend = FALSE
#          )
add.color.bar(200, obj$cols, title="Susceptibility",
              lims=c(0.202, 0.9), digits=3, direction="upwards",
              subtitle="", lwd=15, x=250, y=-120, prompt=FALSE)

dev.off()
}
el #write_rds(obj, "data-raw/mammal_dengue_tree.rda")


# hidden stuff in Cairo
i = Cairo:::.image(dev.cur())
r = Cairo:::.ptr.to.raw(i$ref, 0, i$width * i$height * 4)
dim(r) = c(4, i$width, i$height) # RGBA planes
# have to swap the red & blue components for some reason
r[c(1,3),,] = r[c(3,1),,]
# now use the png library
p = writePNG(r, raw()) # raw PNG bytes


obj<-contMap(mammals_tree_america, susceptible, plot=FALSE)
obj<-setMap(obj,colors=c("blue","purple","red"))
h <- max(nodeHeights(obj$tree))
plot(obj,legend=FALSE,xlim=c(-0.25*h,h),lwd=2, type = "fan",
     outline=FALSE,ftype="off")
add.color.bar(300, obj$cols, title="Susceptibility",
              lims=NULL, digits=3, direction="upwards",
              subtitle="", lwd=15, x=250, y=-120,prompt=FALSE)

## get line width in user units
LWD <- diff(par()$usr[1:2])/dev.size("px")[1]
lines(x=c(250, 250), y=c(120, 120) )
nticks<-10
Y<-cbind(seq(2,Ntip(obj$tree)-1,length.out=nticks),
         seq(2,Ntip(obj$tree)-1,length.out=nticks))
X<-cbind(rep(-0.2*h+LWD*15/2,nticks),
         rep(-0.2*h+LWD*15/2+0.02*h,nticks))
for(i in 1:nrow(Y)) lines(X[i,],Y[i,])
ticks<-seq(obj$lims[1],obj$lims[2],length.out=nticks)
text(x=X[,2],y=Y[,2],round(ticks,3),pos=4,cex=0.8)
