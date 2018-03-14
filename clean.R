source("./utilfuncs.R")
library(dplyr)
groupnon <- bind_rows(non,non2,non3,non4,non5, .id="id")
groupnon <- selectjsym(groupnon)

# groupran <- bind_rows(ran,ran2,ran3,ran4,ran5, .id="id")
# groupran <- selectjsym(groupran)

# group10 <- bind_rows(scfp, mp, siaf1, siap, siar, vm2, vm, non,ran, X1657val , .id="id")
# group10 <- selectjsym(group10)

groupalg <- bind_rows(scfp, mp, siaf1, siap, siar, vm2, vm, .id="id")
groupalg <- selectjsym(groupalg)

# groupall <- bind_rows(scfp, mp, siaf1, siap, siar, vm2, vm, non,non2,non3,non4,non5,ran,ran2,ran3,ran4,ran5, X1657val , .id="id")
# groupall <- selectjsym(groupall)

pat <- selepatjsym(X1657val)