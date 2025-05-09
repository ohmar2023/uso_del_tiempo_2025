
ind

N = base_insumos$N_pobl
M = base_insumos$upm_pobl_M
rho = base_insumos$rho
mu = base_insumos$estimacion_media_mu
sigma = base_insumos$estimaciion_sd_sigma
delta = base_insumos$mer_delta
conf = base_insumos$conf
m = c(11:11)
tnr = 0.2

i=2
ss4m(N = N[i], 
     mu = mu[i], 
     sigma = sigma[i], 
     DEFF = 1 + (12 - 1) * rho[i], 
     conf = conf[i],  
     error = "rme", 
     delta = delta[i], 
     plot = FALSE)

base_insumos$dominio[i]
base_insumos$b[i]
rho[2]
