monthly_climate <- function(data,
                            datetime,
                            t1,
                            t2,
                            z1 = 2,
                            z2 = 10,
                            v1,
                            v2,
                            hum1,
                            hum2,
                            rad_bal = NULL,
                            soil_flux,
                            p = NULL,
                            elev = 270,
                            lat = 8.683303333333333,
                            alt = 50.84050277777778,
                            surface_type = "Wiese",
                            obs_height = 0.3,
                            albedo
                            ){
  t1 <- data[,t1]
  t2 <- data[,t2]
  v1 <- data[,v1]
  v2 <- data[,v2]
  z1 <- data[,z1]
  z2 <- data[,z2]
  rad_bal <- data[,rad_bal]
  soil_flux <- data[,soil_flux]
  hum1 <- data[,hum1]
  hum2 <- data[,hum2]

  #roughness length
  z0 <- turb_roughness_length(surface_type,obs_height)

  #displacement height
  d0 <- turb_displacement(obs_height)

  #friction velocity
  ustar <- turb_ustar(v1,z1,z0)

  #pressure
  if(is.null(p)){p1 <- pres_p((elev+z1),t1)}
  else if(is.null(p)==F){p1 <- p}
  p2 <- pres_p((elev+z2),t2)

  #air density
  air_density <- pres_air_density(p1,t1)

  #Gradient-Richardson-Number ##in Ausgabe
  grad_rich_no <- turb_flux_grad_rich_no(t1,t2,z1,z2,v1,v2,p1,p2)

  #stability ##in Ausgabe
  stability <- turb_flux_stability(grad_rich_no)

  #Monin-Obhukov-Length ##in Ausgabe
  monin <- turb_flux_monin(stability,z1,z2,z0,v1,v2,t1,t2,ustar)

  #exchange quotient
  ex_quotient <- turb_flux_ex_quotient_imp(stability,ustar,monin,z1,air_density)

  #turbulent impulse exchange ##in Ausgabe
  turb_flux <- turb_flux_imp_exchange(ex_quotient,v1,v2,z1,z2)








  ##################################################################################################
  rad_sw_toa <- rad_sw_toa(datetime,lat,lon)
  sol_elevation <- sol_elevation(datetime,lat,lon)
  trans_total <- trans_total(sol_elevation,t1,elev,pressure = p)
  rad_sw_ground_horizontal <- rad_sw_ground_horizontal(rad_sw_toa, trans_total)

  rad_sw_reflected <- rad_sw_reflected(rad_sw_ground_horizontal, albedo)

  rad_sw_radiation_balance <- rad_sw_radiation_balance(rad_sw_ground_horizontal,rad_sw_reflected)

  #####emissivity_surface <- surface_properties[which(surface_properties$surface_type==as.factor(surface_type)),]
  rad_lw_surface <- rad_lw_surface(t1,emissivity_surface)


  if(is.null(radbal)){
    radbal <- rad_bal_total(rad_sw_radiation_balance,rad_lw_surface,rad_lw_atmospheric)
  }

  #Latent Heat Priestley-Taylor Method
  latent_priestley_taylor <- latent_priestley_taylor(t1,rad_bal,soil_flux)

  #Latent Heat Penman Method
  latent_penman <- latent_penman(datetime,v1,t1,hum1,z1,rad_bal,elev,lat)

  #Latent Heat using Monin-Obukhov length ##### Eingangsvariablen anpassen ####
  latent_monin <- latent_monin()

  #Latent Heat using Bowen Method
  latent_bowen <- latent_bowen()

  #Sensible Heat Priestley-Taylor Method  #### Eingangsvariablen anpassen ####
  sensible_priestley_taylor <- sensible_priestley_taylor()

  #Sensible Heat using Monin-Obukhov length #### Eingangsvariablen anpassen ####
  sensible_monin <- sensible_monin()

  #Sensible Heat using Bowen Method #### Eingangsvariablen anpassen ####
  sensible_bowen <- sensible_bowen()

}
