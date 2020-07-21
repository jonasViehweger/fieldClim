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
                            soil_flux = NULL,
                            depth1 = NULL, #needed when soil_flux unknown
                            depth2 = NULL, #needed when soil_flux unknown
                            moisture = NULL, #needed when soil_flux unknown
                            texture = NULL, #needed when soil_flux unknown
                            p = NULL,
                            elev = 270,
                            lat = 8.683303333333333,
                            alt = 50.84050277777778,
                            surface_type = "Acker",
                            obs_height = 0.3,
                            albedo,
                            slope = NULL, #if not NULL, the rad_bal with topography will be calculated
                            valley = F
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

  #pressure
  if(is.null(p)){p1 <- pres_p((elev+z1),t1)}
  else if(is.null(p)==F){p1 <- p}
  p2 <- pres_p((elev+z2),t2)

  #air density
  air_density <- pres_air_density(p1,t1)

  ### Calculation of turbulent fluxes
  #roughness length
  z0 <- turb_roughness_length(surface_type,obs_height)

  #displacement height
  d0 <- turb_displacement(obs_height)

  #friction velocity
  ustar <- turb_ustar(v1,z1,z0)
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

  ### Caclulation of radiances
  #calculation of radiation balance, if unknown
  rad_sw_toa <- rad_sw_toa(datetime,lat,lon)
  sol_elevation <- sol_elevation(datetime,lat,lon)
  trans_total <- trans_total(sol_elevation,t1,elev,pressure = p)
  rad_sw_ground_horizontal <- rad_sw_ground_horizontal(rad_sw_toa, trans_total)
  rad_sw_reflected <- rad_sw_reflected(rad_sw_ground_horizontal, albedo)
  sol_azimuth <- sol_azimuth(datetime,lat,lon)

  #shortwave radiation balance
  rad_sw_radiation_balance <- rad_sw_radiation_balance(rad_sw_ground_horizontal,rad_sw_reflected)

  emissivity_surface <- surface_properties[which(as.character(surface_properties$surface_type)==surface_type),]$emissivity
  rad_lw_surface <- rad_lw_surface(t1,emissivity_surface)

  emissivity_air <- rad_emissivity_air(t1,elev,p1)
  rad_lw_atmospheric <- rad_lw_atmospheric(emissivity_air,t1)

  #total radiation balance (without topography)
  if(is.null(radbal)){
    radbal <- rad_bal_total(rad_sw_radiation_balance,rad_lw_surface,rad_lw_atmospheric)
  }

  #total radiation balance with topography
  if(is.null(slope)==F){
    rad_sw_reflected_by_terrain <- rad_sw_reflected_by_terrain(slope,valley,sol_elevation,sol_azimuth,exposition = 0,rad_sw_ground_horizontal,albedo)
    terr_sky_view <- terr_sky_view(slope,valley)
    rad_bal_total_with_topography <- rad_bal_total_with_topography(rad_sw_reflected_by_terrain, rad_lw_surface,rad_lw_atmospheric,terr_sky_view)
  }
  else if(is.null(slope)){rad_bal_total_with_topography <- NULL}

  ### Caclulation of latent and sensible heat fluxes
  #cakculation of soil_flux, if unknown
  thermal_cond <- soil_thermal_cond(moisture,texture)

  if(is.null(soil_flux)){
    soil_flux <- soil_heat_flux(t1,t2,depth1,depth2, thermal_cond)
  }

  #Latent Heat Priestley-Taylor Method
  latent_priestley_taylor <- latent_priestley_taylor(t1,radbal,soil_flux)

  #Latent Heat Penman Method
  latent_penman <- latent_penman(datetime,v1,t1,hum1,z1,radbal,elev,lat)

  #Latent Heat using Monin-Obukhov length
  latent_monin <- latent_monin(hum1,hum2,t1,t2,p1,p2,z1,z2)

  #Latent Heat using Bowen Method
  latent_bowen <- latent_bowen(t1,t2,hum1,hum2,p1,p2,z1,z2,radbal,soil_flux)

  #Sensible Heat Priestley-Taylor Method
  sensible_priestley_taylor <- sensible_priestley_taylor(t1,radbal,soil_flux)

  #Sensible Heat using Monin-Obukhov length
  sensible_monin <- sensible_monin(t1,t2,p1,p2,z1,z2,monin,ustar,grad_rich_no)

  #Sensible Heat using Bowen Method
  sensible_bowen <- sensible_bowen(t1,t2,hum1,hum2,p1,p2,z1,z2,radbal,soil_flux)




  #### Creation of output dataframe
  out <- data.frame(datetime = datetime,
                    z1 = z1,
                    z2 = z2,
                    t1 = t1,
                    t2 = t2,
                    v1 = v1,
                    v2 = v2,
                    p1 = p1,
                    p2 = p2,
                    hum1 = hum1,
                    hum2 = hum2,
                    stability_of_atmosphere = stability,
                    soil_flux = soil_flux,
                    sw_radiation_balance = rad_sw_radiation_balance,
                    lw_radiation_balance = (rad_lw_surface - rad_lw_atmospheric),
                    total_radiation_balance = radbal,
                    total_radiation_balance_with_topo = rad_bal_total_with_topography,
                    turbulent_flux = turb_flux,
                    sensible_heat_priestly_taylor <-sensible_priestly_taylor,
                    latent_heat_priestly_taylor <-latent_priestly_taylor,
                    sensible_heat_penman <-sensible_penman,
                    latent_heat_penman <-latent_penman,
                    sensible_heat_bowben <-sensible_bowen,
                    latent_heat_bowen <-latent_bowen,
                    sensible_heat_monin <-sensible_monin,
                    latent_heat_monin <-latent_monin
                    )
  return(out)
}
