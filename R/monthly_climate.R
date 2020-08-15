#' Monthly climate, you know
#'
#' @param data Input data as a dataframe.
#' @param datetime Name of datetime-coloumn in data. Form: Character string. NOTE: datetime needs to be converted POSIXlt-Format (see ?as.POSIXlt)
#' @param t1 Name of the coloumn in data, which contains lower temperature data. Form: Character string.
#' @param t2 Name of the coloumn in data, which contains upper temperature data. Form: Character string.
#' @param z1 Lower measurement height in m. Preset: 2m.
#' @param z2 Upper measurement height in m. Preset: 2m.
#' @param v1 Name of the coloumn in data, which contains lower wind speed data. Form: Character string.
#' @param v2 Name of the coloumn in data, which contains upper wind speed data. Form: Character string.
#' @param hum1 Name of the coloumn in data, which contains lower humidity data. Form: Character string.
#' @param hum2 Name of the coloumn in data, which contains upper humidity data. Form: Character string.
#' @param p Name of the coloumn in data, which contains lower pressure data. Form: Character string. Preset: NULL. Note: If NULL, pressure will be calculated.
#' @param rad_bal Name of the coloumn in data, which contains total radiation balance. Form: Character string. Preset: NULL. Note: If NULL, rad_bal will be calculated (Albedo needed).
#' @param sw_bal Name of the coloumn in data, which contains shortwave radiation balance. Form: Character string. Preset: NULL. Note: If NULL, sw_bal will be calculated (Albedo needed).
#' @param lw_bal Name of the coloumn in data, which contains longwave radiation balance. Form: Character string. Preset: NULL. Note: If NULL, lw_bal will be calculated (Albedo needed).
#' @param albedo Name of the coloumn in data, which contains albedo. Form: Character string. Preset: NULL. Note: Only needed, if radiation balances shall be calculated.
#' @param slope Slope of hillside in %. Form: Integer or Numeric. Preset: NULL.
#' @param valley TRUE, if climate station is positioned in a valley. Form: Boolean. Preset: FALSE.
#' @param surface_type Surface Type. Form: Character string. One of: "Wiese", "Acker", "Gruenflaeche", "Strasse", "Landwirtschaft", "Siedlung", "Nadelwald", "Laubwald", "Mischwald", "Stadt". Preset: "Wiese.
#' @param obs_height Height of vegetation in m. Preset: 0.3.
#' @param soil_flux Name of the coloumn in data, which contains soil flux. Form: Character string. Preset: NULL. Note: If NULL, soil_flux will be calculated.
#' @param depth1 Upper depth of measurment in m. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param depth2 Lower depth of measurment in m. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param ts1 Name of the coloumn in data, which contains upper ground temperature data. Form: Character string. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param ts2 Name of the coloumn in data, which contains lower ground temperature data. Form: Character string. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param moisture Name of the coloumn in data, which ground moisture data. Form: Character string. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param texture Texture of ground. Form: Character string. One of: "clay", "sand". Preset: "clay".
#' @param elev Elevation of location above sea level in m. Preset: 270 m (climate station caldern).
#' @param lat Latitude of location. Preset: 50.840502777777788.683303333333333 (climate station caldern).
#' @param lon Longitude of location. Preset: 8.683303333333333 (climate station caldern).
#'
#' @return Dataframe containing:
#' "datetime",
#' "height_lower[m]",
#' "height_upper[m]",
#' "temperature_lower[C°]",
#' "temperature_upper[C°]",
#' "wind_speed_lower[m/s]",
#' "wind_speed_upper[m/s]",
#' "pressure_lower[hPa]",
#' "pressure_upper[hPa]",
#' "humidity_lower[%]",
#' "humidity_upper[%]",
#' "atmospheric_stability",
#' "soil_flux[W/m²]",
#' "shortwave_radiation_balance[W/m²]",
#' "longwave_radiation_balance[W/m²]",
#' "total_radiation_balance[W/m²]",
#' "turbulent_flux[kg/(m*s)]", "
#' sensible_heat[W/m^2]_Priestly-Taylor",
#' "latent_heat[W/m^2]_Priestly-Taylor",
#' "latent_heat[W/m^2]_Penman",
#' "sensible_heat[W/m^2]_Bowen",
#' "latent_heat[W/m^2]_Bowen",
#' "sensible_heat[W/m^2]_Monin",
#' "latent_heat[W/m^2]_Monin"
#' @export
#'
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
                            p = NULL,
                            rad_bal = NULL, #if NULL -> will be calculated, albedo needed
                            sw_bal = NULL, #if NULL -> will be calculated, albedo needed
                            lw_bal = NULL, #if NULL -> tough luck, won't be calculated
                            albedo = NULL, #needed, if radiation balance is unknown an shall be calulated
                            slope = NULL, #if not NULL, the rad_bal with topography will be calculated
                            valley = F,
                            surface_type = "Wiese",  #climate station caldern
                            obs_height = 0.3,
                            soil_flux = NULL,
                            depth1 = NULL, #needed when soil_flux unknown
                            depth2 = NULL, #needed when soil_flux unknown
                            ts1 = NULL,
                            ts2 = NULL,
                            moisture = NULL, #needed when soil_flux unknown
                            texture = "clay", #needed when soil_flux unknown
                            elev = 270, #climate station caldern
                            lat = 8.683303333333333, #climate station caldern
                            lon = 50.84050277777778 #climate station caldern
                            ){
  datetime <- data[,datetime]
  t1 <- as.numeric(data[,t1])
  t2 <- as.numeric(data[,t2])
  v1 <- as.numeric(data[,v1])
  v2 <- as.numeric(data[,v2])
  rad_bal <- as.numeric(data[,rad_bal])
  sw_bal <- as.numeric(data[,sw_bal])
  lw_bal <- as.numeric(data[,lw_bal])
  hum1 <- as.numeric(data[,hum1])
  hum2 <- as.numeric(data[,hum2])

  # calculation of soil_flux, if unknown
  if(is.null(soil_flux)){
    #Check for missing values
    if(is.null(moisture) | is.null(texture) | is.null(ts1) | is.null(ts2) |
       is.null(depth1) | is.null(depth2)){
      stop("moisture, texture, ts1, ts2, depth1 and depth2 need to be set if soil_flux is NULL.")
    }
    ts1 <- as.numeric(data[,ts1])
    ts2 <- as.numeric(data[,ts2])
    moisture <- as.numeric(data[,moisture])
    thermal_cond <- soil_thermal_cond(moisture, texture)
    soil_flux <- soil_heat_flux(ts1, ts2, depth1, depth2, thermal_cond)
  } else {
    soil_flux <- as.numeric(data[,soil_flux])
  }

  if(!is.null(albedo)){
    albedo <- data[,albedo]
  }

  #pressure
  if (is.null(p)){
    p1 <- pres_p((elev+z1),t1)
  } else {
    p <- data[,p]
    p1 <- p
  }
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
  monin <- turb_flux_monin(grad_rich_no,z1,z2,z0,v1,v2,t1,t2)

  #exchange quotient
  ex_quotient <- turb_flux_ex_quotient_imp(grad_rich_no,ustar,monin,z1,air_density)

  #turbulent impulse exchange ##in Ausgabe
  turb_flux <- turb_flux_imp_exchange(ex_quotient,v1,v2,z1,z2)

  if(is.null(rad_bal)){
    ### Caclulation of radiances
    #calculation of radiation balance, if unknown
    rad_sw_toa <- rad_sw_toa(datetime,lat,lon)
    sol_elevation <- sol_elevation(datetime,lat,lon)
    trans_total <- trans_total(sol_elevation,t1,elev,p = p)
    rad_sw_ground_horizontal <- rad_sw_ground_horizontal(rad_sw_toa, trans_total$total)
    rad_sw_reflected <- rad_sw_reflected(rad_sw_ground_horizontal, albedo)
    sol_azimuth <- sol_azimuth(datetime,lat,lon)
    sw_bal <- rad_sw_radiation_balance(rad_sw_ground_horizontal,rad_sw_reflected)
    emissivity_surface <- surface_properties[which(as.character(surface_properties$surface_type)==surface_type),]$emissivity
    rad_lw_surface <- rad_lw_surface(t1,emissivity_surface)
    emissivity_air <- rad_emissivity_air(t1,elev,p1)
    rad_lw_atmospheric <- rad_lw_atmospheric(emissivity_air,t1)
    lw_bal <- rad_lw_surface - rad_lw_atmospheric
    rad_bal <- rad_bal_total(sw_bal,rad_lw_surface,rad_lw_atmospheric)
  }

  #total radiation balance with topography
  if(!is.null(slope)){
    rad_sw_balance_topography <- rad_sw_balance_topography(slope,valley,sol_elevation,sol_azimuth,exposition = 0,rad_sw_ground_horizontal,albedo)
    terr_sky_view <- terr_sky_view(slope,valley)
    rad_bal_total_with_topography <- rad_bal_total_with_topography(rad_sw_balance_topography, rad_lw_surface,rad_lw_atmospheric,terr_sky_view)
  } else if(is.null(slope)){
    rad_bal_total_with_topography <- NULL
  }

  #Latent Heat Priestley-Taylor Method
  latent_priestley_taylor <- latent_priestley_taylor(t1,rad_bal,soil_flux)

  #Latent Heat Penman Method
  latent_penman <- latent_penman(datetime,v1,t1,hum1,z1,rad_bal,elev,lat,lon)

  #Latent Heat using Monin-Obukhov length
  latent_monin <- latent_monin(hum1,hum2,t1,t2,p1,p2,z1,z2,monin,ustar,grad_rich_no)

  #Latent Heat using Bowen Method
  latent_bowen <- latent_bowen(t1,t2,hum1,hum2,p1,p2,z1,z2,rad_bal,soil_flux)

  #Sensible Heat Priestley-Taylor Method
  sensible_priestley_taylor <- sensible_priestley_taylor(t1,rad_bal,soil_flux)

  #Sensible Heat using Monin-Obukhov length
  sensible_monin <- sensible_monin(t1,t2,p1,p2,z1,z2,monin,ustar,grad_rich_no)

  #Sensible Heat using Bowen Method
  sensible_bowen <- sensible_bowen(t1,t2,hum1,hum2,p1,p2,z1,z2,rad_bal,soil_flux)

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
                    sw_radiation_balance = sw_bal,
                    lw_radiation_balance = lw_bal,
                    total_radiation_balance = rad_bal,
                    turbulent_flux = turb_flux,
                    sensible_heat_priestly_taylor = sensible_priestley_taylor,
                    latent_heat_priestly_taylor = latent_priestley_taylor,
                    latent_heat_penman = latent_penman,
                    sensible_heat_bowen = sensible_bowen,
                    latent_heat_bowen = latent_bowen,
                    sensible_heat_monin = sensible_monin,
                    latent_heat_monin = latent_monin
                    )
  colnames(out) <- c("datetime",
                     "height_lower[m]",
                     "height_upper[m]",
                     "temperature_lower[C°]",
                     "temperature_upper[C°]",
                     "wind_speed_lower[m/s]",
                     "wind_speed_upper[m/s]",
                     "pressure_lower[hPa]",
                     "pressure_upper[hPa]",
                     "humidity_lower[%]",
                     "humidity_upper[%]",
                     "atmospheric_stability",
                     "soil_flux[W/m²]",
                     "shortwave_radiation_balance[W/m²]",
                     "longwave_radiation_balance[W/m²]",
                     "total_radiation_balance[W/m²]",
                     "turbulent_flux[kg/(m*s)]",
                     "sensible_heat[W/m^2]_Priestly-Taylor",
                     "latent_heat[W/m^2]_Priestly-Taylor",
                     "latent_heat[W/m^2]_Penman",
                     "sensible_heat[W/m^2]_Bowen",
                     "latent_heat[W/m^2]_Bowen",
                     "sensible_heat[W/m^2]_Monin",
                     "latent_heat[W/m^2]_Monin")
  return(out)
}



