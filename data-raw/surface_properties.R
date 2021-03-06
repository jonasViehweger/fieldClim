## code to prepare `surface_properties` dataset goes here

surface_properties <- data.frame(surface_type = c("Meadow",
                                                  "Field",
                                                  "Park/Lawn",
                                                  "Street",
                                                  "Agriculture",
                                                  "Settlement",
                                                  "Coniferous",
                                                  "Broadleaf",
                                                  "Mixed",
                                                  "City"),
                                 emissivity = c(0.92,
                                                0.98,
                                                0.95,
                                                0.95,
                                                0.95,
                                                0.8,
                                                0.98,
                                                0.98,
                                                0.98,
                                                0.9),
                                 roughness_length = c(0.02,
                                                      0.05,
                                                      0.2,
                                                      0.2,
                                                      0.2,
                                                      1.0,
                                                      1.0,
                                                      1.5,
                                                      1.5,
                                                      2.0)
)

usethis::use_data(surface_properties, overwrite = T, internal = T)
