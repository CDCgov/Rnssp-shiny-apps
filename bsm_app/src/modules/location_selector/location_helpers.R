# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

################### helpers ###########################


# Checks if a set of geoids is connected
selection_is_connected <- function(selected, adj_mat) {
  submat <- adj_mat[selected, selected]
  is_connected(
    graph_from_adjacency_matrix(submat,"undirected",diag = FALSE)
  )
}

# This is a little function that pulls the counties and geometries from
# Rnssp package, and adds the <ST_> prefix in front of all county names
create_us_sf <- function() {
  # pull the state fips and abbrev
  st_sf <- Rnssp::state_sf |> dplyr::as_tibble() |> dplyr::select(STATEFP, STUSPS)
  # join on the county_sf built into the package
  Rnssp::county_sf |> 
    dplyr::inner_join(st_sf, by="STATEFP") |> 
    dplyr::mutate(NAME = paste0(STUSPS, "_", NAME)) |> 
    # make valid and transform
    sf::st_make_valid() |> 
    sf::st_transform(crs = 4326)
}