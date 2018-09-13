## Import individual broken UCL Player Locations
importBrokenUCLLocs <- function(){
  uclPlayers$Location[123] <<- "Buenos Aires, Argentina" # Javier Zanetti
  uclPlayers$Location[230] <<- "Samtredia, Georgia" # Kakha Kaladze
  uclPlayers$Location[288] <<- "Águilas, Murcia" # Jose Manuel Meca Garcia
  uclPlayers$Location[302] <<- "Leytonstone, London, England" # David Beckham
  uclPlayers$Location[329] <<- "Palma de Mallorca, Spain" # Raul Pareja
  uclPlayers$Location[502] <<- "Vila Do Conde, Portugal" # Joao Festas
  uclPlayers$Location[525] <<- "Joeuf, France" # Michel Platini
  uclPlayers$Location[531] <<- "Bydgoszcz, Poland" # Zbigniew Boniek
  uclPlayers$Location[569] <<- "Wolverhampton, England" # Gary Williams
  uclPlayers$Location[577] <<- "Skegness, Lincolnshire, England" # Ray Clemence
  uclPlayers$Location[581] <<- "Liverpool, England" # Jimmy Case
  uclPlayers$Location[589] <<- "Clifton, Nottingham, England" # Viv Anderson
  uclPlayers$Location[606] <<- "Barrow-in-Furness,England" # Emyln Hughes 
  uclPlayers$Location[608] <<- "Dublin, Ireland" # Steve Heighway
  uclPlayers$Location[621] <<- "Munich, Germany" # Hugo Robl ((unconfirmed))
  uclPlayers$Location[669] <<- "Caselle Lurani, Italy" # Giovanni Lodetti
  uclPlayers$Location[680] <<- "Glenboig, North Lanarkshire, Scotland" # Francis Burns
  uclPlayers$Location[706] <<- "Cordoba, Spain" # Ramon Tejeda
  uclPlayers$Location[708] <<- "Madrid, Spain" # Pedro De Felipe
  uclPlayers$Location[748] <<- "	Barreiro, Portugal" # Jose Augusto Pinto de Almeida
  uclPlayers$Location[760] <<- "Las Palmas, Spain" # Manuel Quevedo Vernetta
  uclPlayers$Location[766] <<- "Salamanca, Spain" # Miche (Miguel García Martín)
  uclPlayers$Location[771] <<- "Teruel, Spain" # Manuel Torres Pastor
  uclPlayers$Location[772] <<- "Deba, Guipuzcoa, Spain" # Javier Berasaluce Marquiegui
  uclPlayers$Location[775] <<- "Ksar-el-Kebir" # Heliodoro Castaño Pedrosa
  uclPlayers$Location[777] <<- "Alcoy, Spain" # Jose Luis Perez Paya
  uclPlayers$Location[779] <<- "Gavà, Barcelona, Spain" # Joaquin Navarro Perona
  uclPlayers$Location[780] <<- "Guissona, Spain" # Joaquin Oliva
  uclPlayers$Location[783] <<- "Madrid, Spain" # Jose Becerril Minguela
  uclPlayers$Location[784] <<- "Madrid, Spain" # Ramon Marsal
}

importBrokenUCLCoords <- function(){
  uclCoords[251, ] <<- data.frame(lat = 37.4119, lon = 1.5826)
  uclCoords[377, ] <<- data.frame(lat = 42.0912, lon = 19.0899)
  uclCoords[616, ] <<- data.frame(lat = 41.2988, lon = 1.9930)
}
