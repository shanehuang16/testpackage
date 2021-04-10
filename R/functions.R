#' Event Tally - Count the frequency of a defensive basketball event in a game
#' @param event name of the game event to track
#' @param name name of the player to track
#' @return numerical count of the frequency of the event by the player
#' @export

tally <- function(event, name){

  sub1 <- gm_log %>% filter(possession == 'away') # Pull opposite team's events for defensive metrics
  id1 <- sapply(sub1$home_names, FUN=function(x){name %in% x})

  sub2 <- gm_log %>% filter(possession == 'home')
  id2 <- sapply(sub2$away_names, FUN=function(x){name %in% x})

  sub <- rbind(sub1[id1,],sub2[id2,])

  count <- sum(sub$event == event)

  return(count)
}
