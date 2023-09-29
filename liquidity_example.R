require(tidyverse)

total_wealth <- tibble(
  player_number = 0:9, overall_wealth = 0
)

some_random <- tibble(
  stock_number = 1:10, change = rnorm(10, mean = 1, sd = 0.02)
)

for (round in 1:10000){
  this_round_stats <- tibble(
    player_number = 0:9, stock_number = 0, wealth = 0
  )
  
  stock_price <- tibble(
    stock_number = 1:10, price = 100
  )

  for (time in 1:20){
    stock_price <- stock_price |>
      mutate(change = rnorm(10, mean = 1.0005, sd = 0.02)) |>
      arrange(-change)
    
    buyers <- this_round_stats |>
      filter(stock_number == 0, player_number < (stock_price$change[1] -1) * 100) |>
      mutate(stock_number = stock_price$stock_number[1], wealth = -stock_price$price[1])
    
    this_round_stats <- anti_join(this_round_stats, buyers, by = "player_number") |>
      bind_rows(buyers) |>
      arrange(player_number)
    
    stock_price <- stock_price |>
      mutate(price = price * change)
  }

  stock_price <- stock_price |>
    add_row(stock_number = 0, price = 0)
  
  this_round_stats <- left_join(this_round_stats, stock_price, by = "stock_number") |>
    mutate(wealth = wealth + price)
    
  total_wealth <- total_wealth |>
    left_join(this_round_stats, by = "player_number") |>
    mutate(overall_wealth = overall_wealth + wealth) |>
    select(player_number, overall_wealth)
}


