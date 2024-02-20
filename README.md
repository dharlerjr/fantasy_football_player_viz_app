# Fantasy Football: Player Visualization App

[Shiny App Link](https://david-harler-jr.shinyapps.io/player-viz-app/)

## Purpose

After the 2023 fantasy football season concluded, I craved a resource that allowed me to quickly visualize player data and see how various players performed throughout the season. I wanted quick answers to questions such as...

- Did Justin Jefferson pay off his average draft position (ADP)?
- When did Rashee Rice's breakout begin?
- If Tank Dell hadn't gotten hurt, where would he have finished in the final wide receiver rankings?
- Who was the better running back to draft in round 3: Rhamondre Stevenson or Travis Etienne Jr.?

With these questions and many more in mind, I set out to create the Fantasy Football Player Visualization App, which utilizes R Shiny and shinyapps.io to make the app interactive and easily accessible via the internet. This app includes many features such as a line chart of player scores by week, a table of summary statistics displaying stats such as a player's floor and ceiling, and a summary of a player's season overall. Furthermore, the app includes two main pages: one to analyze a single player, and one to compare two players at a time. After completion, there was no question I had which was left unanswered, and, I had a tool to help give me an edge in many more fantasy football seasons to come.

## Results

Here are two screenshots of the Player Analysis Page upon startup.

![App Startup 1](https://github.com/dharlerjr/fantasy_football_player_viz_app/blob/main/Images/AppStartup1.PNG)

![App Startup 2](https://github.com/dharlerjr/fantasy_football_player_viz_app/blob/main/Images/AppStartup2.PNG)

Here are two screenshots of the Player Comparison Page upon startup.

![App Startup 3](https://github.com/dharlerjr/fantasy_football_player_viz_app/blob/main/Images/AppStartup3.PNG)

![App Startup 4](https://github.com/dharlerjr/fantasy_football_player_viz_app/blob/main/Images/AppStartup4.PNG)

Here is a screenshot displaying fantasy football statistics for Ja'Marr Chase.

![Ja'Marr Chase](https://github.com/dharlerjr/fantasy_football_player_viz_app/blob/main/Images/JamarrChase.PNG)

Here is a screenshot comparing fantasy football statistics between Jordan Love and Dak Prescott.

![Jordan Love vs. Dak Prescott](https://github.com/dharlerjr/fantasy_football_player_viz_app/blob/main/Images/JordanLoveVsDakPrescott.PNG)

## Sample Insights

1. De'Von Achane scored 51.3 PPR points in week 3 against the Denver Broncos. However, despite this single-week performance which led all running backs, Achane only finished as the RB24.
2. Rookie wide receiver Tank Dell finished as the WR38. He had a high weekly median score of 17.2 PPR points. His low finish is largely due to the leg injury that he suffered in week 13, which caused him to miss the rest of the season.
3. Lamar Jackson and Brock Purdy both finished as QB1s. Purdy, however, proved to be the more efficient investment for fantasy managers during draft season, as Lamar costed a 3rd round pick, while Purdy largely went undrafted.

## Improvements

Overall, I am very proud of how this application turned out. However, there is one major weakness: when searching for a player, currently, the user must input the player's name exactly as it is saved in the dataset. In other words, when searching for a player, the user cannot make any typos, the player's name must be capitalized correctly, and punctuation must be matched perfectly. While building the app, I brainstormed two different solutions to this major issue. First, I hoped to write a player search function to allow the user to search for a player as if they were searching for a player on Google. However, this approach proved to be very challenging, as in our app, the user could potentially only know the player's first name, only know the player's last name, or even misspell the player's name entirely. It seemed as if I would have to build my own search engine from scratch, and I decided that this task far exceeded the scope of the project. My second idea was to remove the text input widget and instead use either one or three select boxes. Using one select box proved unrealistic, as displaying all 596 players in a single list would be overwhelming for the user. Using three select boxes then semeed to be the next best idea. I would use one select box for the team and another for the position. The third select box would then display only players fitting the first two criteria. For instance, if the user selected wide receivers from the Cincinatti Bengals, the third select box would list off Ja'Marr Chase, Tee Higgins, Tyler Boyd, and only a few more players to choose from. While this option seemed feasible, it too proved impractical, as Shiny did not easily allow me to update the values in the third text box based on the user's input in the previous two. Instead, the select box options had to be set-in-stone before even launching the app. Therefore, I stuck to my original design, in hopes that users would be able to lookup and learn the spellings of their favorite NFL players' names.

## More Notes

- The data I used was retrieved from FantasyPros.com. However, I'm sure plenty of resources exist online for retrieving fantasy football data.
- Team logos and wordmarks are courtesy of the nflfastR team. These logos and wordmarks are by far my favorite feature, and I'm glad I able to figure out how to render them on my app.