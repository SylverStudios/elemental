
# Elemental Game Idea

* Summon runes each turn (a lot like "drawing a hand")
* Combine runes into a spell to cast (like "playing a hand", but order matters)
* Resolve effects
* Repeat

## Run me!
```
# Install stuff, maybe add instructions
npx elm-watch hot
open index.html

npm install http-server -g
open http://127.0.0.1:8080
```

## Notes 8/16/2024

* Took 1 hour to get mise and vscode working together to use Elm
* Next up - focus on "Game" mode
  * Design simple UI for interaction, world view on right
  * Track turns and history (maybe not history at first?)
  * Select Runes, cast, start entirely fresh with original runes + effects
  * Round is over when Life is > X
  * Next round starts where last ended, seemless new objective after recieving reward