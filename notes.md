Input: map of all asteroids in region, pixels are either an asteroid or not.

Each asteroid has different size? and coordinates

>  with `X,Y` coordinates where `X` is the distance from the left edge and `Y` is the distance from the top edge (so the top-left corner is `0,0` and the position immediately to its right is `1,0`).

Goal: Find location that has line of sight to most asteroids without another one in between them.

guessing from examples:

* Can I see through the asteroid I am on?
  * yes, according to sansero
* Can I build in outer space?
  * no. the best location is an *asteroid* only.

* How do asteroid sizes work?
  * unspecified. Point-sized, I guess.
  * All asteroids are exactly at the center.

## Approach 1

Goal: Find best location (coordinates) and number of visible asteroids from there.