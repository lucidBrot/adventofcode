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

Reading Info in: I guess I can just copy-paste the whole input into one cell by pressing <kbd>F2</kbd>,<kbd>CTRL</kbd><kbd>V</kbd>

### Algo

Given any asteroid pair, I can compute a line from their locations.

1. Collect all lines for one station candidate
2. Count them for every asteroid location by checking whether that asteroid lies on that line
3. Every time a line is collected, it could be any of:
   * Only two asteroids lie on that line. It counts one score for each of them, plus itself. So score two.
   * More than two asteroids lie on that line. Each can only see itself plus at most two others on the same line, but maybe also only one plus itself. So need to somehow sort them on the line ...
     * I can choose any point on the line as the line-anchor and subtract it from every asteroid on the line to get a directional vector. Projecting that one onto the line direction by a scalar product should give a sortable signed value. Then only the largest and lowest get score two and the others get score three.

Now a straightforward approach would be to compute that multiple times: One time per station candidate. That is $\mathcal O(n^2)$.

But maybe I can do it faster by computing all lines going through each meteor using some hough-transform approach. If I can quickly get a heatmap where every meteor has a count of lines going through it, I just need to filter those lines based on count. I.e. I would have a small amount of lines to consider and all would give at least one count, so that second step would be worth it.