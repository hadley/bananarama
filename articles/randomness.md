# Randomness and seeds

Image generation is inherently stochastic: the same prompt can produce
visibly different results each time. bananarama lets you control this
with two YAML fields:

- **`n`**: generate multiple images from the same prompt.
- **`seed`**: pin the random seed for more reproducible output.

## No seed

Without a seed, each image is generated independently and you’ll see
meaningful variation in composition, color, and detail.

![](randomness/no-seed-1.png)

![](randomness/no-seed-2.png)

![](randomness/no-seed-3.png)

![](randomness/no-seed-4.png)

![](randomness/no-seed-5.png)

![](randomness/no-seed-6.png)

## Setting a seed

Setting a seed encourages the model to produce more consistent output.
There’s much less variation between images compared to the unseeded set,
event though results are not perfectly identical.

![](randomness/seed-a-1.png)

![](randomness/seed-a-2.png)

![](randomness/seed-a-3.png)

![](randomness/seed-a-4.png)

![](randomness/seed-a-5.png)

![](randomness/seed-a-6.png)

A different seed value produces a different but again more consistent
set of images. Comparing this set with seed A shows that the seed
affects the specific output, not just the consistency.

![](randomness/seed-b-1.png)

![](randomness/seed-b-2.png)

![](randomness/seed-b-3.png)

![](randomness/seed-b-4.png)

![](randomness/seed-b-5.png)

![](randomness/seed-b-6.png)

Unfortunately there’s no way to determine what seed gemini uses if you
don’t specify one, so you can’t reproduce unseeded results. If you want
to be able to reproduce a specific image, you have to set a seed from
the start.
