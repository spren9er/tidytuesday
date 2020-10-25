# #TidyTuesday

## What is it all about?

Excerpt from the official [tidytuesday](https://github.com/rfordatascience/tidytuesday) project on GitHub:

The #tidytuesday project is a weekly data project aimed at the `R` ecosystem. An emphasis will be placed on understanding how to summarize and arrange data to make meaningful charts with `ggplot2`, `tidyr`, `dplyr`, and other tools in the `tidyverse` ecosystem.

## Code

This repository is a collection of some code snippets used for wrangling, aggregating and visualizing data with `tidyverse` to create a meaningful insight about the underlying weekly data sets.

**Important** _Sometimes, code is minimalized and its focus is to show how data is prepared, aggregated and visualized (without distraction of styling and modifying theme, fonts, etc.) and charts in the `images` folder are created with the code shown here with additional theme modifications._

## Animations

In order to submit an animation to twitter, one has to convert it to a `gif` or movie file.
On twitter, _gif_ files look often better than movie files.
Additionally, one can pause animation by tapping on `gif` animation.

For detailed twitter specifications on image and movie files, see [here](https://developer.twitter.com/en/docs/twitter-api/v1/media/upload-media/uploading-media/media-best-practices). 

### Installation

Install `ffmpeg` and `gifski`, e.g. in _macOS_ via `brew` 

```
brew install ffmpeg
brew install gifski
```

### Video file conversion (to .gif)

In order to get a high resolution `gif` file, use `gifski`.
On _macOS_:

```
ffmpeg -y -i input.mov -vf "setpts=0.5*PTS" frames/frame%04d.png
gifski -o output.gif frames/frame*.png
```

Change `fps` and `width`

```
gifski --fps 1 -W 1080 -o output.gif frames/frame*.png
```

### Video file conversion (to .mp4)

```
ffmpeg -y -i input.mov -vf "setpts=0.5*PTS,scale=-1:1080" -r 40000/1001 output.mp4
```