#!/bin/bash

# In the future, it might be kind of cool to auto-populate metadata - ffmpeg -i out.mp3 -i coverart.png -map 0:0 -map 1:0 -c copy -id3v2_version 3 metadata:s:v title="Album cover" -metadata:s:v comment="Cover (Front)" out.mp3

for i in $1/*; do
	basename=${i%.mp4}
	ffmpeg -i $basename.mp4 -vn -acodec libmp3lame -ac 2 -ab 160k -ar 48000 $basename.mp3
done

