ffmpeg -i videofile -r fps -s 640x360 -f image2 outputName-%03d.jpeg

ffmpeg --enable-libzvbi
ffmpeg --enable-libzvbi -txt_format text -i GurrenTestVid.mkv outtest.srt
ffmpeg -i GurrenTestVid.mkv -map 0:s:0 outtest.srt

FrameGrab1
ffmpeg -i GurrenTestVid.mkv -r 1 -s 640x360 -f image2 out-%03d.jpeg

FrameGrab2
ffpeg -ss secondsStart -t secondsRun -i videoFile -r frameRate -s WxL -f image2 outfilename-%03d.jpeg