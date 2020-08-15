Code meant to explore methods to extract and detect elliptical features from video and create metrics on their change over time.  This project will combine ellipse detection scripts, 


The working parts of this project currently will are:

EllipseAlgo1: An unused exploration of Hough methods of detecting ellipse used to build useful test functions for the project.  Hough methodology is ultimately too slow for our video needs, even with radical amounts of preprocessing of images.  It still usually involves iterating over too many pixel pairs. Method based on Xie-Ji semi-majoraxis voting scheme, extended to be agnostic of which axis it has selected

EllipseAlgo2: A faster algorithm for detecting ellipse using edge angle data and the arrangement of nearby edges to winnow the candidate pixels to consider before voting on ellipse measures.  Based on Jia-Fan et al.

ImgProcess1: A collection of different image processing files and results from preprocessing images.

Frame Selection1: Functionality to extract frames from a video file to analyze to look for ellipses fitting criteria of interest- a first pass over the video file.

Frame Selection2: After detecting ellipses with the desired architecture, this pulls more frames around the detected ellipes frame to analyze the evolution of the ellipses over time.

TDE Measure:  Applies ellipse recognition to frames found in Frame Selection 2 and creates metrics for the change in ellipses over time.

TDE Machine:  An attempt at a machine-learning based approach to apply the above libraries to recognize relevant ellipses rapidly and accurate by tweaking the following metrics
	From EllipseAlgo:
	From ImgProcess:

Sub Selection:  Grabs text from .sub file at times concurrent to the times from FrameSelcection2

Sub Analysis:  Creates metrics about the text from the Sub Selection Stage, and similar metrics about the entire .sub file it contains.

Presentation:  Graphs the TDE Measures with aspects of the Sub Analysis