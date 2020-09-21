# using convert through Python
import subprocess
import os
import glob
import moviepy.editor as mpy


os.chdir("./plots/")
print(os.getcwd())

gif_name = 'outputName'
fps = 8
file_list = glob.glob('*.jpg') # Get all the pngs in the current directory
list.sort(file_list, key=lambda x: (x[0].split('.jpg')[0])) # Sort the images by #, this may need to be tweaked for your use case
clip = mpy.ImageSequenceClip(file_list, fps=fps)
clip.write_gif('{}.gif'.format(gif_name), fps=fps)