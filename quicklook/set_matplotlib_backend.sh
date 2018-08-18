#!/bin/bash
#Exit immediately if any command returns a non zero exit status:
set -e

#Use Agg backend instead of Xwindows to remove the need for a display:
mkdir -p /root/.config/matplotlib
touch /root/.config/matplotlib/matplotlibrc
echo 'backend : Agg' > /root/.config/matplotlib/matplotlibrc
