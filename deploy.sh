#!/bin/bash
. /home/maneesh/.bashrc
export FLASK_APP=/home/maneesh/Pavlok/pavlok-scripting/app.py
screen -dmS pavlok_scripting /usr/local/bin/flask run --host=0.0.0.0 -p 5020
