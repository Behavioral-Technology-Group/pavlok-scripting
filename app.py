#!/usr/bin/env python3

#run server from directory using `python3 -m flask run`
#Navigate to /send_morse?token=[TOKEN]&input=[TEXT_TO_SEND_AS_MORE].
#	eg http://localhost:5000/send_morse?token=d8931902e8df215d9f8f483d47a512ca0eec04d771e61a4fe70057684da2e7d8&input=SOS

# import urllib.parse

import pbc
import os

parser = pbc.ScriptParser()
grammar = pbc.ScriptGrammar()


try:
    import morse_talk as morse
    from flask import request
    import requests

    from flask import Flask
    app = Flask(__name__)

except ImportError:
    if __name__ != '__main__':  # regular failure if running in flask
        raise

    # otherwise allow "python app.py" to run local test code at end
    print('Dependencies missing, fake it for test purposes:\n')
    class app:
        route = lambda *x, **y: (lambda z: z)


def compile(source):
    res = parser.api(pb=True, seq='time', source=source)
    return '\n' + res['payload']


def get_numeric_urlparam_or_default(param,default_value):
    if (request.args.get(param) is not None and request.args.get(param).isnumeric() and int(request.args.get(param)) > 0):
        return str(request.args.get(param))
    else:
        return str(default_value)


@app.route('/send_morse', methods = ['GET','POST'])
def send_morse():
        morse_str=input=request.args.get('input')
        reason = request.args.get('reason')
        if (reason is None):
            reason = "No reason given."
        morse_str = morse.encode(morse_str)
        if (morse_str is None):
            morse_str = morse.encode('SOS')
        stimtype = request.args.get('stimtype')
        if stimtype == "vibrate":
            stimtype = "vibe"
        if stimtype not in ["beep", "vibe"]:
            stimtype='beep'

        dot_t_on = get_numeric_urlparam_or_default('dot_t_on',100)
        dash_t_on = get_numeric_urlparam_or_default('dash_t_on',500)
        char_delay = get_numeric_urlparam_or_default('char_delay',100)
        word_delay = get_numeric_urlparam_or_default('word_delay',500) 
        level = get_numeric_urlparam_or_default('level',100)

        newstr=morse_str.replace('   ', "delay: {};".format(word_delay)).replace('.',"{}: level {}, t_on {}; delay: {};".format(stimtype,level,dot_t_on,char_delay)).replace('-',"{}: level {}, t_on {}; delay: {};".format(stimtype,level,dash_t_on,char_delay))
        
#	newstr=str.replace('   ', "delay: 500;\n").replace('.',"vibe: level 100, t_on 100;\ndelay: 100;\n").replace('-',"vibe: level 100, t_on 500\ndelay: 100;\n");

        token = request.args.get('token')

        payload = compile(newstr)
        #payload = "\n" + "PBf-RgN!3xbT]-+A$0+qBA0%8{m0}=-#0rM^mwch<y0%nc10rAmj1y}Pnwch]a1{1-awcuAy" #(y['payload']);

        headers = {"Authorization": "Bearer {}".format(token)}

        json_str = {"title": "🆘New Morse Message: {}".format(input),"reason": "Here's why: " + reason, "subtitle": payload, "headers": headers, "newstr": newstr}
        r = requests.post('https://app.pavlok.com/api/v3/stimulus/script',headers=headers, data=json_str)

        json_str["usage_notes"]="\n\nHere are the parameters you can include, and their defaults: \n" + "dot_t_on: 100;  dash_t_on: 500;  char_delay: 100;  word_delay: 500;  level: 100"
        return json_str 


@app.route('/')
def hello_world():
	return "Try navigating to this URL: <a href=\"/send_morse?token=[TOKEN]&input=[TEXT]\">/send_morse?token=[TOKEN]&input=[TEXT]</a>."
        # str = morse.encode(request.args.get('input'));
        # newstr=str.replace('   ', "delay: 500\n").replace('.',"vibe: level 100, t_on 100\ndelay: 100;\n").replace('-',"vibe: level 100, t_on 500\ndelay: 107;\n");


        # x= parser.parse(newstr)#("vibe: level 100, t_on 100\ndelay: 100;\n");
        # #b'\x03\x02\xf5\x06\x01\x01\x02d\x04d\x03\x01d\x03\x02\xf5\x06\x01\x01\x02d\x04d\x03\x01d\x03\x02\xf5\x06\x01\x01\x02d\x04d\x03\x01d\x03\x01\x81t\x03\x02\xf5\x07\x01\x01\x02d\x04\x81t\x03\x01k\x03\x02\xf5\x07\x01\x01\x02d\x04\x81t\x03\x01k\x03\x02\xf5\x07\x01\x01\x02d\x04\x81t\x03\x01k\x03\x01\x81t\x03\x02\xf5\x06\x01\x01\x02d\x04d\x03\x01d\x03\x02\xf5\x06\x01\x01\x02d\x04d\x03\x01d\x03\x02\xf5\x06\x01\x01\x02d\x04d\x03\x01d'
	# y=parser.payload(x);
	# # /pbc.py", line 1200, in generate
 #    #name = cmd.data[0]
	# #AttributeError: 'bytes' object has no attribute 'data'
	# payload = (y['payload']);

	# headers = {"Authorization": "Bearer 81a684bae27d32930697a9f22e0d81e8f8981a198e53f60443c5dae90ee9537a"}



	# r = requests.post('https://app.pavlok.com/api/v3/stimulus/script',headers=headers, data = {'token':'81a684bae27d32930697a9f22e0d81e8f8981a198e53f60443c5dae90ee9537a', "title": "Igor", "reason": "sending push to Geoff", "subtitle": "\nPBr@-j2sYCLc[kCAv[fO/86b85{69wss0$=gW6CVfj69wsI6bhe{6bhe{6bhe{6b7@>6bhgj6b82)5d?tGm&1Jemhegyh8H146/gL3g>^?BG/7Yv6+@JUh2)QR"})
	# r.json()

	# return payload + '<p>' + newstr


# for local testing: run as "python app.py"
if __name__ == '__main__':
    port = int(os.getenv("PORT", 8080))
    app.run(host='0.0.0.0', port=port)
    #newstr = '...   ---   ...'.replace('   ', "delay: 500;").replace('.',"beep: level 100, t_on 100; delay: 100;").replace('-',"beep: level 100, t_on 500; delay: 100;")
    #print(repr(compile(newstr)))
