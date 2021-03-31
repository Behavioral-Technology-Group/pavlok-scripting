#run server from directory using `python3 -m flask run`
#Navigate to /send_morse?token=[TOKEN]&input=[TEXT_TO_SEND_AS_MORE]. 
#	eg http://localhost:5000/send_morse?token=d8931902e8df215d9f8f483d47a512ca0eec04d771e61a4fe70057684da2e7d8&input=SOS

import pbc;
import morse_talk as morse;
from flask import request
import requests
import urllib.parse

parser = pbc.ScriptParser();
grammar = pbc.ScriptGrammar();



from flask import Flask
app = Flask(__name__)



@app.route('/send_morse', methods = ['GET','POST'])
def send_morse():
	str = morse.encode(request.args.get('input'));
	newstr=str.replace('   ', "delay: 500;").replace('.',"vibe: count 1, level 100, t_on 100; delay: 100;").replace('-',"vibe: count 1, level 100, t_on 500; delay: 100;");
#	newstr=str.replace('   ', "delay: 500;\n").replace('.',"vibe: count 1, level 100, t_on 100;\ndelay: 100;\n").replace('-',"vibe: count 1, level 100, t_on 500\ndelay: 100;\n");

	token = request.args.get('token');

	x = parser.parse(newstr)
	y = parser.payload(x);
	payload = "\n" + (y['payload']) 	
	#payload = "\n" + "PBf-RgN!3xbT]-+A$0+qBA0%8{m0}=-#0rM^mwch<y0%nc10rAmj1y}Pnwch]a1{1-awcuAy" #(y['payload']);

	headers = {"Authorization": "Bearer {}".format(token)}


	json_str = {"title": "Default title", "reason": "Default reason", "subtitle": payload, "headers": headers, "newstr": newstr}
	r = requests.post('https://app.pavlok.com/api/v3/stimulus/script',headers=headers, data=json_str)

	return json_str





@app.route('/')
def hello_world():
	return "Try navigating to this URL: <a href=\"/send_morse?token=[TOKEN]&input=[TEXT]\">/send_morse?token=[TOKEN]&input=[TEXT]</a>."
	# str = morse.encode(request.args.get('input'));
	# newstr=str.replace('   ', "delay: 500\n").replace('.',"vibe: count 1, level 100, t_on 100\ndelay: 100;\n").replace('-',"vibe: count 1, level 100, t_on 500\ndelay: 107;\n");
	

	# x= parser.parse(newstr)#("vibe: count 1, level 100, t_on 100\ndelay: 100;\n");
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