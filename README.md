# pavlok-morsecode
 A library to send text and numbers in the form of Pavlok stimuli


`python3 -m pip install morse_talk`
`python3 -m flask run` #defaults to running app.py
Then, navigate to the server url: /send_morse?token=[TOKEN]&input=[TEXT_TO_SEND_AS_MORE]. 
	*eg http://localhost:5000/send_morse?token=d8931902e8df215d9f8f483d47a512ca0eec04d771e61a4fe70057684da2e7d8&input=SOS*


HOW IT WORKS AND MY PROBLEMS
The server uses the token to convert the input text into morse code, and uses these commands to return a payload

	x = parser.parse(newstr)
	y = parser.payload(x);
	payload = "\n" + (y['payload']) 	

The returned and printed JSON includes debugger text:

newstr: the morse code as vibe: delay: language.
subtitle: the resulting payload from the lines above.

When I copy/paste a payload and hardcode it into this file, it fires properly.

When I use the one I generate it doesn’t.

My request: how do I fix app.py to properly generate a Payload which will arrive to the Pavlok device?
 