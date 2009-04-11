.SUFFIXES:  .beam .hrl .erl .html .ehtml .fig .gif

.fig.gif:
	fig2dev -L gif $< >$@

XFIG = msg1.fig start_client.fig  start_fail1.fig  start_xml.fig\
       register.fig  start_erl.fig     start_fail2.fig  termination.fig\
	start_client_ubf.fig start_server_ubf.fig

GIFS = ${XFIG:.fig=.gif} 

all: ${BEAM_FILES} 

clean:
	rm -f ${BEAM_FILES} 
