
from pygments import highlight
from pygments.lexers import HaskellLexer
from pygments.formatters import HtmlFormatter

formatter = HtmlFormatter(linenos=True, nobackground=True)

def styles():
	css =  "<style type=\"text/css\">\n"
	css += formatter.get_style_defs('.highlight')
	css += "</style>\n"
	return css

def code():
	highlighted = ""
	for module in ["TPF", "Tipos", "Atleta", "Competencia", "JJOO"]:
		highlighted += "<h2>M&oacute;dulo " + module + "</h2>\n"
		f = open(module + ".hs")
		code = f.read()
		f.close()
		highlighted += highlight(code, HaskellLexer(), formatter)
	return highlighted

html =  "<!doctype html>\n"
html += "<html>\n"
html += "<head><title>TPF</title></head>\n"
html += "<body>\n"
html += styles()
html += code()
html += "</body>\n"
html += "</html>\n"

f = open("output.html", "wb+")
f.write(html.encode("utf-8"))
f.close()

print "Done"