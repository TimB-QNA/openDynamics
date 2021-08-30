docName = document

all: pdf

pdf: clean $(docName).tex
	@echo "Running unit tests for all components" ;
	@cd ../../src/unitTests/	&& qmake unitTests.pro ;
	@cd ../../src/unitTests/	&& $(MAKE) check ;
	@echo "Rerun outline" > $(docName).log
	@latex_count=1 ; \
	while egrep -s 'Rerun.*outline|Rerun.*bib|Rerun.*index' $(docName).log >/dev/null && [ $$latex_count -lt 17 ] ;\
	    do \
	      echo "Running latex: Pass $$latex_count..." ;\
	      pdflatex -shell-escape $(docName) > /dev/null;\
	      bibtex $(docName).aux > /dev/null;\
	      makeindex $(docName).idx > /dev/null 2>/dev/null;\
	      latex_count=`expr $$latex_count + 1` ;\
	    done

clean:
	@rm -f *.ps *.dvi *.aux *.toc *.idx *.ind *.ilg *.log *.out *.brf *.blg *.bbl $(docName).pdf
