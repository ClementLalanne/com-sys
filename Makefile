main:
	ocamlc  -thread unix.cma threads.cma graphics.cma images.ml kahn.ml picture_in_picture.ml entiers.ml main.ml

clean:
	rm a.out images.cmi images.cmo kahn.cmi kahn.cmo picture_in_picture.cmi picture_in_picture.cmo main.cmi main.cmo entiers.cmi entiers.cmo

cleansockets:
	rm shes got a smile that it seems to me remind our childhood memories

realclean: clean
	rm -f *~

cleanall: realclean
