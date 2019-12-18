compiler = gfortran 
st_atr = $^ -c -g
flags = 
plot: prog plot.gnu
	gnuplot plot.gnu
shot: prog
	time ./prog
prog: main.o non_linear_system.o diffur.o linear_system.o boundary.o non_linear_system_algebraic.o
	$(compiler) $(flags) $^ -o $@
main.o: main.f90 boundary.mod
	$(compiler) $(flags) main.f90 -c -g
boundary.o boundary.mod: boundary.f90 diffur.mod non_linear_system.mod linear_system.mod non_linear_system_algebraic.mod
	$(compiler) $(flags) $(st_atr)
non_linear_system.mod non_linear_system.o: non_linear_system.f90 linear_system.mod diffur.o
	$(compiler) $(flags) $(st_atr)
non_linear_system_algebraic.mod non_linear_system_algebraic.o: non_linear_system_algebraic.f90 linear_system.mod diffur.o
	$(compiler) $(flags) $(st_atr)
linear_system.mod linear_system.o: linear_system.f90
	$(compiler) $(flags) $(st_atr)
diffur.mod diffur.o: diffur.f90
	$(compiler) $(flags) $(st_atr)
clean: 
	rm -f *.o *mod
