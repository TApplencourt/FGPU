test.exe: struct_pointer_target_map.f90
	$(FC) $(FFLAGS) $^ -o $@ 

run_test: test.exe
	./test.exe

run: run_test

clean:
	rm -f *.o *.md *.mod test_* *__genmod.f90 *.modmic *.exe
