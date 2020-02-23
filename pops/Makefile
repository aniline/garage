
EXE=pops
OBJS=main.o lmap.o

CXX=g++
LD=$(CXX)

CXXFLAGS=-Wall -c -std=c++11 $(shell sdl2-config --cflags)
LIBS=$(shell sdl2-config --libs)
LDFLAGS=$(LIBS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(EXE): $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $^

.PHONY: clean
clean:
	rm -f $(OBJS) $(EXE)
