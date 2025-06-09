CXX=clang
LIBSCALUS_PATH=../../scalus-core/native/target/scala-3.3.6
CXXFLAGS=-Wall -I.
LDFLAGS=-L$(LIBSCALUS_PATH) -L/nix/store/nrwrqyyd91krcxgwriv72j7hgmrkdc83-secp256k1-0.6.0 \
        -L/nix/store/wjzbbmzixi4f9fc551r4041jsjpw4k9d-libsodium-1.0.20 \
        -L/nix/store/yfd282n7c0ygkc1pc6jai2lim6m7l3v7-tiny_keccak_wrapper
LIBS=-lsodium -lsecp256k1 -ltiny_keccak_wrapper -lc++ -lscalus


all: example

example: main.c $(LIBSCALUS_PATH)/libscalus.a
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o example main.c $(LIBS)

clean:
	rm -f example

run: clean all
	./example
