CXX=clang++
CXXFLAGS=-Wall -I. -std=c++17
LDFLAGS=-L. -L/nix/store/lcq9ckpy6c6rf25679qb47rkgy4lrl4a-secp256k1-0.5.0 \
        -L/nix/store/bjgbcdi4v0y6hdzic9xl03z6lwnpxxkc-libsodium-1.0.20 \
        -L/nix/store/ckc6chj43pj34xafirv47vvig2zz5awm-tiny_keccak_wrapper
LIBS=-lsodium -lsecp256k1 -ltiny_keccak_wrapper -lc++

all: test_scalus

test_scalus: test_scalus.cpp libscalus.a
	$(CXX) $(CXXFLAGS) -o test_scalus test_scalus.cpp ./libscalus.a $(LIBS)

clean:
	rm -f test_scalus