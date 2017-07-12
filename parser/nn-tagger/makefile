COMPILER=g++ -std=c++11
INCLUDES=-I${DYNET} -I${EIGEN}
LIBS=-L${DYNET}/build/dynet -L${BOOST_LIBRARYDIR}

spine-tagger: spine-tagger.cc ExternalSpines.pb.cc
	${COMPILER} ${INCLUDES} ${LIBS} $^ -lprotobuf -lboost_serialization -ldynet -Wl,-rpath,${DYNET}/build/dynet -o $@ -g

ExternalSpines.pb.cc: ExternalSpines.proto
	protoc ExternalSpines.proto --cpp_out=.

clean:
	rm spine-tagger *.pb.cc *.pb.h
