// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/tokenizer.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
StringVector Boost_Tokenizer(const StringVector strings) {
    std::vector<std::string> tokens;

    for (unsigned int index = 0; index < strings.size(); index++) {
        std::string str = std::string(strings(index));
	typedef boost::tokenizer<boost::char_separator<char> > tokenizer;
	boost::char_separator<char> sep(" \f\n\r\t\v");
	tokenizer tok(str, sep);
        for (tokenizer::iterator it = tok.begin(); 
	     it != tok.end();
             ++it) {
            tokens.push_back(*it);
	}
    }

    return wrap(tokens);
}
