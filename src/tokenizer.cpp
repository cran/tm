// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/tokenizer.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
StringVector Boost_Tokenizer(const StringVector strings) {
    std::vector<std::string> tokens;
    std::vector<unsigned int> places;

    for (unsigned int index = 0; index < strings.size(); index++) {
	if(StringVector::is_na(strings[index])) {
	    places.push_back(tokens.size());
	    tokens.push_back("");
	    continue;
	}
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

    StringVector y = wrap(tokens);
    for(unsigned int i = 0; i < places.size(); i++) {
	y[places[i]] = NA_STRING;
    }
    
    return y;
}
