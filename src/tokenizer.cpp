// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/tokenizer.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
StringVector Boost_Tokenizer(const StringVector strings) {
    std::vector<std::string> tokens;

    for (unsigned int index = 0; index < strings.size(); index++) {
        std::string s = std::string(strings(index));
        boost::tokenizer<> tok(s);

        for (boost::tokenizer<>::iterator it = tok.begin();
             it != tok.end();
             ++it)
            tokens.push_back(*it);
    }

    return wrap(tokens);
}
