// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]

#include <Rcpp.h>
#include <boost/tokenizer.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
List tdm(const StringVector strings,
         const bool remove_digits,
         const std::vector<std::string> stopwords,
         const std::vector<std::string> dictionary,
         const unsigned int min_term_freq,
         const unsigned int max_term_freq,
         const unsigned int min_word_length,
         const unsigned int max_word_length) {
    unsigned int column = 1;
    std::map<std::string, unsigned int> line, terms_pos;
    std::set<std::string> dict(dictionary.begin(), dictionary.end()),
        sw(stopwords.begin(), stopwords.end());
    std::vector<unsigned int> i, j, v;
    std::vector<std::string> terms;

    for (unsigned int index = 0; index < strings.size(); index++) {
        std::string s = std::string(strings(index));
        boost::tokenizer<> tok(s);

        line.clear();
        for (boost::tokenizer<>::iterator it = tok.begin();
             it != tok.end();
             ++it) {
            std::string token = *it;
            if (remove_digits)
                token.erase(
                        std::remove_if(token.begin(), token.end(), &isdigit),
                        token.end());
            if ((dict.empty() || dict.count(token)) &&
                min_word_length <= token.length() &&
                token.length() <= max_word_length &&
                !sw.count(token))
                line[token]++;
        }

        for (std::map<std::string, unsigned int>::iterator it = line.begin();
             it != line.end();
             ++it) {
            std::string term = it->first;
            unsigned int freq = it->second;

            if (min_term_freq <= freq && freq <= max_term_freq) {
                if (!terms_pos.count(term)) {
                    terms_pos[term] = column++;
                    terms.push_back(term);
                }
                i.push_back(terms_pos[term]);
                j.push_back(index + 1);
                v.push_back(freq);
            }
        }
    }

    for (const std::string &term : dictionary)
        if (std::find(terms.begin(), terms.end(), term) == terms.end())
            terms.push_back(term);

    return List::create(Named("i") = i,
                        Named("j") = j,
                        Named("v") = v,
                        Named("terms") = terms);
}
