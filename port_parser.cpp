
#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/fusion/include/io.hpp>

#include <vector>
#include <iostream>
#include <string>
#include <complex>

namespace client
{
    namespace qi = boost::spirit::qi;
    namespace ascii = boost::spirit::ascii;

    ///////////////////////////////////////////////////////////////////////////
    //  Our port_sig struct
    ///////////////////////////////////////////////////////////////////////////
    //[tutorial_port_sig
    struct port_sig
    {
        std::string sig_name;
        std::string dir;
    };
    //]
}

// We need to tell fusion about our port_sig struct
// to make it a first-class fusion citizen. This has to
// be in global scope.


BOOST_FUSION_ADAPT_STRUCT(
  client::port_sig,
  (std::string, sig_name)
  (std::string, dir) 
  //(VHDL_Parser::direction, dir)
)


namespace client
{
    ///////////////////////////////////////////////////////////////////////////////
    //  Our port_sig parser
    ///////////////////////////////////////////////////////////////////////////////
    //[tutorial_port_sig
    template <typename Iterator>
    struct port_parser : qi::grammar<Iterator, std::vector<port_sig>(), ascii::space_type>
    {
        port_parser() : port_parser::base_type(start)
        {
            using qi::int_;
            using qi::lit;
            using qi::double_;
            using qi::lexeme;
            using ascii::char_;
            using ascii::no_case;

            quoted_string %= lexeme['"' >> +(char_ - '"') >> '"'];
            
            start %=  lit("port") 
                      >> '('
                      >> p_sig >> *(lit(';') >> p_sig)
                      >> ')' >> ';';

            p_sig %= ident >> ':' >>  direction_spec ;

           ident %= ((char_("a-zA-Z_") >> *char_("a-zA-Z_0-9")) - keyword );
           direction_spec %= (in_kwd | out_kwd | inout_kwd );
/* //more keywords for later
            dot            = char_('.');
            colon          = char_(':');
            semicolon      = char_(';');
            comma          = char_(',');
            single_quote   = char_('\'');
            double_quote   = char_('"');
            lparen         = char_('(');
            comment_delim  = lexeme["--"];
            sig_assign     = lexeme["<="];
            eq_gt          = lexeme["=>"];
            var_assign     = lexeme[":="];
            x_char         = no_case[char_('x')];
            entity_kwd     = no_case[ lexeme["entity"] ];
            architecture_kwd     = no_case[ lexeme["architecture"] ];
            package_kwd    = no_case[ lexeme["package"] ];
            port_kwd       = no_case[ lexeme["port"] ];
            map_kwd        = no_case[ lexeme["map"] ];
            body_kwd       = no_case[ lexeme["body"] ];
            generic_kwd    = no_case[ lexeme["generic"] ];
            signal_kwd     = no_case[ lexeme["signal"] ];
            is_kwd         = no_case[ lexeme["is"] ];
            to_kwd         = no_case[ lexeme["to"] ];
            if_kwd         = no_case[ lexeme["if"] ];
            of_kwd         = no_case[ lexeme["of"] ];
*/
            in_kwd         = no_case[ lexeme["in"] ];
            out_kwd        = no_case[ lexeme["out"] ];
            inout_kwd      = no_case[ lexeme["inout"]];
/*
            for_kwd        = no_case[ lexeme["for"] ];
            downto_kwd     = no_case[ lexeme["downto"] ];
            case_kwd       = no_case[ lexeme["case"] ];
            loop_kwd       = no_case[ lexeme["loop"] ];
            component_kwd  = no_case[ lexeme["component"] ];
            configuration_kwd  = no_case[ lexeme["configuration"] ];
            library_kwd    = no_case[ lexeme["library"] ];
            process_kwd    = no_case[ lexeme["process"] ];
            use_kwd        = no_case[ lexeme["use"] ];
            begin_kwd      = no_case[ lexeme["begin"]] ;
            end_kwd        = no_case[ lexeme["end"] ];
            type_kwd       = no_case[ lexeme["type"]];
            when_kwd       = no_case[ lexeme["when"]];
            else_kwd       = no_case[ lexeme["else"]];
            not_kwd        = no_case[ lexeme["not"]];
            and_kwd        = no_case[ lexeme["and"]];
            attribute_kwd  = no_case[ lexeme["attribute"]];
            // are pragams are case-sensitive?
            pragma         = no_case[ lexeme["pragma"]];
            synopsys       = no_case[ lexeme["synopsys"]];
            synthesis      = no_case[ lexeme["synthesis"]];
            translate_off  = no_case[ lexeme["translate_off"]];
            translate_on   = no_case[ lexeme["translate_on"]];
*/

/*            
      keyword = ( 
                 entity_kwd | architecture_kwd | package_kwd |
                 port_kwd | map_kwd | body_kwd | generic_kwd |
                 signal_kwd | is_kwd | to_kwd  | if_kwd |
                 of_kwd | in_kwd | out_kwd | inout_kwd | for_kwd |
                 downto_kwd | case_kwd | loop_kwd | component_kwd |
                 library_kwd | process_kwd | use_kwd | begin_kwd | 
                 end_kwd | type_kwd | when_kwd | else_kwd | not_kwd |
                 and_kwd | attribute_kwd | configuration_kwd
                 );
*/
      keyword = ( in_kwd | out_kwd | inout_kwd);

        }

        qi::rule<Iterator, std::string(), ascii::space_type> direction_spec;
        qi::rule<Iterator, std::string(), ascii::space_type> ident;
        qi::rule<Iterator, std::string(), ascii::space_type> quoted_string;
        qi::rule<Iterator, port_sig(), ascii::space_type> p_sig;
        qi::rule<Iterator, std::vector<port_sig>(), ascii::space_type> start;
        qi::rule<Iterator, std::string(), ascii::space_type> in_kwd;
        qi::rule<Iterator, std::string(), ascii::space_type> out_kwd;
        qi::rule<Iterator, std::string(), ascii::space_type> inout_kwd;
        qi::rule<Iterator, std::string(), ascii::space_type> keyword;
    };
    //]
}

////////////////////////////////////////////////////////////////////////////
//  Main program
////////////////////////////////////////////////////////////////////////////
int
main()
{
    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "\t\tA port_sig parser for Spirit...\n\n";
    std::cout << "/////////////////////////////////////////////////////////\n\n";

    std::cout
        << "Give me a port of the form :"
        << "port(sig_name: direction ) \n";
    std::cout << "Type [q or Q] to quit\n\n";

    using boost::spirit::ascii::space;
    typedef std::string::const_iterator iterator_type;
    typedef client::port_parser<iterator_type> port_parser;

    port_parser g; // Our grammar
    std::string str;
    while (getline(std::cin, str))
    {
        if (str.empty() || str[0] == 'q' || str[0] == 'Q')
            break;

        std::vector<client::port_sig> emps;
        std::string::const_iterator iter = str.begin();
        std::string::const_iterator end = str.end();
        bool r = phrase_parse(iter, end, g, space, emps);

        if (r && iter == end)
        {
            std::cout << boost::fusion::tuple_open('[');
            std::cout << boost::fusion::tuple_close(']');
            std::cout << boost::fusion::tuple_delimiter(", ");

            std::cout << "-------------------------\n";
            std::cout << "Parsing succeeded\n";
            //std::cout << "got: " << boost::fusion::as_vector(emp) << std::endl;
            std::cout << "\n-------------------------\n";
        }
        else
        {
            std::cout << "-------------------------\n";
            std::cout << "Parsing failed\n";
            std::cout << "-------------------------\n";
        }
    }

    std::cout << "Bye... :-) \n\n";
    return 0;
}
