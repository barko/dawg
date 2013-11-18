%{

type dense = string list
type sparse = (int * string) list
type row = [ `Dense of dense | `EOF | `Sparse of sparse ]

%}

%token <string> STRING
%token <int> NEG_INT
%token <int> POS_INT
%token <float> FLOAT

%token LCURLY
%token RCURLY
%token COMMA
%token EOL
%token EOF
%token <string> COMMENT

%start header
%start row

%type <string list> header
%type <Csv_types.row> row

%%

header:
| strings EOL { $1 }
| strings COMMENT EOL { $1 }
| newlines strings EOL { $2 }
| newlines strings COMMENT EOL { $2 }

strings:
| STRING COMMA strings { $1 :: $3 }
| STRING { [ $1 ] }

value:
| NEG_INT { (`Int $1) }
| POS_INT { (`Int $1) }
| FLOAT { (`Float $1) }
| STRING { (`String $1) }

values:
| value COMMA values { $1 :: $3 }
| value { [ $1 ] }

row:
| row_sans_nl EOL { $1 }
| row_sans_nl COMMENT EOL { $1 }
| newlines row_sans_nl EOL { $2 }
| newlines row_sans_nl COMMENT EOL { $2 }
| newlines EOF { `EOF }
| EOF { `EOF }

row_sans_nl:
| dense_row { `Dense $1 }
| sparse_row { `Sparse $1 }

dense_row:
| values { $1 }

sparse_row:
| LCURLY pairs RCURLY { $2 }
| LCURLY RCURLY { [] }

pairs:
| pair COMMA pairs { $1 :: $3 }
| pair { [ $1 ] }

pair:
| POS_INT value { $1, $2 }

newlines:
| EOL { () }
| EOL newlines { () }
| COMMENT newlines { () }
