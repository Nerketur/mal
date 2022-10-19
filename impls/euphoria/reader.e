include std/regex.e as re
include std/sequence.e
include std/pretty.e
include std/convert.e

constant TRUE = 1, FALSE = 0

constant TOKENS = 1, POS = 2

public enum type MAL_TYPE
    M_SYMBOL,
    M_INTEGER,
    M_LIST, -- types: T_LIST, T_VECTOR, T_HASH
    M_NIL,
    M_TRUE,
    M_FALSE,
    M_STRING,
    M_KEYWORD,
    M_COMMENT,
    ------
    M_QUOTE,
    M_QUASIQUOTE,
    M_UNQUOTE,
    M_SPLICE_UNQUOTE,
    M_DEREF,
    M_UNKNOWN = 999
end type

public enum type LIST_TYPE
    T_LIST,
    T_HASH,
    T_VECTOR
end type
constant TOKENS_REGEX = re:new("[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)")

type reader(object o)
    -- If the target language has objects types (OOP), then the next step is to create a simple
    -- stateful Reader object in reader.qx. This object will store the tokens and a position.
    -- The Reader object will have two methods: next and peek. next returns the token at the current
    -- position and increments the position. peek just returns the token at the current position.
    if atom(o) or length(o) != 2 or atom(o[TOKENS]) or (not integer(o[POS])) then
        return FALSE
    end if
    return TRUE

end type

function make_reader(sequence tokens)
    return {tokens, 1}
end function

-- all of these functions return the new reader as well as the result
function reader_next(reader r)
    if r[POS] > length(r[TOKENS]) then
        return {r, {4}}
    end if
    object currentToken = r[TOKENS][r[POS]]
    r[POS] += 1
    return {r, currentToken}
end function

function reader_peek(reader r)
    if r[POS] > length(r[TOKENS]) then
        return {r, {4}}
    end if
    object currentToken = r[TOKENS][r[POS]]
    return {r, currentToken}
end function

function tokenize(sequence str)
    -- return fetch(columnize(re:all_matches(TOKENS_REGEX, str), 2), {1})
    return vslice(re:all_matches(TOKENS_REGEX, str), 2)
end function

public function read_str(sequence str)
    sequence parsed = {}
    reader r = make_reader(tokenize(str))
    {r, parsed} = read_form(r)
    return parsed
end function

function read_form(reader r)
    sequence parsed, tok
    -- Add the function read_form to reader.qx.  This function will peek at the first token in the Reader
    -- object and switch on the first character of that token.  If the character is a left paren then
    -- read_list is called with the Reader object.  Otherwise, read_atom is called with the Reader Object.
    -- The return value from read_form is a mal data type.  If your target language is statically typed then
    -- you will need some way for read_form to return a variant or subclass type.  For example, if your
    -- language is object oriented, then you can define a top level MalType (in types.qx) that all your mal
    -- data types inherit from.  The MalList type (which also inherits from MalType) will contain a
    -- list/array of other MalTypes.  If your language is dynamically typed then you can likely just return
    -- a plain list/array of other mal types.
    {r, tok} = reader_peek(r)
    switch tok[1] do
        case '(', '{', '[' then
            {r, parsed} = read_list(r)
        case else
            {r, parsed} = read_atom(r)
    end switch
    return {r, parsed}
end function

public constant LIST_TOKENS = {"({[", ")}]", {T_LIST, T_HASH, T_VECTOR}},
         L_OPEN = 1,
         L_CLOSE = 2,
         L_TYPE = 3


function read_list(reader r, LIST_TYPE listType = T_LIST)
    -- Add the function read_list to reader.qx.  This function will repeatedly call read_form with the
    -- Reader object until it encounters a ')' token (if it reach EOF before reading a ')' then that is an
    -- error).  It accumulates the results into a List type.  If your language does not have a sequential
    -- data type that can hold mal type values you may need to implement one (in types.qx).  Note that
    -- read_list repeatedly calls read_form rather than read_atom.  This mutually recursive definition
    -- between read_list and read_form is what allows lists to contain lists.
    sequence parsed = {}, res
    {r, res} = read_atom(r) -- will be one of "({["
    integer l_idx = find(res[2][1], LIST_TOKENS[L_OPEN])
    sequence l_close = {LIST_TOKENS[L_CLOSE][l_idx]}
    -- parsed = append(parsed, res)
    while TRUE do
        {r, res} = read_form(r)
        if equal(res[2], l_close) then
            exit -- exit while loop
        elsif equal(res[2], {4}) then
            puts(1, "Error, unbalanced parentheses\n")
            return {r, {M_UNKNOWN, {}}}
        end if
        parsed = append(parsed, res)
    end while
    return {r, {M_LIST, parsed, LIST_TOKENS[L_TYPE][l_idx]}}
end function

function balancedQuotes(sequence str)
    if length(str) > 1 and str[1] = '"' and str[$] = '"' then
        --here we check the entire string
        return checkBalance(str[2..$-1])
    end if
    return FALSE
end function

function checkBalance(sequence str)
    
    if length(str) = 0 then
        return TRUE
    elsif str[1] != '\\' then
        return checkBalance(str[2..$])
    elsif length(str) > 1 and find(str[2], "n\"\\") then
        return checkBalance(str[3..$])
    end if
    return FALSE
end function

function read_splice_unquote(reader r)
    sequence parsed
    {r, parsed} = read_form(r)
    return {r, {M_SPLICE_UNQUOTE, parsed}}
end function

function read_atom(reader r)
    -- Add the function read_atom to reader.qx.  This function will look at the contents of the token and
    -- return the appropriate scalar (simple/single) data type value.  Initially, you can just implement
    -- numbers (integers) and symbols.  This will allow you to proceed through the next couple of steps
    -- before you will need to implement the other fundamental mal types: nil, true, false, and string.
    -- The remaining scalar mal type, keyword does not need to be implemented until step A (but can be
    -- implemented at any point between this step and that). BTW, symbols types are just an object that
    -- contains a single string name value (some languages have symbol types already).
    sequence mtype
    sequence tok
    {r, tok} = reader_next(r)
    -- switch tok[1] do
    --     case '(', ')' then
    --         mtype = {M_SYMBOL, tok}
    --     case '0','1','2','3','4','5','6','7','8','9' then
    --         mtype = {M_INTEGER, tok}
    --     case else
    --         mtype = {M_UNKNOWN, tok}
    -- end switch
    if find(tok, {"[", "]", "{", "}", "(", ")", "^"} ) then
        mtype = {M_SYMBOL, tok}
    elsif equal(tok, "~@") then
        -- change from ~@x
        -- change to (splice-unquote x)
        -- here we just parse the next token as a splice-unquote
        {r, mtype} = read_form(r)
        mtype = {M_SPLICE_UNQUOTE, mtype}
    elsif (tok[1] = '\'') then
        {r, mtype} = read_form(r)
        mtype = {M_QUOTE, mtype}
    elsif (tok[1] = '`') then
        {r, mtype} = read_form(r)
        mtype = {M_QUASIQUOTE, mtype}
    elsif (tok[1] = '~') then
        {r, mtype} = read_form(r)
        mtype = {M_UNQUOTE, mtype}
    elsif (tok[1] = '@') then
        {r, mtype} = read_form(r)
        mtype = {M_DEREF, mtype}
    elsif (tok[1] = '"') then
        -- valid gotchas:
        -- "\"" -- valid
        -- "\\"" -- invalid, parses as "\""
        if balancedQuotes(tok) then
            mtype = {M_STRING, transmute(tok[2..$-1], {{}, "\\\"", "\\n", "\\\\"}, "\"\n\\")}
        else
            puts(1, "Error, unbalanced quotes\n")
            mtype = {M_UNKNOWN, tok}
        end if
    elsif (tok[1] = ';') then
        mtype = {M_COMMENT, tok}
    elsif (tok[1] >= '0' and tok[1] <= '9') or (length(tok) > 1 and tok[1] = '-' and tok[2] >= '0' and tok[2] <= '9' ) then
        mtype = {M_INTEGER, to_integer(tok)}
    elsif (equal(tok, "nil")) then
        mtype = {M_NIL, tok}
    elsif (equal(tok, "true")) then
        mtype = {M_TRUE, tok}
    elsif (equal(tok, "false")) then
        mtype = {M_FALSE, tok}
    else
        mtype = {M_SYMBOL, tok}
    end if
    return {r, mtype}
end function


-- "(?:\\.|[^\\"])*"?
-- Starts capturing at a double-quote and stops at the next double-quote unless it was preceded by a
-- backslash in which case it includes it until the next double-quote (tokenized). It will also match
-- unbalanced strings (no ending double-quote) which should be reported as an error.

-- ;.*
-- Captures any sequence of characters starting with ; (tokenized).

-- [^\s\[\]{}('"`,;)]*
-- Captures a sequence of zero or more non special characters (e.g. symbols, numbers, "true", "false", and
-- "nil") and is sort of the inverse of the one above that captures special characters (tokenized).


-- ? reader_peek({"abcd", 1})
-- ? reader_next({"abcd", 1})

-- pretty_print(1, tokenize("( test () )"), {2})
-- pretty_print(1, tokenize("( 1 20 () )"), {2})
-- pretty_print(1, read_atom(make_reader(tokenize(" 20 () )"))), {2})
-- pretty_print(1, read_str("( (3) )"), {2})