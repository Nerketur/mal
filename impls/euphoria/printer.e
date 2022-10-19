public include reader.e
include std/sequence.e
-- Add a file printer.qx. This file will contain a single function pr_str which does the opposite of
-- read_str: take a mal data structure and return a string representation of it. But pr_str is much
-- simpler and is basically just a switch statement on the type of the input object:

-- symbol: return the string name of the symbol
-- number: return the number as a string
-- list: iterate through each element of the list calling pr_str on it, then join the results with a
-- space separator, and surround the final result with parens

constant TRUE = 1, FALSE = 0

public function pr_str(sequence mal_type, integer print_readably = TRUE)
    switch mal_type[1] do
        case M_SYMBOL, M_TRUE, M_FALSE, M_NIL then
            return mal_type[2]
        case M_INTEGER then
            return sprintf("%d", {mal_type[2]})
        case M_LIST then
            integer l_idx = find(mal_type[3], LIST_TOKENS[L_TYPE])
            sequence l_open = {LIST_TOKENS[L_OPEN][l_idx]}
            sequence l_close = {LIST_TOKENS[L_CLOSE][l_idx]}
            return l_open & join(apply(mal_type[2], routine_id("print_mal_ele")), " ") & l_close
        case M_STRING then
            if print_readably then
                return "\"" & transmute(mal_type[2], "\"\n\\", {{}, "\\\"", "\\n", "\\\\"}) & "\""
            end if
            return mal_type[2]
        case M_QUOTE then
            return "(quote " & pr_str(mal_type[2]) & ")"
        case M_QUASIQUOTE then
            return "(quasiquote " & pr_str(mal_type[2]) & ")"
        case M_UNQUOTE then
            return "(unquote " & pr_str(mal_type[2]) & ")"
        case M_SPLICE_UNQUOTE then
            return "(splice-unquote " & pr_str(mal_type[2]) & ")"
        case M_DEREF then
            return "(deref " & pr_str(mal_type[2]) & ")"
    end switch
    return "<possible parse error>"
end function

function print_mal_ele(object ele, object d)
    return pr_str(ele)
end function

-- puts(1, pr_str({3, {{2, "100"}, {2, "100"}}}))