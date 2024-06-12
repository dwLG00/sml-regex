(* Most code "taken" from https://sebfisch.github.io/haskell-regexp/regexp-play.pdf *)

(* Datatypes *)
datatype Regex = EMPTY | CHAR of (char * bool) | OR of (Regex * Regex) | AND of (Regex * Regex) | STAR of Regex

(* Aux functions for use in shift(3) *)
fun empty EMPTY = true
  | empty (CHAR _) = false
  | empty (OR (p, q)) = empty p orelse empty q
  | empty (AND (p, q)) = empty p andalso empty q
  | empty (STAR r) = true

fun final EMPTY = false
  | final (CHAR (_, b)) = b
  | final (OR (p, q)) = final p orelse final q
  | final (AND (p, q)) = (final p andalso empty q) orelse final q
  | final (STAR r) = final r

(* shift(3) - shifts markers of regex according to found character *)
val rec shift : (bool * Regex * char) -> Regex =
    fn (_, EMPTY, _) => EMPTY
      | (m, CHAR (x, _), c) => CHAR (x, m andalso (x = c))
      | (m, OR (p, q), c) => OR (shift (m, p, c), shift (m, q, c))
      | (m, AND (p, q), c) => AND (shift (m, p, c), shift (((m andalso empty p) orelse final p), q, c))
      | (m, STAR r, c) => STAR (shift (m orelse final r, r, c))

(* shift_wrap(2) - shifts by an entire list char *)
fun shift_wrap r [] = r
  | shift_wrap r (c::cs) = shift_wrap (shift (false, r, c)) cs

(* re_match(2) - returns whether given string matches regex pattern *)
fun re_match r [] = empty r
  | re_match r (c::[]) = final (shift (true, r, c))
  | re_match r (c::cs) = final (shift_wrap (shift (true, r, c)) cs)

(* matching aux functions *)
fun re_find_aux r [] i = NONE
  | re_find_aux r (c::cs) i = if re_match r (c::cs) then (SOME i) else (re_find_aux r cs (i+1))

(* re_match_with_end(2) - returns the last index of regex pattern match *)
fun re_match_end_aux r [] i start = if empty r then (SOME i) else NONE
  | re_match_end_aux r (c::cs) i start = if start 
    then
        (if final (shift (true, r, c)) then (SOME i) else re_match_end_aux r cs (i+1) false)
    else
        (if final (shift (false, r, c)) then (SOME i) else re_match_end_aux r cs (i+1) false)

(* re_find(2) - returns first match, or -1 if doesn't exist *)
fun re_find r cs =
    let
        fun re_find_aux re [] i = NONE
          | re_find_aux re (d::ds) i = case (re_match_end_aux r (d::ds) i true) of
                SOME j => SOME i
              | NONE => re_find_aux re ds (i+1)
    in
        re_find_aux r cs 0
    end

(* re_find_all(2) - returns a list of all matches *)
fun re_find_all r cs =
    let
        fun forwards_n n [] = []
          | forwards_n 0 cs = cs
          | forwards_n n (c::cs) = forwards_n (n-1) cs
        fun re_find_all_aux re [] i matches = matches
          | re_find_all_aux re (d::ds) i matches = case (re_match_end_aux r (d::ds) i true) of
                SOME j => re_find_all_aux re (forwards_n (j-i) ds) (j+1) ((i, j)::matches)
              | NONE => re_find_all_aux re ds (i+1) matches 
    in
        re_find_all_aux r cs 0 []
    end

(* parsing helper functions - list of regex to OR-chains and AND-chains *)
fun reduce_or ([]) = EMPTY
  | reduce_or (r::[]) = r
  | reduce_or (r::rest) = OR (reduce_or rest, r)

fun reduce_and ([]) = EMPTY
  | reduce_and (r::[]) = r
  | reduce_and (r::rest) = AND (reduce_and rest, r)

(* parse(1) - parses a list char into a regex tree *)
fun parse (char_list) =
    let fun parse_helper (regex, [], cps, ps, [cands]) = reduce_and (regex::cands)
      | parse_helper (regex, #"*"::rest, cps, ps, ands) = parse_helper (STAR regex, rest, cps, ps, ands)
      | parse_helper (EMPTY, #"("::rest, cps, ps, ands) = parse_helper (EMPTY, rest, [], cps::ps, []::ands)
      | parse_helper (regex, #"("::rest, cps, ps, cands::ands) = parse_helper (EMPTY, rest, [], cps::ps, []::(regex::cands)::ands)
      | parse_helper (regex, #"|"::rest, cps, ps, ands) = parse_helper (EMPTY, rest, regex::cps, ps, ands)
      | parse_helper (regex, #")"::rest, cps, prev::ps, cands::ands) = (case cands of 
            [] => parse_helper (reduce_or (regex::cps), rest, prev, ps, ands)
          | cand_stack => parse_helper (reduce_and ((reduce_or (regex::cps))::cand_stack), rest, prev, ps, ands)
        )
      | parse_helper (EMPTY, c::rest, cps, ps, ands) = parse_helper (CHAR (c, false), rest, cps, ps, ands)
      | parse_helper (regex, c::rest, cps, ps, cands::ands) = parse_helper (CHAR (c, false), rest, cps, ps, (regex::cands)::ands)
      | parse_helper (_, _, _, _, _) = EMPTY
    in
        parse_helper (EMPTY, char_list, [], [], [[]])
    end

(* display(1) - returns string representation of regex tree *)
fun display EMPTY = "[empty]"
  | display (CHAR (x, _)) = Char.toString x
  | display (AND (p, q)) = display p ^ display q
  | display (OR (p, q)) = "(" ^ display p ^ "|" ^ display q ^ ")"
  | display (STAR r) = (case r of 
        AND _ => "(" ^ display r ^ ")*"
      | _ => display r ^ "*"
    )

(* parse_display_string(1) - wraps display *)
fun parse_display_string str = display (parse (String.explode str))

(*
val test_string_false = "aaccbbc"
val test_string_true = "accbcaacba"
val regex_string = "((a|b)*c(a|b)*c)*(a|b)*"
val regex = parse (String.explode regex_string)

fun main () =
        print "";
        print ((display regex) ^ "\n");
        print ("Matching " ^ test_string_true ^ "...\n");
        (if (re_match regex (String.explode test_string_true)) then
            print "Matched! (Correct)\n"
        else
            print "Didn't match! (Incorrect)\n");
        print ("Matching " ^ test_string_false ^ "...\n");
        (if (re_match regex (String.explode test_string_false)) then
            print "Matched! (Incorrect)\n"
        else
            print "Didn't match! (Correct)\n");
*)

fun list_len li =
    let
        fun list_len_aux [] i = i
          | list_len_aux (l::li) i = list_len_aux li (i+1)
    in
        list_len_aux li 0
    end

val regex_string = "(a|b)*c(a|b)*" (* matches all strings with a single c and (possibly) multiple a/bs on either side *)
val regex = parse (String.explode regex_string)

fun display_loop () =
    case TextIO.inputLine TextIO.stdIn of
        SOME s => (print ("Found " ^ (Int.toString (list_len (re_find_all regex (String.explode s)))) ^ " matches\n"); display_loop ())
      | NONE => print "Exiting..\n"

fun main () =
    (print ("Using regex expression " ^ (display regex) ^ "\n");
    display_loop ())

val _ = main ()
