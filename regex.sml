
datatype Regex = EMPTY | CHAR of (char * bool) | OR of (Regex * Regex) | AND of (Regex * Regex) | STAR of Regex

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

val rec shift : (bool * Regex * char) -> Regex =
    fn (_, EMPTY, _) => EMPTY
      | (m, CHAR (x, _), c) => CHAR (x, m andalso (x = c))
      | (m, OR (p, q), c) => OR (shift (m, p, c), shift (m, q, c))
      | (m, AND (p, q), c) => AND (shift (m, p, c), shift (((m andalso empty p) orelse final p), q, c))
      | (m, STAR r, c) => STAR (shift (m orelse final r, r, c))

fun reduce_or ([]) = EMPTY
  | reduce_or (r::[]) = r
  | reduce_or (r::rest) = OR (reduce_or rest, r)

fun reduce_and ([]) = EMPTY
  | reduce_and (r::[]) = r
  | reduce_and (r::rest) = AND (reduce_and rest, r)

fun parse (regex, char_list) =
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
        parse_helper (regex, char_list, [], [], [[]])
    end

fun display EMPTY = "[empty]"
  | display (CHAR (x, _)) = Char.toString x
  | display (AND (p, q)) = display p ^ display q
  | display (OR (p, q)) = "(" ^ display p ^ "|" ^ display q ^ ")"
  | display (STAR r) = (case r of 
        AND _ => "(" ^ display r ^ ")*"
      | _ => display r ^ "*"
    )

fun parse_display_string str = display (parse (EMPTY, String.explode str))

fun main () =
    case TextIO.inputLine TextIO.stdIn of
        SOME s => print (parse_display_string s)
      | NONE => print "no input detected, exiting..."


val _ = main ()
