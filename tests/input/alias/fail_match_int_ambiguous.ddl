test =
    match 23 : Int { 23 => true, _ => false }; //~ error: ambiguous match expression
