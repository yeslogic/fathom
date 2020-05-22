Test1 = U32Be : U8; //~ error: type mismatch
Test2 = U32Be : (23 : Int); //~ error: universe mismatch
Test3 = U32Be : Type; //~ error: type mismatch
Test4 = Int : Format; //~ error: type mismatch
